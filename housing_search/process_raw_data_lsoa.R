


# Code to wrangle and combine all data used in app including:

# Land registry price paid
# lat/long coordinates
# lsoa
# indices of multiple deprivation
# road noise
# travel time to Big Ben
# flood risk
#
# takes about 30 mins to run due to the large Road Noise shapefile

# Adam Bricknell, August 2021

library(tidyverse)
library(data.table)
library(lubridate)
library(sf)
library(rgeos)


setwd("~/Desktop/shiny_apps/housing_search/housing_search")

# find LSOAs in each postcode
postcode_lsoa_lookup <-
  fread("raw_data/PCD_OA_LSOA_MSOA_LAD_AUG21_UK_LU.csv",
        data.table = F)

postcode_lsoa_lookup <- postcode_lsoa_lookup %>%
  select(pcds, lsoa11cd) 



land_registry_price_paid <-
  fread(
    "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2021.csv"
  )

price_paid_lsoa <- land_registry_price_paid %>%
  rename(price = V2, pcds = V4) %>%
  select(price, pcds) %>%        
  merge(postcode_lsoa_lookup, by = 'pcds') %>%   # lose 1000 out of 350k in merge
  group_by(lsoa11cd) %>%
  summarise(price = mean(price))










# join IMD (crime, ediucation, geographic barriers) and get avg score for each simple postcode
domains_deprivation_lsoa <-
  openxlsx::read.xlsx("raw_data/File_2_-_IoD2019_Domains_of_Deprivation.xlsx",
                      sheet = "IoD2019 Domains") %>%
  rename(
    lsoa11cd = `LSOA.code.(2011)`,
    crime_deprivation = `Crime.Decile.(where.1.is.most.deprived.10%.of.LSOAs)`,
    health_deprivation = `Health.Deprivation.and.Disability.Decile.(where.1.is.most.deprived.10%.of.LSOAs)`,
    housing_services_deprivation = `Barriers.to.Housing.and.Services.Decile.(where.1.is.most.deprived.10%.of.LSOAs)`
  ) %>%
  select(lsoa11cd,
         crime_deprivation,
         health_deprivation,
         housing_services_deprivation)


subdomains_deprivation_lsoa <-
  openxlsx::read.xlsx("raw_data/File_4_-_IoD2019_Sub-domains_of_Deprivation.xlsx",
                      sheet = "IoD2019 Sub-domains") %>%
  rename(
    lsoa11cd = `LSOA.code.(2011)`,
    education_deprivation = `Education,.Skills.and.Training.Decile.(where.1.is.most.deprived.10%.of.LSOAs)`,
    living_environment_deprivation = `Living.Environment.Decile.(where.1.is.most.deprived.10%.of.LSOAs)`,
    outdoors_deprivation = `Outdoors.Sub-domain.Decile.(where.1.is.most.deprived.10%.of.LSOAs)`,
    geographic_barriers_deprivation = `Geographical.Barriers.Sub-domain.Decile.(where.1.is.most.deprived.10%.of.LSOAs)`
  ) %>%
  select(
    lsoa11cd,
    education_deprivation,
    living_environment_deprivation,
    outdoors_deprivation,
    geographic_barriers_deprivation
  )

all_deprivation_lsoa <- domains_deprivation_lsoa %>%
  merge(subdomains_deprivation_lsoa, by = "lsoa11cd")




# get lat/long centroids from Lsoa shapefile
lsoa_shp <-
  st_read(
    "raw_data/lsoa_shapefile/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp"
  )

lsoa_centroids <- st_centroid(lsoa_shp) %>%
  pull(geometry) %>%
  unlist() %>%
  matrix(ncol = 2, byrow = T) %>%
  as.data.frame() %>%
  setNames(., c('Long', 'Lat')) %>%
  mutate(lsoa11cd = lsoa_shp$lsoa11cd) 







# flood risk https://environment.data.gov.uk/apiportal
flood_areas <-
  jsonlite::read_json('https://environment.data.gov.uk/flood-monitoring/id/floodAreas')

flood_areas_df <- purrr::map(flood_areas$items, as.data.frame) %>%
  rbindlist(fill = T) %>%
  select(lat, long)   # 'polygon' col gives URLs to get polygons only


counter <-
  vector(mode = 'numeric',
         length = nrow(lsoa_centroids))
for (i in seq_along(1:length(counter))) {
  # within 0.045 (5km) of how many flood areas?
  
  counter[i] <-
    sum(sqrt((flood_areas_df$lat - lsoa_centroids$Lat[i]) ^ 2 +
               (flood_areas_df$long - lsoa_centroids$Long[i]) ^ 2
    ) < 0.045)
  
  if (i %% 100 == 0) { print(i)}
  
}

lsoa_flood_areas <-
  data.frame(lsoa11cd = lsoa_centroids$lsoa11cd,
             flood_areas_count = counter)









# driving time from Big Ben with OSRM – with lat/long for each centroid of single digit postcode
duration <- rep(0, nrow(lsoa_centroids))
distance <- rep(0, nrow(lsoa_centroids))

for (i in 1:nrow(lsoa_centroids)) {
  query_string <- paste0(
    '0.0.0.0:5000/route/v1/driving/',
    '0.124600,51.500700;',
    sprintf('%.6f', lsoa_centroids$Long[i]),  # string 6 DPs, no sci notation
    ',',
    sprintf('%.6f', lsoa_centroids$Lat[i])
  )
  
  curl::curl_download(query_string, 'raw_data/big_ben_travel.json')
  routing <- jsonlite::read_json('raw_data/big_ben_travel.json')
  
  duration[i] <- routing$routes[[1]]$duration
  distance[i] <- routing$routes[[1]]$distance
  
  if (i %% 100 == 0) { print(i)}
}

lsoa_big_ben_travel_time <- data.frame(
  lsoa11cd = lsoa_centroids$lsoa11cd,
  commute_hours = duration / 3600,
  commute_distance_km = distance / 1000
)

# storing in case of crash as takes 30 mins to run
fwrite(lsoa_big_ben_travel_time, 'data/lsoa_big_ben_travel_time.csv')








# Aggregating housing stats by LSOA, using the most recent EPCs (from 1st Jan 2019s)
get_most_common_value = function(x) {
  idx <- !x %in% c('NO DATA!', '')   # removing missing data
  if (sum(idx) < 0.5) {return('')}   # dealing with it when all data missing
  sort(table(x[idx]), decreasing=T)[1]%>%names()
  }

all_areas <- paste0('/Users/apple/Downloads/all-domestic-certificates/',
                    list.files('/Users/apple/Downloads/all-domestic-certificates', include.dirs = T), 
                    '/certificates.csv')


epc_list <- vector(mode = 'list', length = length(all_areas))
for (i in 1:length(epc_list)) {
  epc_list[[i]] <- fread(all_areas[i], data.table = F)  %>%
    mutate(LODGEMENT_DATE = ymd(LODGEMENT_DATE)) %>%
    filter(LODGEMENT_DATE >= ymd('2019-01-01')) %>%
    merge(postcode_lsoa_lookup, by.x = "POSTCODE", by.y = "pcds") %>% # about 1% lost in the merge for test LA
    group_by(lsoa11cd) %>%
    summarise(
      CONSTRUCTION_AGE_BAND = get_most_common_value(CONSTRUCTION_AGE_BAND),
      NUMBER_HABITABLE_ROOMS = mean(NUMBER_HABITABLE_ROOMS, na.rm = T),
      CURRENT_ENERGY_EFFICIENCY = mean(CURRENT_ENERGY_EFFICIENCY, na.rm = T),
      PROPERTY_TYPE = get_most_common_value(PROPERTY_TYPE),
      BUILT_FORM = get_most_common_value(BUILT_FORM),
      CURRENT_ENERGY_RATING = get_most_common_value(CURRENT_ENERGY_RATING),
      MAINS_GAS_FLAG = get_most_common_value(MAINS_GAS_FLAG),
      epc_sample_size = n()
    )
  print(i)
}

epc_data_lsoa <- rbindlist(epc_list)

epc_data_lsoa <- epc_data_lsoa %>%  # drops 2,000 out of 3.5m
  arrange(desc(epc_sample_size)) %>%  # where there is >1 row for an lsoa, keep the one with biggest sample
  distinct(lsoa11cd, .keep_all = T)

fwrite(epc_data_lsoa, 'data/epc_data_lsoa.csv')   # all lsoas should be unique but need to check










# road noise https://data.gov.uk/dataset/9f1d7309-dc00-4b13-8ba6-f3096aba4c13/road-noise-laeq-16h-england-round-2
road_noise_shp <-
  st_read("raw_data/lsoa_road_noise_shapefile/Road_Noise_LAeq16h_England_Round_2.shp")

# more on transformations here https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html
centroids <-
  st_transform(road_noise_shp, 4326) %>%  # convert os coords to lat/long system
  st_centroid()

road_noise_centroids <- centroids$geometry %>% unlist() %>%
     matrix(ncol = 2, byrow = T) %>%
     as.data.frame() %>%
     setNames(., c('Long', 'Lat')) %>%
     mutate(noise = case_when(road_noise_shp$noiseclass =="55.0-59.9" ~ 57 ,
                              road_noise_shp$noiseclass =="60.0-64.9" ~ 62,
                              road_noise_shp$noiseclass =="65.0-69.9" ~ 67,
                              road_noise_shp$noiseclass =="70.0-74.9" ~ 72,
                              T ~ 77))


# getting avg and variance of noise of all readings within 1km of the lsoa centroid
noise_mean_simple_postcode_store <- vector(mode = "numeric", length = nrow(lsoa_centroids))
noise_variance_simple_postcode_store <- vector(mode = "numeric", length = nrow(lsoa_centroids))

for (i in seq_along(noise_mean_simple_postcode_store)) {
  idx <-
    sqrt((road_noise_centroids$Long - lsoa_centroids$Long[i]) ^ 2 +
           (road_noise_centroids$Lat - lsoa_centroids$Lat[i]) ^ 2
    ) < 0.009
  
  if (sum(idx) > 0.99) {   
    noise_mean_simple_postcode_store[i] <-
      mean(road_noise_centroids$noise[idx])
    noise_variance_simple_postcode_store[i] <-
      var(road_noise_centroids$noise[idx])
  } else {
    noise_mean_simple_postcode_store[i] <- NA      # making NA if no readings within 1km
    noise_variance_simple_postcode_store[i] <- NA
  }
  
  if (i %% 100 == 0) {
    print(i)
  }
}

lsoa_road_noise <- data.frame(
  lsoa11cd = lsoa_centroids$lsoa11cd,
  noise_decibels_mean = noise_mean_simple_postcode_store,
  noise_decibels_var = noise_variance_simple_postcode_store
)


rm(list = c('road_noise_shp', 'centroid', 'road_noise_centroids'))   # a few gb memory saved!





## Joining all features into single df for app
all_data_lsoa <- price_paid_lsoa %>% 
  merge(all_deprivation_lsoa, by = 'lsoa11cd') %>% 
  merge(lsoa_flood_areas, by = 'lsoa11cd') %>% 
  merge(lsoa_big_ben_travel_time, by = 'lsoa11cd') %>% 
  merge(lsoa_road_noise, by = 'lsoa11cd') %>%
  merge(lsoa_centroids, by = 'lsoa11cd') %>%
  merge(epc_data_lsoa, by = 'lsoa11cd') 

fwrite(all_data_lsoa, 'data/all_data_lsoa.csv')








# wild swimming https://www.wildswimming.co.uk/wild-swim-map-uk/



# ### Not used: decoding polylines
# library(googlePolylines)
# library(leaflet)
# routing <- jsonlite::read_json('raw_data/big_ben_travel.json')
#
# polyline_decoded <- decode(routing$routes[[1]]$geometry)[[1]] %>% as.data.frame()
#
# leaflet()%>%
#   addTiles() %>%
#   addPolylines(data = polyline_decoded, lng = ~lon, lat = ~lat)
#
#



##### Ideas for app.R
# Aggregation: to single digit postcode level, and it then recommends for user based on their chosen params
# Highlights on map. Make it a
# super nice design, modular, golemified and very fast reactivity
# try tmaps for fast mapping
