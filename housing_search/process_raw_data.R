


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

simple_coords <- openxlsx::read.xlsx("raw_data/simple_coords.xlsx")

land_registry_price_paid <-
  fread(
    "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2021.csv"
  )

price_paid_simple_postcode <- land_registry_price_paid %>%
  mutate(simple_postcode = substr(V4, 1, 4),
         sale_date = ymd(substr(V3, 1, 10))) %>%
  rename(price = V2) %>%
  select(simple_postcode, sale_date, price) %>%
  group_by(simple_postcode) %>%
  summarise(price = mean(price)) %>%
  filter(simple_postcode != "")

price_and_coords <-
  price_paid_simple_postcode %>%  # lose 5 out of 2520 simple postcodes in this join: good enough in this case
  merge(simple_coords, by.x = "simple_postcode", by.y = "SIMPLE_POSTCODE")


# convert OS grid reference to lat/long (ref https://www.movable-type.co.uk/scripts/latlong-os-gridref.html)






# find LSOAs in each postcode
postcode_lsoa_lookup <-
  fread("raw_data/PCD_OA_LSOA_MSOA_LAD_AUG21_UK_LU.csv",
        data.table = F)

postcode_lsoa_lookup_simple <- postcode_lsoa_lookup %>%
  mutate(simple_postcode = substr(pcds, 1, 4)) %>%
  select(simple_postcode, lsoa11cd, lsoa11nm) %>%
  distinct(lsoa11cd, .keep_all = T)

fwrite(postcode_lsoa_lookup_simple, 'data/lsoa_simple_postcode_lookup.csv')

postcode_lsoa_lookup <- postcode_lsoa_lookup_simple # overwriting



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

all_deprivation_simple_postcode <- all_deprivation_lsoa %>%
  merge(postcode_lsoa_lookup, by = "lsoa11cd") %>%
  select(-lsoa11nm,-lsoa11cd) %>%
  group_by(simple_postcode) %>%
  summarise_all(mean)





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
  mutate(lsoa11cd = lsoa_shp$lsoa11cd) %>%
  merge(postcode_lsoa_lookup, by = "lsoa11cd")

simple_postcode_centroids <- lsoa_centroids %>%
  group_by(simple_postcode) %>%
  summarise(
    lat_min = min(Lat),
    lat_max = max(Lat),
    long_min = min(Long),
    long_max = max(Long)
  ) %>%
  mutate(lat = lat_min + (lat_max - lat_min) / 2,
         long = long_min + (long_max - long_min) / 2) %>%
  select(simple_postcode, lat, long)








# flood risk https://environment.data.gov.uk/apiportal
flood_areas <-
  jsonlite::read_json('https://environment.data.gov.uk/flood-monitoring/id/floodAreas')

flood_areas_df <- purrr::map(flood_areas$items, as.data.frame) %>%
  rbindlist(fill = T) %>%
  select(lat, long)   # 'polygon' col gives URLs to get polygons only


counter <-
  vector(mode = 'numeric',
         length = nrow(simple_postcode_centroids))
for (i in seq_along(1:length(counter))) {
  # within 0.09 (10km) of how many flood areas?
  
  counter[i] <-
    sum(sqrt((flood_areas_df$lat - simple_postcode_centroids$lat[i]) ^ 2 +
               (flood_areas_df$long - simple_postcode_centroids$long[i]) ^ 2
    ) < 0.09)
  
}

simple_postcode_flood_areas <-
  data.frame(simple_postcode = simple_postcode_centroids$simple_postcode,
             flood_areas_count = counter)









# driving time from Big Ben with OSRM – with lat/long for each centroid of single digit postcode
duration <- rep(0, nrow(simple_postcode_centroids))
distance <- rep(0, nrow(simple_postcode_centroids))

for (i in seq_along(1:nrow(simple_postcode_centroids))) {
  query_string <- paste0(
    '0.0.0.0:5000/route/v1/driving/',
    '0.124600,51.500700;',
    round(simple_postcode_centroids$long[i], 6),
    ',',
    round(simple_postcode_centroids$lat[i], 6)
  )
  
  curl::curl_download(query_string, 'raw_data/big_ben_travel.json')
  routing <- jsonlite::read_json('raw_data/big_ben_travel.json')
  
  duration[i] <- routing$routes[[1]]$duration
  distance[i] <- routing$routes[[1]]$distance
  
  print(i)
}

simple_postcode_big_ben_travel_time <- data.frame(
  simple_postcode = simple_postcode_centroids$simple_postcode,
  commute_hours = duration / 3600,
  commute_distance_km = distance / 1000
)






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


# getting avg and variance of noise of all readings within 10km of the simple_postcode centroid
noise_mean_simple_postcode_store <- vector(mode = "numeric", length = nrow(simple_postcode_centroids))
noise_variance_simple_postcode_store <- vector(mode = "numeric", length = nrow(simple_postcode_centroids))

for (i in seq_along(noise_mean_simple_postcode_store)) {
  idx <-
    sqrt((road_noise_centroids$Long - simple_postcode_centroids$long[i]) ^ 2 + 
           (road_noise_centroids$Lat - simple_postcode_centroids$lat[i]) ^ 2) < 0.09
  noise_mean_simple_postcode_store[i] <- mean(road_noise_centroids$noise[idx])
  noise_variance_simple_postcode_store[i] <- var(road_noise_centroids$noise[idx])
  print(i)
}

simple_postcode_road_noise <- data.frame(
  simple_postcode = simple_postcode_centroids$simple_postcode,
  noise_decibels_mean = noise_mean_simple_postcode_store,
  noise_decibels_var = noise_variance_simple_postcode_store
)


rm(list = c('road_noise_shp', 'centroid', 'road_noise_centroids'))   # a few gb memory saved!





## Joining all features into single df for app
all_data_simple_postcode <- price_and_coords %>% 
  merge(all_deprivation_simple_postcode, by = 'simple_postcode') %>% 
  merge(simple_postcode_flood_areas, by = 'simple_postcode') %>% 
  merge(simple_postcode_big_ben_travel_time, by = 'simple_postcode') %>% 
  merge(simple_postcode_road_noise, by = 'simple_postcode') %>%
  merge(simple_postcode_centroids, by = 'simple_postcode') 

fwrite(all_data_simple_postcode, 'data/all_data_simple_postcode.csv')




# merging LSOA level polygons to make simple_postcode polygons
lsoa_shp_with_simple_postcode <- lsoa_shp %>% merge(postcode_lsoa_lookup, by = 'lsoa11cd') 

simple_postcode_shp <- lsoa_shp_with_simple_postcode %>%    # effectively loops with an individual iter to dissolve each simple_polygon
  split(.$simple_postcode) %>%
  lapply(st_union) %>% 
  do.call(c, .)

simple_postcode_shp <- st_as_sf(simple_postcode_shp) %>%
  mutate(simple_postcode = simple_postcode_flood_areas$simple_postcode) %>%
  rename(geometry = x)

st_write(simple_postcode_shp, 'data/simple_postcodes_shapefile/simple.shp')






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
