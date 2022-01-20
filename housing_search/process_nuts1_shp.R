
# run this after other preprocessing scripts
# prereqisite for app

# simplifies NUTS1 shapefile, then extracts sample of coordoinates from the shapefiles



library(dplyr)
library(sf)

nuts1_closers_to_london <-
  c(
    "London",
    "South East (England)" ,
    "East of England",
    "East Midlands (England)",
    "West Midlands (England)",
    "South West (England)"
  )

nuts1_shape <- st_read("~/Downloads/NUTS_Level_1_(January_2018)_Boundaries/NUTS_Level_1_(January_2018)_Boundaries.shp")
nuts1_shape <- rmapshaper::ms_simplify(nuts1_shape, keep = 0.01)
nuts1_shape <- nuts1_shape %>% filter(nuts118nm %in% nuts1_closers_to_london)  # dropping areas too far
nuts1_shape <- nuts1_shape %>% select(geometry, nuts118cd)
nuts1_shape <- nuts1_shape %>% st_transform(4326)

st_write(nuts1_shape, "~/Desktop/shiny_apps/housing_search/housing_search/data/simpler_nuts1_shp/nuts1.shp")


setwd("~/Desktop/shiny_apps/housing_search/housing_search")
nuts1_shape_as_points <- st_read("data/simpler_nuts1_shp/nuts1.shp") %>%
  st_coordinates(nuts1_shape) %>% 
  as.data.frame() %>%
  select(X, Y)

interval <- ceiling(nrow(nuts1_shape_as_points) / 2000)  # to plot evenly, as altair only allows 5000 points
sample_idx <- seq(from = 1, to = (interval*1999) + 1, by = interval )
sample_idx <- sample_idx[sample_idx < nrow(nuts1_shape_as_points) ] 

nuts1_sample_points <- nuts1_shape_as_points[sample_idx, ]  # this goes to altair plot
fwrite(nuts1_sample_points, 'data/nuts1_sample_points.csv')




