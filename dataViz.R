#Data visualizations code
library(adehabitatHR)
library(leaflet)
library(mapview)
library(ggplot2)
library(tidyverse)
library(sf)

f <- "https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/LagoDAllDistCombined_data.csv"
d <- read_csv(f, col_names = TRUE)

d$mean_ltime <- as.POSIXct(d$mean_ltime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

d_sf <- st_as_sf(d, coords = c("mean_longitude", "mean_latitude"), crs = 4326)

gps_proj <- st_transform(d_sf, crs = 32718)

gps_proj$year <- year(gps_proj$mean_ltime)
