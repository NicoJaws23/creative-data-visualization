#Data visualizations code
library(adehabitatHR)
library(leaflet)
library(mapview)
library(ggplot2)
library(tidyverse)
library(sf)
library(lubridate)
library(tmap)

f <- "https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/LagoDAllDistCombined_data.csv"
d <- read_csv(f, col_names = TRUE)

trails <- st_read("C:\\Users\\Jawor\\Desktop\\repos\\creative-data-visualization\\TBS_Trails.geojson") |>
  st_transform(32718)
river <- st_read("C:\\Users\\Jawor\\Desktop\\repos\\creative-data-visualization\\rio_tiputini.geojson") |>
  st_transform(32718)

d <- d |>
  mutate(mean_ltime = as.POSIXct(mean_ltime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         year = year(mean_ltime)) |>
  select(mean_ltime, mean_x_proj, mean_y_proj, year, mean_alt)

pts <- st_as_sf(d, coords = c("mean_x_proj", "mean_y_proj"), crs = 32718)
pts$year <- as.factor(pts$year)
pts_sp <- as(pts, "Spatial")
proj4string(pts_sp) <- CRS("+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs")

# Convert to a simpler SpatialPoints object if necessary
pts_simple <- SpatialPoints(pts_sp@coords, proj4string = CRS("+proj=utm +zone=18 +south +datum=WGS84"))

# Run kernelUD() on the simpler object
khr <- kernelUD(pts_simple, h = "href")


hr <- getverticeshr(khr, percent = 95)
hr_sf <- st_as_sf(hr)
hr_sf$year <- as.factor(row.names(hr))

unique(hr_sf$year)
str(hr_sf$year)

mapviewOptions(basemaps = "Esri.WorldImagery")

map_list <- lapply(levels(hr_sf$year), function(y) {
  hr_layer <- mapview(hr_sf[hr_sf$year == y, ], zcol = "year", alpha.regions = 0.4, layer.name = paste("Home Range", y))
  trail_layer <- mapview(trails, color = "orange", layer.name = "Trails", lwd = 2)
  river_layer <- mapview(river, color = "blue", layer.name = "River", lwd = 2)
  
  hr_layer + trail_layer + river_layer
})

map_list[[3]]
# Extract the year from the original data (assuming 'mean_ltime' contains the datetime information)
hr_sf$year <- as.factor(format(pts_sp$mean_ltime, "%Y"))


