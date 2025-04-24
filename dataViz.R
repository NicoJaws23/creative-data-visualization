#Data visualizations code
library(adehabitatHR)
library(leafsync)
library(mapview)
library(tidyverse)
library(sf)
library(lubridate)
library(terra)

f <- "https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/LagoDAllDistCombined_data.csv"
d <- read_csv(f, col_names = TRUE)

trails <- st_read("https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/TBS_Trails.geojson") |>
  st_transform(32718)
river <- st_read("https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/rio_tiputini.geojson") |>
  st_transform(32718)

d <- d |>
  mutate(mean_ltime = as.POSIXct(mean_ltime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         year = year(mean_ltime)) |>
  dplyr::select(mean_ltime, mean_x_proj, mean_y_proj, year, mean_alt)

d14 <- d |>
  filter(year == 2014)
d15 <- d |>
  filter(year == 2015)
d16 <- d |>
  filter(year == 2016)
d17 <- d |>
  filter(year == 2017)
d18 <- d |>
  filter(year == 2018)

viewHR <- function(df, trailsDF, riverDF, year, HRcolor, elevation = c("Y", "N")){
  pts <- st_as_sf(df, coords = c("mean_x_proj", "mean_y_proj"), crs = 32718)
  pts_sp <- as(pts, "Spatial")
  proj4string(pts_sp) <- CRS("+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs")
  pts_simple <- SpatialPoints(pts_sp@coords, proj4string = CRS("+proj=utm +zone=18 +south +datum=WGS84"))
  khr <- kernelUD(pts_simple, h = "href")
  hr <- getverticeshr(khr, percent = 95)
  hr_sf <- st_as_sf(hr)
  hr_layer <- mapview(hr_sf, col.regions = HRcolor, color = HRcolor, alpha.regions = 0.4, layer.name = paste(year, "Home Range"))
  trail_layer <- mapview(trailsDF, color = "orange", layer.name = "Trails", lwd = 2)
  river_layer <- mapview(riverDF, color = "blue", layer.name = "River", lwd = 2)
  map <- hr_layer + trail_layer + river_layer
  return(list(map = map, homerange = hr_sf))
}

hr14 <- viewHR(d14, trails, river, 2014, "tan4")
hr15 <- viewHR(d15, trails, river, 2015, "tan4")
hr16 <- viewHR(d16, trails, river, 2016, "tan4")
hr17 <- viewHR(d17, trails, river, 2017, "tan4")
hr18 <- viewHR(d18, trails, river, 2018, "tan4")
hrFull <- viewHR(d, trails, river, "All Time", "tan4")
sync(hr14$map, hr15$map, hr16$map, hr17$map, hr18$map, ncol = 1)

#Making elevation maps
elev <- function(df, map, hr, res){
  points <- vect(df, geom = c("mean_x_proj", "mean_y_proj"), crs = "EPSG:32718")
  grid <- rast(ext(points), resolution = res, crs = "EPSG:32718")
  plot(grid)
  elevation_raster <- rasterize(points, grid, field = "mean_alt", fun = mean, na.rm = TRUE)
  
  homerange_vect <- vect(hr)
  homerange_vect <- project(homerange_vect, crs(elevation_raster))
  elevation_clipped <- mask(elevation_raster, homerange_vect)
  
  ev <- mapview(elevation_clipped, layer.name = "Average Elevation", col.regions = terrain.colors(20), legend = TRUE, na.color = NA)
  evMap <- map + ev
  return(evMap)
  
}

ev14 <- elev(d14, hr14$map, hr14$homerange, 150)
ev15 <- elev(d15, hr15$map, hr15$homerange, 150)
ev16 <- elev(d16, hr16$map, hr16$homerange, 150)
ev17 <- elev(d17, hr17$map, hr17$homerange, 150)
ev18 <- elev(d18, hr18$map, hr18$homerange, 150)
sync(ev14, ev15, ev16, ev17, ev18, ncol = 1)
