#Data visualizations code
library(adehabitatHR)
library(leaflet)
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
  return(list(evMap = evMap, evClip = elevation_clipped))
  
}

ev14 <- elev(d14, hr14$map, hr14$homerange, 150)
ev15 <- elev(d15, hr15$map, hr15$homerange, 150)
ev16 <- elev(d16, hr16$map, hr16$homerange, 150)
ev17 <- elev(d17, hr17$map, hr17$homerange, 150)
ev18 <- elev(d18, hr18$map, hr18$homerange, 150)
sync(ev14$evMap, ev15$evMap, ev16$evMap, ev17$evMap, ev18$evMap, ncol = 1)

#Adding Icons
mountain <- makeIcon(iconUrl = "https://www.pngmart.com/files/23/Cartoon-Mountains-PNG-Isolated-HD.png", iconWidth = 20, iconHeight = 20)
tree <- makeIcon(iconUrl = "https://img.pikbest.com/png-images/tree-clipart-cartoon-jungle-vector-green-big-tree_5856321.png!sw800", iconWidth = 20, iconHeight = 20)
hill <- makeIcon(iconUrl = "https://png.pngtree.com/png-vector/20220929/ourmid/pngtree-green-hill-ground-vector-clipart-png-image_6221576.png", iconWidth = 20, iconHeight = 20)
evIcon <- function(evClipped, hr, icon1, icon2, icon3, trailsDF, riverDF, maxThreshold, minThreshold){
  evPoints <- as.points(evClipped)
  evPoints <- project(evPoints, "EPSG:4326")
  evDF <- as.data.frame(evPoints, geom = "xy", na.rm = TRUE, cellvalues = TRUE)
  evDF <- evDF|>
    mutate(class = case_when(
      evDF$mean >= maxThreshold ~ "mountain",
      evDF$mean < maxThreshold & evDF$mean >= minThreshold ~ "hill",
      evDF$mean < minThreshold ~ "tree"
    ))
  map <- leaflet() |>
    addProviderTiles(providers$Esri.WorldTopoMap) |>
    addPolygons(data = st_transform(hr, 4326), color = "tan4", weight = 2, fillOpacity = 0.3, group = "Home Range") |>
    addPolylines(data = st_transform(trailsDF, 4326), color = "orange", weight = 2, group = "Trails") |>
    addPolylines(data = st_transform(riverDF, 4326), color = "blue", weight = 2, group = "River") |>
    addMarkers(data = filter(evDF, class == "mountain"), lng = ~x, lat = ~y, icon = icon1) |>
    addMarkers(data = filter(evDF, class == "hill"), lng = ~x, lat = ~y, icon = icon2) |>
    addMarkers(data = filter(evDF, class == "tree"), lng = ~x, lat = ~y, icon = icon3) |>
    addLayersControl(overlayGroups = c("Home Range", "Trails", "River"), options = layersControlOptions(collapsed = FALSE))
  return(map)
}
(i14 <- evIcon(ev14$evClip, hr14$homerange, icon1 = mountain, icon2 = hill, icon3 = tree, trails, river, maxThreshold = 260, minThreshold = 240))
(i15 <- evIcon(ev15$evClip, hr15$homerange, icon1 = mountain, icon2 = hill, icon3 = tree, trails, river, maxThreshold = 260, minThreshold = 240))
(i16 <- evIcon(ev16$evClip, hr16$homerange, icon1 = mountain, icon2 = hill, icon3 = tree, trails, river, maxThreshold = 260, minThreshold = 240))
(i17 <- evIcon(ev17$evClip, hr17$homerange, icon1 = mountain, icon2 = hill, icon3 = tree, trails, river, maxThreshold = 260, minThreshold = 240))
(i18 <- evIcon(ev18$evClip, hr18$homerange, icon1 = mountain, icon2 = hill, icon3 = tree, trails, river, maxThreshold = 260, minThreshold = 240))
