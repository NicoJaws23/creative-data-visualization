#Data visualizations code
library(adehabitatHR)
library(leaflet)
library(mapview)
library(ggplot2)
library(tidyverse)
library(sf)
library(tmap)

f <- "https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/LagoDAllDistCombined_data.csv"
d <- read_csv(f, col_names = TRUE)

d$mean_ltime <- as.POSIXct(d$mean_ltime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

d <- d |>
  select(mean_ltime, mean_x_proj, mean_y_proj, mean_alt) |>
  mutate(year = year(mean_ltime))

sf <- st_as_sf(d, coords = c("mean_x_proj", "mean_y_proj"), crs = 32718)

sf <- sf |>
  mutate(year = as.factor(year))

sp <- as(sf, "Spatial")

spdf <- SpatialPointsDataFrame(coordinates(sp), data = data.frame(year = sf$year))

kud <- kernelUD(spdf, h = "href", grid = 1000)  # h can be tuned

hr <- getverticeshr(kud, percent = 95)

hr_sf <- st_as_sf(hr)
hr_sf$year <- rownames(hr@data)
plot(hr_sf["2014", 4])

mapviewOptions(default = FALSE)
trails <- st_read("C:\\Users\\Jawor\\Desktop\\repos\\creative-data-visualization\\TBS_Trails.geojson")
river <- st_read("C:\\Users\\Jawor\\Desktop\\repos\\creative-data-visualization\\rio_tiputini.geojson")
trail_layer <- mapview(trails, color = "orange", layer.name = "Trail", lwd = 2)
river_layer <- mapview(river, color = "blue", layer.name = "River", lwd = 2)

d14 + trail_layer + river_layer

(d14 <- mapview(hr_sf["2014",], zcol = "year", layer.name = "Home Range", alpha.regions = 0.4))
(d15 <- mapview(hr_sf["2015",], zcol = "year", layer.name = "Home Range", alpha.regions = 0.4))
(d16 <- mapview(hr_sf["2016",], zcol = "year", layer.name = "Home Range", alpha.regions = 0.4))
(d17 <- mapview(hr_sf["2017",], zcol = "year", layer.name = "Home Range", alpha.regions = 0.4))
(d18 <- mapview(hr_sf["2018",], zcol = "year", layer.name = "Home Range", alpha.regions = 0.4))

