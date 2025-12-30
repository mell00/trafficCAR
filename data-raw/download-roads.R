library(osmdata)
library(sf)

bbox <- getbb("College Station, Texas")

roads <- opq(bbox) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

edges_sf <- roads$osm_lines
