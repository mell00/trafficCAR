# data-raw/download_roads.R
# download road network from OpenStreetMap and save as GeoJSON (roads.geojson)
# script for data preparation only

library(osmdata)
library(sf)

# parameters
place_name <- "Los Angeles, California"
out_file <- "inst/extdata/roads.geojson"

# download
bbox <- getbb(place_name)

roads <- opq(bbox) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

edges_sf <- roads$osm_lines

# basic cleaning
edges_sf <- edges_sf[!st_is_empty(edges_sf), ]
edges_sf <- st_make_valid(edges_sf)

# save
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
st_write(edges_sf, out_file, delete_dsn = TRUE)

message("Saved roads to: ", out_file)
