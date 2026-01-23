# data-raw/roads_small.R
library(sf)
library(usethis)

roads_small <- st_read(
  "data-raw/roads.geojson",
  quiet = TRUE
)

use_data(roads_small, overwrite = TRUE)
