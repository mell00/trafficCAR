# data-raw/roads_small.R
library(sf)
library(usethis)

roads_small <- st_read(
  "inst/extdata/roads.geojson",
  quiet = TRUE
)

use_data(roads_small, overwrite = TRUE)
