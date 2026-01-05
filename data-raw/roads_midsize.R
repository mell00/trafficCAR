# data-raw/roads_midsize.R
library(sf)
library(usethis)

roads_midsize <- st_read(
  "inst/extdata/roads.geojson",
  quiet = TRUE
)

use_data(roads_midsize, overwrite = TRUE)
