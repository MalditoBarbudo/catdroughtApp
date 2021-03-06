library(tidyverse)
source('data-raw/polygon_objects_creation.R')
source('data-raw/translations.R')
source('data-raw/palette_builder.R')

# use_data internal for all
usethis::use_data(
  municipalities_polygons, counties_polygons, watersheds_polygons,
  app_translations, nfi3_plots, nfi4_plots,
  palettes_dictionary,
  # veguerias_polygons, provinces_polygons,
  # catalonia_polygons, natural_interest_area_polygons,
  # special_protection_natural_area_polygons, natura_network_2000_polygons,


  internal = TRUE, overwrite = TRUE
)
