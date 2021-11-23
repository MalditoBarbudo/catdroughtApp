# polygons and nfi plots objects

## polygons ####
municipalities_polygons <- sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(poly_id = NOMMUNI, geometry)

counties_polygons <- sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(poly_id = NOMCOMAR, geometry)

watersheds_polygons <- sf::read_sf('data-raw/shapefiles/Concajs.shp') %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) %>%
  dplyr::select(poly_id = CONCA, geometry) %>%
  sf::st_cast('MULTIPOLYGON') %>%
  dplyr::group_by(poly_id) %>%
  dplyr::summarise(dplyr::across(.fns = sf::st_combine))

## nfi plots ####
nfidb <- lfcdata::nfi()

nfi4_plots <- nfidb$get_data('plot_nfi_4_results') %>%
  dplyr::select(plot_id) %>%
  dplyr::left_join(nfidb$get_data('plots')) %>%
  dplyr::select(plot_id, coords_longitude, coords_latitude) %>%
  sf::st_as_sf(
    coords = c('coords_longitude', 'coords_latitude'),
    crs = 4326
  )

nfi3_plots <- nfidb$get_data('plot_nfi_3_results') %>%
  dplyr::select(plot_id) %>%
  dplyr::left_join(nfidb$get_data('plots')) %>%
  dplyr::select(plot_id, coords_longitude, coords_latitude) %>%
  sf::st_as_sf(
    coords = c('coords_longitude', 'coords_latitude'),
    crs = 4326
  )
