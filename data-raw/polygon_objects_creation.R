# polygons and nfi plots objects

## polygons ####
municipalities_polygons <- sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(poly_id = NOMMUNI, geometry)

counties_polygons <- sf::read_sf('data-raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp') %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(poly_id = NOMCOMAR, geometry)

watersheds_polygons <- sf::read_sf('data-raw/shapefiles/Concajs.shp') %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  dplyr::select(poly_id = CONCA, geometry)

## nfi plots ####
nfidb <- tidyNFI::nfi_connect(
  user = 'guest', password = 'guest', host = NULL, port = NULL, dbname = 'tururu'
)

nfi4_plots <- dplyr::tbl(nfidb, 'PLOTS') %>%
  dplyr::select(
    plot_id, coords_longitude, coords_latitude, presence_NFI_4
  ) %>%
  dplyr::filter(
    presence_NFI_4
  ) %>%
  dplyr::collect() %>%
  sf::st_as_sf(
    coords = c('coords_longitude', 'coords_latitude'),
    crs = '+proj=longlat +datum=WGS84'
  )

nfi3_plots <- dplyr::tbl(nfidb, 'PLOTS') %>%
  dplyr::select(
    plot_id, coords_longitude, coords_latitude, presence_NFI_3
  ) %>%
  dplyr::filter(
    presence_NFI_3
  ) %>%
  dplyr::collect() %>%
  sf::st_as_sf(
    coords = c('coords_longitude', 'coords_latitude'),
    crs = '+proj=longlat +datum=WGS84'
  )

tidyNFI::nfi_close(nfidb)
