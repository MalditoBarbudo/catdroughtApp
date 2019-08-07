raster_brick_creation <- function(file) {
  load(file)
  raster::raster(spdf) %>%
    raster::projectRaster(crs = sp::CRS("+init=epsg:4326"))
}


#' Function to update the database with the dates missing
#'
#' Usually, this function will update the database with the daily data from serverprocess
#'
#' @param user
#' @param pass
#' @param dry
#'
#' @export
catdrought_daily_update <- function(
  user = 'guest', pass = 'guest', port = 'staging',
  path = '/home/vgranda/LFC/10_serverprocess_data/CatDrought', dry = TRUE
) {

  port <- switch (port,
    'staging' = 5433,
    'production' = 5432
  )

  # db connection
  database <- pool::dbPool(
    RPostgreSQL::PostgreSQL(),
    user = user,
    password = pass,
    dbname = 'catdrought_db',
    host = '158.109.46.23',
    port = port
  )
  temp_postgresql_conn <- pool::poolCheckout(database)

  # vars to check
  vars <- c(
    list.files(
      file.path(
        path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB'
      ), include.dirs = TRUE
    ),
    list.files(
      file.path(
        path, 'Rdata', 'Maps', 'Current', '200m', 'DroughtStress'
      ), include.dirs = TRUE
    )
  )

  # we need the new dates, meaning the dates present in data folder that are not present
  # in the database:
  # dates in folder
  dates_daily <- list.files(
    file.path(
      path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'NetPrec'
    ), full.names = FALSE, pattern = '.rda'
  ) %>% stringr::str_remove(pattern = '.rda')

  dates_daily_formatted <- dates_daily %>%
    stringr::str_remove_all(pattern = '-')


  # the dates in the db
  dates_database_formatted <- dplyr::db_list_tables(database) %>%
    stringr::str_extract(pattern = '[0-9]{8}') %>%
    purrr::discard(~ is.na(.x)) %>%
    unique() %>%
    sort()

  # dates to upload
  dates_to_upload <- dates_daily[
    which(!(dates_daily_formatted %in% dates_database_formatted))
    ]

  # now we make a loop for each date to update to create the rasters and update the
  # database, but checking if there is no updates
  if (length(dates_to_upload) < 1) {
    return()
  }

  # after checking we have updates, lets looping!!
  for (date_sel in dates_to_upload) {

    # build the raster brick and create the temp table
    raster_high <- list.files(
      file.path(
        path, 'Rdata', 'Maps', 'Current', '200m'
      ), full.names = TRUE, pattern = date_sel, recursive = TRUE
    ) %>%
      purrr::map(raster_brick_creation) %>%
      raster::stack()

    raster_low <- list.files(
      file.path(
        path, 'Rdata', 'Maps', 'Current', '1km'
      ), full.names = TRUE, pattern = date_sel, recursive = TRUE
    ) %>%
      purrr::map(raster_brick_creation) %>%
      raster::stack()

    raster_smooth <- list.files(
      file.path(
        path, 'Rdata', 'Maps', 'Current', 'Smoothed'
      ), full.names = TRUE, pattern = date_sel, recursive = TRUE
    ) %>%
      purrr::map(raster_brick_creation) %>%
      raster::stack()

    # create the temp tables
    raster_high %>% {
      rpostgis::pgWriteRast(
        temp_postgresql_conn, 'temp200', ., blocks = 25, overwrite = TRUE
      )
    }

    raster_low %>% {
      rpostgis::pgWriteRast(
        temp_postgresql_conn, 'temp1000', ., blocks = 25, overwrite = TRUE
      )
    }

    raster_smooth %>% {
      rpostgis::pgWriteRast(
        temp_postgresql_conn, 'tempsmooth', ., blocks = 25, overwrite = TRUE
      )
    }

    # traspose to the partitioned table
    create_parti_table_query <- glue::glue(
      "
     DROP TABLE IF EXISTS daily.catdrought_high_{stringr::str_remove_all(date_sel, '-')};
     CREATE TABLE daily.catdrought_high_{stringr::str_remove_all(date_sel, '-')} (
         CHECK (day = DATE '{date_sel}')
     ) INHERITS (daily.catdrought_high);

     DROP TABLE IF EXISTS daily.catdrought_low_{stringr::str_remove_all(date_sel, '-')};
     CREATE TABLE daily.catdrought_low_{stringr::str_remove_all(date_sel, '-')} (
         CHECK (day = DATE '{date_sel}')
     ) INHERITS (daily.catdrought_low);

     DROP TABLE IF EXISTS daily.catdrought_smooth_{stringr::str_remove_all(date_sel, '-')};
     CREATE TABLE daily.catdrought_smooth_{stringr::str_remove_all(date_sel, '-')} (
         CHECK (day = DATE '{date_sel}')
     ) INHERITS (daily.catdrought_smooth);
    "
    )
    pool::dbExecute(database, create_parti_table_query)


    alter_table_query <- glue::glue(
      "
     ALTER TABLE temp200 ADD COLUMN day date;
     UPDATE temp200 SET day = '{date_sel}'::date;
     INSERT INTO daily.catdrought_high_{stringr::str_remove_all(date_sel, '-')} (rid, day, rast)
       SELECT rid,day,rast FROM temp200;

    ALTER TABLE temp1000 ADD COLUMN day date;
     UPDATE temp1000 SET day = '{date_sel}'::date;
     INSERT INTO daily.catdrought_low_{stringr::str_remove_all(date_sel, '-')} (rid, day, rast)
       SELECT rid,day,rast FROM temp1000;

    ALTER TABLE tempsmooth ADD COLUMN day date;
     UPDATE tempsmooth SET day = '{date_sel}'::date;
     INSERT INTO daily.catdrought_smooth_{stringr::str_remove_all(date_sel, '-')} (rid, day, rast)
       SELECT rid,day,rast FROM tempsmooth;
    "
    )
    pool::dbExecute(database, alter_table_query)

  }


  # closing database connections
  pool::poolReturn(temp_postgresql_conn)
  pool::poolClose(database)
}
