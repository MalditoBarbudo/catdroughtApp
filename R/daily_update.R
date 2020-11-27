raster_brick_creation <- function(file) {
  spdf <- readr::read_rds(file)
  raster::raster(spdf) %>%
    raster::projectRaster(crs = sp::CRS("+init=epsg:4326"))
}

# day_files_checker <- function(folder, day) {
#
#   files_to_load <- list.files(folder, full.names = TRUE, pattern = day, recursive = FALSE)
#
#   # check if there is files
#   if (length(files_to_load) < 1) {
#     # if not, do it again for the previous day
#     files_to_load <- day_files_checker(folder, as.character(lubridate::date(day) - 1))
#   }
#
#   # of there is files, then return them
#   return(files_to_load)
# }


#' Function to update the database with the dates missing
#'
#' Usually, this function will update the database with the daily data from serverprocess
#'
#' @param user
#' @param pass
#' @param port
#'
#' @export
catdrought_daily_update <- function(user, password, dbname, host, port) {

  # db connection
  database <- pool::dbPool(
    RPostgres::Postgres(),
    user = user,
    password = password,
    dbname = dbname,
    host = host,
    port = port
  )
  temp_postgresql_conn <- pool::poolCheckout(database)

  # vars to check
  vars <- list.files(
    file.path(
      '/home', 'miquel', 'CatDroughtEngine', 'Rdata', 'Maps', 'Current', '200m'
    ), include.dirs = TRUE
  )

  # dates to check
  dates_daily <- list.files(
    file.path(
      '/home', 'miquel', 'CatDroughtEngine', 'Rdata', 'Maps', 'Current', '200m', 'Eplant'
    ), full.names = FALSE, pattern = '.rds'
  ) %>% stringr::str_remove(pattern = '.rds')



  # lets looping!!
  for (date_sel in dates_daily) {

    # we need to check if the table is already in the database
    high_table_name <-
      glue::glue("daily.catdrought_high_{stringr::str_remove_all(date_sel, '-')}")

    if (!dplyr::db_has_table(high_table_name)) {
      message(glue::glue("Process for {date_sel}:"))

      # build the raster brick and create the temp table
      message(glue::glue("Building high res raster for {date_sel}"))
      raster_high <- list.files(
        file.path(
          '/home', 'miquel', 'CatDroughtEngine', 'Rdata', 'Maps', 'Current', '200m'
        ), full.names = TRUE, pattern = date_sel, recursive = TRUE
      ) %>%
        purrr::map(raster_brick_creation) %>%
        raster::stack() %>%
        magrittr::set_names(vars)

      message(glue::glue("Building low res raster for {date_sel}"))
      raster_low <- list.files(
        file.path(
          '/home', 'miquel', 'CatDroughtEngine', 'Rdata', 'Maps', 'Current', '1km'
        ), full.names = TRUE, pattern = date_sel, recursive = TRUE
      ) %>%
        purrr::map(raster_brick_creation) %>%
        raster::stack() %>%
        magrittr::set_names(vars)

      message(glue::glue("Building smoothed raster for {date_sel}"))
      raster_smooth <- list.files(
        file.path(
          '/home', 'miquel', 'CatDroughtEngine', 'Rdata', 'Maps', 'Current', 'Smoothed'
        ), full.names = TRUE, pattern = date_sel, recursive = TRUE
      ) %>%
        purrr::map(raster_brick_creation) %>%
        raster::stack() %>%
        magrittr::set_names(vars)

      # write the rasters in the db
      message(glue::glue("Writing high res raster for {date_sel}"))
      try_write_high <- try({
        raster_high %>% {
          rpostgis::pgWriteRast(
            temp_postgresql_conn, 'temp200', ., blocks = 25, overwrite = TRUE
          )
        }
      })

      if (class(try_write_high) == 'try-error') {
        pool::poolReturn(temp_postgresql_conn)
        pool::poolClose(catdrought_db)
        catdrought_db <- pool::dbPool(
          RPostgreSQL::PostgreSQL(),
          user = user,
          password = password,
          dbname = dbname,
          host = host,
          port = port
        )
        temp_postgresql_conn <- pool::poolCheckout(catdrought_db)

        raster_high %>% {
          rpostgis::pgWriteRast(
            temp_postgresql_conn, 'temp200', ., blocks = 25, overwrite = TRUE
          )
        }
      }
      # raster_high %>% {
      #   rpostgis::pgWriteRast(
      #     temp_postgresql_conn, 'temp200', ., blocks = 25, overwrite = TRUE
      #   )
      # }

      message(glue::glue("Writing low res raster for {date_sel}"))
      try_write_low <- try({
        raster_low %>% {
          rpostgis::pgWriteRast(
            temp_postgresql_conn, 'temp1000', ., blocks = 25, overwrite = TRUE
          )
        }
      })

      if (class(try_write_low) == 'try-error') {
        pool::poolReturn(temp_postgresql_conn)
        pool::poolClose(catdrought_db)
        catdrought_db <- pool::dbPool(
          RPostgreSQL::PostgreSQL(),
          user = user,
          password = password,
          dbname = dbname,
          host = host,
          port = port
        )
        temp_postgresql_conn <- pool::poolCheckout(catdrought_db)

        raster_low %>% {
          rpostgis::pgWriteRast(
            temp_postgresql_conn, 'temp1000', ., blocks = 25, overwrite = TRUE
          )
        }
      }
      # raster_low %>% {
      #   rpostgis::pgWriteRast(
      #     temp_postgresql_conn, 'temp1000', ., blocks = 25, overwrite = TRUE
      #   )
      # }

      message(glue::glue("Writing smoothed raster for {date_sel}"))
      try_write_smooth <- try({
        raster_smooth %>% {
          rpostgis::pgWriteRast(
            temp_postgresql_conn, 'tempsmooth', ., blocks = 25, overwrite = TRUE
          )
        }
      })

      if (class(try_write_smooth) == 'try-error') {
        pool::poolReturn(temp_postgresql_conn)
        pool::poolClose(catdrought_db)
        catdrought_db <- pool::dbPool(
          RPostgreSQL::PostgreSQL(),
          user = user,
          password = password,
          dbname = dbname,
          host = host,
          port = port
        )
        temp_postgresql_conn <- pool::poolCheckout(catdrought_db)

        raster_smooth %>% {
          rpostgis::pgWriteRast(
            temp_postgresql_conn, 'tempsmooth', ., blocks = 25, overwrite = TRUE
          )
        }
      }
      # raster_smooth %>% {
      #   rpostgis::pgWriteRast(
      #     temp_postgresql_conn, 'tempsmooth', ., blocks = 25, overwrite = TRUE
      #   )
      # }


      # traspose to the partitioned table
      message(glue::glue("Creating partitioned tables for {date_sel}"))
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

      try_create_parti_table <- try({
        pool::dbExecute(catdrought_db, create_parti_table_query)
      })

      if (class(try_create_parti_table) == 'try-error') {
        pool::poolReturn(temp_postgresql_conn)
        pool::poolClose(catdrought_db)
        catdrought_db <- pool::dbPool(
          RPostgreSQL::PostgreSQL(),
          user = 'ifn',
          password = 'IFN2018creaf',
          dbname = 'catdrought_db',
          host = 'laboratoriforestal.creaf.uab.cat',
          port = 5432
        )
        temp_postgresql_conn <- pool::poolCheckout(catdrought_db)

        pool::dbExecute(catdrought_db, create_parti_table_query)
      }
      # pool::dbExecute(catdrought_db, create_parti_table_query)


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

      try_alter_table <- try({
        pool::dbExecute(catdrought_db, alter_table_query)
      })

      if (class(try_alter_table) == 'try-error') {
        pool::poolReturn(temp_postgresql_conn)
        pool::poolClose(catdrought_db)
        catdrought_db <- pool::dbPool(
          RPostgreSQL::PostgreSQL(),
          user = 'ifn',
          password = 'IFN2018creaf',
          dbname = 'catdrought_db',
          host = 'laboratoriforestal.creaf.uab.cat',
          port = 5432
        )
        temp_postgresql_conn <- pool::poolCheckout(catdrought_db)

        pool::dbExecute(catdrought_db, alter_table_query)
      }
    } else {
      message(glue::glue('Tables for {date} already exists'))
    }
  }
  # closing database connections and writing the new cached data
  pool::poolReturn(temp_postgresql_conn)
  pool::poolClose(database)
}
