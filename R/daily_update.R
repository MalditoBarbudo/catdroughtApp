raster_brick_creation <- function(file) {
  load(file)
  raster::raster(spdf) %>%
    raster::projectRaster(crs = sp::CRS("+init=epsg:4326"))
}

day_files_checker <- function(folder, day) {

  files_to_load <- list.files(folder, full.names = TRUE, pattern = day, recursive = FALSE)

  # check if there is files
  if (length(files_to_load) < 1) {
    # if not, do it again for the previous day
    files_to_load <- day_files_checker(folder, as.character(lubridate::date(day) - 1))
  }

  # of there is files, then return them
  return(files_to_load)
}


#' Function to update the database with the dates missing
#'
#' Usually, this function will update the database with the daily data from serverprocess
#'
#' @param user
#' @param pass
#' @param port
#' @param path
#' @param changed
#' @param added
#'
#' @export
catdrought_daily_update <- function(
  user = 'guest', pass = 'guest', port = 'staging',
  path = '/home/vgranda/LFC/10_serverproces_data/CatDrought', changed = TRUE, added = TRUE
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
  # in the database OR also those that are present in the database but they have been
  # modified in serverprocess due to fixes or improvements in the data treatment by
  # Miquel:

  # First, we need to load the state of the files before the last update. We use a cached
  # file in the path folder, named fileSnapshot_cached.RData and compare with a new
  # fileSnapshot object to see differences
  load(file = file.path(path, 'fileSnapshot_cached.RData'))
  fileSnapshot_actual <- utils::fileSnapshot(
    path = file.path(path, 'Rdata', 'Maps', 'Current'),
    md5sum = TRUE, pattern = '.rda', recursive = TRUE
  )

  changes_object <- utils::changedFiles(fileSnapshot_cached, fileSnapshot_actual)

  added_rdas <- changes_object$added %>%
    magrittr::extract(
      stringr::str_detect(
        changes_object$added,
        pattern = "Fagus|Pinus|Quercus",
        negate = TRUE
      )
    )

  changed_rdas <- changes_object$changed %>%
    magrittr::extract(
      stringr::str_detect(
        changes_object$added,
        pattern = "Fagus|Pinus|Quercus",
        negate = TRUE
      )
    )


  dates_to_upload <- c(
    if (isTRUE(added)) {added_rdas} else {character()},
    if (isTRUE(changed)) {changed_rdas} else {character()}
  ) %>%
    stringr::str_sub(-14, -5) %>%
    unique()

  # now we make a loop for each date to update to create the rasters and update the
  # database, but checking if there is no updates
  if (length(dates_to_upload) < 1) {
    # closing database connections and writing the new cached data
    fileSnapshot_cached <- fileSnapshot_actual
    save(fileSnapshot_cached, file = file.path(path, 'fileSnapshot_cached.RData'))
    pool::poolReturn(temp_postgresql_conn)
    pool::poolClose(database)
    # exit the function
    return()
  }

  # after checking we have updates, lets looping!!
  for (date_sel in dates_to_upload) {

    # build the raster brick and create the temp table
    raster_high <- c(
      # NDD
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'DroughtStress', 'NDD', 'Overall'),
        day = date_sel
      ),
      # DDS
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'DroughtStress', 'DDS', 'Overall'),
        day = date_sel
      ),
      # DeepDrainage
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'DeepDrainage'),
        day = date_sel
      ),
      # Eplant
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'Eplant'),
        day = date_sel
      ),
      # Esoil
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'Esoil'),
        day = date_sel
      ),
      # LAI
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'LAI'),
        day = date_sel
      ),
      # NetPrec
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'NetPrec'),
        day = date_sel
      ),
      # PET
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'PET'),
        day = date_sel
      ),
      # Psi
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'Psi'),
        day = date_sel
      ),
      # Rain
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'Rain'),
        day = date_sel
      ),
      # REW
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'REW'),
        day = date_sel
      ),
      # Runoff
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'Runoff'),
        day = date_sel
      ),
      # Theta
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '200m', 'SPWB', 'Theta'),
        day = date_sel
      )
    ) %>%
      purrr::map(raster_brick_creation) %>%
      raster::stack()

    raster_low <- c(
      # NDD
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'DroughtStress', 'NDD', 'Overall'),
        day = date_sel
      ),
      # DDS
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'DroughtStress', 'DDS', 'Overall'),
        day = date_sel
      ),
      # DeepDrainage
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'DeepDrainage'),
        day = date_sel
      ),
      # Eplant
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'Eplant'),
        day = date_sel
      ),
      # Esoil
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'Esoil'),
        day = date_sel
      ),
      # LAI
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'LAI'),
        day = date_sel
      ),
      # NetPrec
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'NetPrec'),
        day = date_sel
      ),
      # PET
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'PET'),
        day = date_sel
      ),
      # Psi
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'Psi'),
        day = date_sel
      ),
      # Rain
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'Rain'),
        day = date_sel
      ),
      # REW
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'REW'),
        day = date_sel
      ),
      # Runoff
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'Runoff'),
        day = date_sel
      ),
      # Theta
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', '1km', 'SPWB', 'Theta'),
        day = date_sel
      )
    ) %>%
      purrr::map(raster_brick_creation) %>%
      raster::stack()

    raster_smooth <- c(
      # NDD
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'DroughtStress', 'NDD', 'Overall'),
        day = date_sel
      ),
      # DDS
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'DroughtStress', 'DDS', 'Overall'),
        day = date_sel
      ),
      # DeepDrainage
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'DeepDrainage'),
        day = date_sel
      ),
      # Eplant
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'Eplant'),
        day = date_sel
      ),
      # Esoil
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'Esoil'),
        day = date_sel
      ),
      # LAI
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'LAI'),
        day = date_sel
      ),
      # NetPrec
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'NetPrec'),
        day = date_sel
      ),
      # PET
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'PET'),
        day = date_sel
      ),
      # Psi
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'Psi'),
        day = date_sel
      ),
      # Rain
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'Rain'),
        day = date_sel
      ),
      # REW
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'REW'),
        day = date_sel
      ),
      # Runoff
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'Runoff'),
        day = date_sel
      ),
      # Theta
      day_files_checker(
        folder = file.path(path, 'Rdata', 'Maps', 'Current', 'Smoothed', 'SPWB', 'Theta'),
        day = date_sel
      )
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

  # closing database connections and writing the new cached data
  fileSnapshot_cached <- fileSnapshot_actual
  save(fileSnapshot_cached, file = file.path(path, 'fileSnapshot_cached.RData'))
  pool::poolReturn(temp_postgresql_conn)
  pool::poolClose(database)
}
