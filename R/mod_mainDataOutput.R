#' @title mod_mainDataOutput and mod_mainData
#'
#' @description Shiny module to get the data
#'
#' @param id
#'
#' @export
mod_mainDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' @title mod_mainData server function
#'
#' @details mod_mainData always return the data in the 3043 projection
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,map_reactives reactives from modules
#' @param catdroughtdb object to access the meteoland db
#' @param lang lang selected
#'
#' @importFrom dplyr n
#'
#' @export
#'
#' @rdname mod_mainDataOuput
mod_mainData <- function(
  input, output, session,
  data_reactives, map_reactives,
  catdroughtdb, lang
) {

  ## waiter/hostess progress ####
  # set a progress with waiter. We will use infinite TRUE, that way we dont
  # need to calculate any steps durations
  # 1. hostess progress
  hostess_raster <- waiter::Hostess$new(infinite = TRUE)
  hostess_raster$set_loader(waiter::hostess_loader(
    svg = 'images/hostess_image.svg',
    progress_type = 'fill',
    fill_direction = 'btt'
  ))
  hostess_ts <- waiter::Hostess$new(infinite = TRUE)
  hostess_ts$set_loader(waiter::hostess_loader(
    svg = 'images/hostess_image.svg',
    progress_type = 'fill',
    fill_direction = 'btt'
  ))


  # data reactive with the raster ####
  raster_selected_daily <- shiny::reactive({

    shiny::validate(
      shiny::need(data_reactives$date_daily, 'No date selected')
    )

    # 2. waiter overlay related to map id
    waiter_map <- waiter::Waiter$new(
      id = 'overlay_div',
      html = shiny::tagList(
        hostess_raster$get_loader(),
        shiny::h3(translate_app("progress_raster", lang())),
        shiny::p(translate_app("progress_detail_raster", lang()))
      ),
      color = '#E8EAEB'
    )

    waiter_map$show()
    # waiter_map$update(
    #   html = shiny::tagList(
    #     hostess_raster$get_loader(
    #       svg = 'images/hostess_image.svg',
    #       progress_type = 'fill',
    #       fill_direction = 'btt'
    #     ),
    #     shiny::h3(translate_app("progress_raster", lang())),
    #     shiny::p(translate_app("progress_detail_raster", lang()))
    #   )
    # )
    hostess_raster$start()
    on.exit(hostess_raster$close(), add = TRUE)
    on.exit(waiter_map$hide(), add = TRUE)

    # date
    date_sel <- as.character(data_reactives$date_daily)

    # raster_res
    raster_res <- catdroughtdb$get_raster(date_sel, 'raster')
    return(raster_res)
  })

  ## timeseries reactive ####
  # A reactive with the dataInput ractives as inputs. They will determine the
  # click that we need. For the click, we also need map_reactives.
  # So, based on dataInputs we know if is map click, shape click or file uploaded,
  # so we just select the one that we need as sf
  # After that, we just need to execute the catdroughtdb$get_current_time_series
  # function to get the time series dataframe to return as the data reactive.
  #
  # So, we will need:
  #   - map_click reactive
  #   - map_shape reactive
  #   - nfi plots reactive
  #   - file reactive
  #   - time series reactive (the only one returned)
  map_click_sf_builder <- shiny::reactive({

    shiny::validate(
      shiny::need(map_reactives$map_daily_click, 'no map click')
    )

    clicked_pixel <- map_reactives$map_daily_click
    point_sel <- tibble::tibble(
      point_id = 'clicked_coords',
      long = clicked_pixel$lng,
      lat = clicked_pixel$lat
    ) %>%
      sf::st_as_sf(
        coords = c('long', 'lat'),
        crs = 4326
      )

    return(point_sel)
  })

  map_shape_sf_builder <- shiny::reactive({
    shiny::validate(
      shiny::need(map_reactives$map_daily_shape_click, 'no map click')
    )

    clicked_poly <- map_reactives$map_daily_shape_click
    polygon_object <- rlang::eval_tidy(
      rlang::sym(glue::glue("{tolower(data_reactives$display_daily)}_polygons"))
    ) %>%
      dplyr::filter(
        poly_id == clicked_poly$id
      )

    return(polygon_object)
  })

  nfi_plots_sf_builder <- shiny::reactive({
    shiny::validate(
      shiny::need(map_reactives$map_daily_marker_click, 'no map click')
    )

    clicked_marker <- map_reactives$map_daily_marker_click

    point_sel <- tibble::tibble(
      point_id = clicked_marker$id,
      long = clicked_marker$lng,
      lat = clicked_marker$lat
    ) %>%
      sf::st_as_sf(
        coords = c('long', 'lat'),
        crs = 4326
      )

    return(point_sel)
  })

  # file reactive
  file_sf_builder <- shiny::reactive({
    shiny::validate(
      shiny::need(data_reactives$user_file_sel, 'no file uploaded yet')
    )

    path_to_file <- data_reactives$user_file_sel$datapath
    file_name <- data_reactives$user_file_sel$name

    # check if there is user file
    if (is.null(path_to_file)) {
      user_file_sf <- NULL
    } else {
      # alert and validation for the file extension
      if (!stringr::str_detect(file_name, '.gpkg$|.zip$')) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = translate_app('sweet_alert_fileext_title', lang()),
          text = translate_app('sweet_alert_fileext_text', lang())
        )
        shiny::validate(
          shiny::need(
            stringr::str_detect(file_name, '.gpkg$|.zip$'), 'bad file format'
          )
        )
      }

      # check if zip (shapefile) or gpkg to load the data
      if (stringr::str_detect(file_name, '.zip$')) {
        tmp_folder <- tempdir()
        utils::unzip(path_to_file, exdir = tmp_folder)

        user_file_sf <- sf::st_read(
          list.files(tmp_folder, '.shp', recursive = TRUE, full.names = TRUE),
          as_tibble = TRUE
        ) %>%
          sf::st_transform(4326)
      } else {
        # gpkg
        user_file_sf <- sf::st_read(path_to_file) %>%
          sf::st_transform(4326)
      }
    }

    shiny::validate(
      shiny::need(user_file_sf, 'no file provided')
    )

    # check for poly_id
    if (!"poly_id" %in% names(user_file_sf)) {
      warning('No poly_id variable found in spatial file, using first variable found as id')
      user_file_sf$poly_id <- as.character(user_file_sf[[1]])
      shiny::showNotification(
        ui = shiny::tagList(
          shiny::h4(translate_app("poly_id_missing_title", lang()))
        ),
        action = shiny::tagList(
          translate_app("poly_id_missing_message", lang())
        ),
        duration = 15,
        type = "warning"
      )

    } else {
      # ensure polygon id is character (factors fuck it all)
      user_file_sf$poly_id <- as.character(user_file_sf$poly_id)
    }

    return(user_file_sf)

  })

  timeseries_data <- shiny::reactive({

    shiny::validate(
      shiny::need(data_reactives$var_daily, 'No variable selected')#,
      # shiny::need(data_reactives$display_daily, 'No display selected')
    )


    waiter_ts <- waiter::Waiter$new(
      id = 'overlay_div',
      html = shiny::tagList(
        hostess_ts$get_loader(),
        shiny::h3(translate_app("progress_ts", lang())),
        shiny::p(translate_app("progress_detail_ts", lang()))
      ),
      color = '#E8EAEB'
    )

    waiter_ts$show()
    # waiter_ts$update(
    #   html = shiny::tagList(
    #     hostess_ts$get_loader(
    #       svg = 'images/hostess_image.svg',
    #       progress_type = 'fill',
    #       fill_direction = 'btt'
    #     ),
    #     shiny::h3(translate_app("progress_ts", lang())),
    #     shiny::p(translate_app("progress_detail_ts", lang()))
    #   )
    # )
    hostess_ts$start()
    on.exit(hostess_ts$close(), add = TRUE)
    on.exit(waiter_ts$hide(), add = TRUE)

    var_daily <- data_reactives$var_daily
    display_daily <- data_reactives$display_daily

    # If we have markers, shapes or file
    if (display_daily != 'none') {
      # nfi plots (markers)
      if (display_daily == 'IFN plots') {
        sf_for_ts <- nfi_plots_sf_builder()
        title_for_ts <- glue::glue(
          "{map_reactives$map_daily_marker_click$id}",
          " [{round(map_reactives$map_daily_marker_click$lng, 3)} long,",
          " {round(map_reactives$map_daily_marker_click$lat, 3)} lat]"
        )
        df_for_ts <- catdroughtdb$get_current_time_series(
          sf_for_ts, var_daily
        )
        dygraph_for_ts <- df_for_ts %>%
          dplyr::select({{ var_daily }}) %>%
          xts::as.xts(order.by = df_for_ts$day) %>%
          dygraphs::dygraph(
            main = title_for_ts,
            ylab = glue::glue("{translate_app(var_daily, lang())}")
          ) %>%
          dygraphs::dySeries(
            var_daily, label = map_reactives$map_daily_marker_click$id,
            color = '#448714', strokeWidth = 2
          ) %>%
          dygraphs::dyAxis("x", drawGrid = FALSE) %>%
          dygraphs::dyHighlight(
            highlightCircleSize = 5,
            highlightSeriesBackgroundAlpha = 1,
            hideOnMouseOut = TRUE
          ) %>%
          dygraphs::dyLegend(
            show = "follow", labelsSeparateLines = TRUE
          ) %>%
          dygraphs::dyOptions(
            axisLineWidth = 1.5,
            # drawGrid = FALSE,
            axisLineColor = '#647a8d', axisLabelColor = '#647a8d',
            includeZero = TRUE, gridLineColor = '#647a8d'
          )
      } else {
        if (display_daily == 'file') {
          # file
          sf_for_ts <- file_sf_builder()

          # need rows in sf_for_ts
          shiny::validate(
            shiny::need(nrow(sf_for_ts) > 0, 'no rows in sf data')
          )

          title_for_ts <- translate_app("file", lang())
          df_for_ts <- catdroughtdb$get_current_time_series(
            sf_for_ts, var_daily
          )

          # if features are points, build the dygraph for points, if features are
          # polygons, build the dygraph for polygons
          if ('mean' %in% names(df_for_ts)) {
            # polygons
            temp_data <- df_for_ts %>%
              dplyr::mutate(
                low_es = mean - stddev,
                high_es = mean + stddev,
                low_es = dplyr::if_else(
                  low_es < 0 & var_daily != 'Psi', 0, low_es
                ),
                high_es = dplyr::if_else(
                  high_es > 0 & var_daily == 'Psi', 0, high_es
                )
              ) %>%
              dplyr::select(
                day, polygon_id, {{var_daily}} := mean, low_es, high_es
              ) %>%
              tidyr::pivot_wider(
                names_from = 'polygon_id',
                values_from = c(var_daily, 'low_es', 'high_es')
              )

            dygraph_for_ts <- temp_data %>%
              dplyr::select(-day) %>%
              xts::as.xts(order.by = temp_data$day) %>%
              dygraphs::dygraph(
                main = title_for_ts,
                ylab = glue::glue("{translate_app(var_daily, lang())}")
              ) %>%
              dygraphs::dyAxis("x", drawGrid = FALSE) %>%
              dygraphs::dyHighlight(
                highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 1,
                hideOnMouseOut = TRUE
              ) %>%
              dygraphs::dyLegend(
                show = "follow", labelsSeparateLines = TRUE
              ) %>%
              dygraphs::dyOptions(
                axisLineWidth = 1.5,
                # drawGrid = FALSE,
                axisLineColor = '#647a8d', axisLabelColor = '#647a8d',
                includeZero = TRUE, gridLineColor = '#647a8d'
              ) %>% {
                temp_dygraph <- .
                series_index <- 1:((ncol(temp_data) - 1)/3)
                for (serie in series_index) {
                  names_series <-
                    names(temp_data)[c(serie+1+length(series_index), serie+1, serie+1+(length(series_index)*2))]
                  temp_dygraph <- temp_dygraph %>%
                    dygraphs::dySeries(
                      names_series,
                      label = stringr::str_remove_all(
                        names(temp_data)[serie+1], '^[A-Za-z]+_'
                      )
                    )
                }
                temp_dygraph
              }

          } else {
            # points
            temp_data <- df_for_ts %>%
              tidyr::pivot_wider(
                names_from = 'point_id',
                values_from = var_daily
              )

            dygraph_for_ts <- temp_data %>%
              dplyr::select(-day) %>%
              xts::as.xts(order.by = temp_data$day) %>%
              dygraphs::dygraph(
                main = title_for_ts,
                ylab = glue::glue("{translate_app(var_daily, lang())}")
              ) %>%
              dygraphs::dyAxis("x", drawGrid = FALSE) %>%
              dygraphs::dyHighlight(
                highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 1,
                hideOnMouseOut = TRUE
              ) %>%
              dygraphs::dyLegend(
                show = "follow", labelsSeparateLines = TRUE
              ) %>%
              dygraphs::dyOptions(
                axisLineWidth = 1.5,
                # drawGrid = FALSE,
                axisLineColor = '#647a8d', axisLabelColor = '#647a8d',
                includeZero = TRUE, gridLineColor = '#647a8d'
              )
          }
        } else {
          # shapes
          sf_for_ts <- map_shape_sf_builder()

          # need rows in sf_for_ts
          shiny::validate(
            shiny::need(nrow(sf_for_ts) > 0, 'no rows in sf data')
          )

          title_for_ts <- glue::glue(
            "{map_reactives$map_daily_shape_click$id}"
          )
          df_for_ts <- catdroughtdb$get_current_time_series(
            sf_for_ts, var_daily
          )

          dygraph_for_ts <- df_for_ts %>%
            dplyr::mutate(
              low_es = mean - stddev,
              high_es = mean + stddev,
              low_es = dplyr::if_else(
                low_es < 0 & var_daily != 'Psi', 0, low_es
              ),
              high_es = dplyr::if_else(
                high_es > 0 & var_daily == 'Psi', 0, high_es
              )
            ) %>%
            dplyr::select(
              low_es, {{var_daily}} := mean, high_es
            ) %>%
            xts::as.xts(order.by = df_for_ts$day) %>%
            dygraphs::dygraph(
              main = title_for_ts,
              ylab = glue::glue("{translate_app(var_daily, lang())}")
            ) %>%
            dygraphs::dyAxis("x", drawGrid = FALSE) %>%
            dygraphs::dyHighlight(
              highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 1,
              hideOnMouseOut = TRUE
            ) %>%
            dygraphs::dyLegend(
              show = "follow", labelsSeparateLines = TRUE
            ) %>%
            dygraphs::dyOptions(
              axisLineWidth = 1.5,
              # drawGrid = FALSE,
              axisLineColor = '#647a8d', axisLabelColor = '#647a8d',
              includeZero = TRUE, gridLineColor = '#647a8d'
            ) %>%
            dygraphs::dySeries(
              c('low_es', var_daily, 'high_es'),
              label = map_reactives$map_daily_shape_click$id,
              color = '#448714', strokeWidth = 2
            )
        }
      }
    } else {

      # bare map clicks
      sf_for_ts <- map_click_sf_builder()
      title_for_ts <- glue::glue(
        "[{round(map_reactives$map_daily_click$lng, 3)} long,",
        " {round(map_reactives$map_daily_click$lat, 3)} lat]"
      )
      df_for_ts <- try(catdroughtdb$get_current_time_series(
        sf_for_ts, var_daily
      ))

      shiny::validate(
        shiny::need(!inherits(df_for_ts, 'try-error'), 'no data in coords')
      )

      dygraph_for_ts <- df_for_ts %>%
        dplyr::select({{ var_daily }}) %>%
        xts::as.xts(order.by = df_for_ts$day) %>%
        dygraphs::dygraph(
          main = title_for_ts,
          ylab = glue::glue("{translate_app(var_daily, lang())}")
        ) %>%
        dygraphs::dySeries(
          c(var_daily), label = 'Point',
          color = '#448714', strokeWidth = 2
        ) %>%
        dygraphs::dyAxis("x", drawGrid = FALSE) %>%
        dygraphs::dyHighlight(
          highlightCircleSize = 5,
          highlightSeriesBackgroundAlpha = 1,
          hideOnMouseOut = TRUE
        ) %>%
        dygraphs::dyLegend(
          show = "follow", labelsSeparateLines = TRUE
        ) %>%
        dygraphs::dyOptions(
          axisLineWidth = 1.5,
          # drawGrid = FALSE,
          axisLineColor = '#647a8d', axisLabelColor = '#647a8d',
          includeZero = TRUE, gridLineColor = '#647a8d'
        )
    }

    res <- list(
      data = df_for_ts,
      sf = sf_for_ts,
      dygraph = dygraph_for_ts
    )

    return(res)
  })

  ## reactives to return ####
  main_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    main_data_reactives$raster_selected_daily <- raster_selected_daily()
    main_data_reactives$timeseries_data <- timeseries_data()
  })
  return(main_data_reactives)

}
