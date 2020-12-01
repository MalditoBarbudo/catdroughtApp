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

  # data reactive with the raster ####
  raster_selected_daily <- shiny::reactive({

    shiny::validate(
      # shiny::need(data_reactives$var_daily, 'No variable selected'),
      shiny::need(data_reactives$date_daily, 'No date selected'),
      shiny::need(data_reactives$resolution_daily, 'No resolution selected')
    )

    # date
    date_sel <- as.character(data_reactives$date_daily)

    # resolution
    resolution_sel <- tolower(data_reactives$resolution_daily)

    # raster_res
    raster_res <- catdroughtdb$get_raster(date_sel, resolution_sel, 'raster')
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
  # TODO

  timeseries_data <- shiny::reactive({
    shiny::validate(
      shiny::need(data_reactives$var_daily, 'No variable selected'),
      shiny::need(data_reactives$display_daily, 'No display selected'),
      shiny::need(data_reactives$resolution_daily, 'No resolution selected')
    )

    var_daily <- data_reactives$var_daily
    display_daily <- data_reactives$display_daily
    resolution_daily <- tolower(data_reactives$resolution_daily)

    # If we have markers, shapes or file
    if (display_daily != 'none') {
      # nfi plots (markers)
      if (display_daily == 'IFN plots') {
        sf_for_ts <- nfi_plots_sf_builder()
        title_for_ts <- glue::glue(
          "{map_reactives$map_daily_marker_click$id}",
          " [{round(map_reactives$map_daily_marker_click$lng, 3)} lng,",
          " {round(map_reactives$map_daily_marker_click$lat, 3)} lat]"
        )
        df_for_ts <- catdroughtdb$get_current_time_series(
          sf_for_ts, var_daily, resolution_daily
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
          )
      } else {
        if (display_daily == 'file') {
          # file
          # TODO
        } else {
          # shapes
          sf_for_ts <- map_shape_sf_builder()
          title_for_ts <- glue::glue(
            "{map_reactives$map_daily_shape_click$id}"
          )
          df_for_ts <- catdroughtdb$get_current_time_series(
            sf_for_ts, var_daily, resolution_daily
          )
          dygraph_for_ts <- df_for_ts %>%
            dplyr::mutate(low = avg_pval - se_pval, high = avg_pval + se_pval) %>%
            dplyr::select(avg_pval, low, high) %>%
            xts::as.xts(order.by = df_for_ts$day) %>%
            dygraphs::dygraph(
              main = title_for_ts,
              ylab = glue::glue("{translate_app(var_daily, lang())}")
            ) %>%
            dygraphs::dySeries(
              c('low', 'avg_pval', 'high'),
              label = map_reactives$map_daily_shape_click$id,
              color = '#448714', strokeWidth = 2
            )
        }
      }
    } else {
      # bare map clicks
      sf_for_ts <- map_click_sf_builder()
      title_for_ts <- glue::glue(
        "[{round(map_reactives$map_daily_click$lng, 3)} lng,",
        " {round(map_reactives$map_daily_click$lat, 3)} lat]"
      )
      df_for_ts <- catdroughtdb$get_current_time_series(
        sf_for_ts, var_daily, resolution_daily
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
        )
    }

    res <- list(
      data = df_for_ts,
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
