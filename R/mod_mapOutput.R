#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#'
#' @export
mod_mapOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    mapdeck::mapdeckOutput(ns("map_daily"), height = 600),
    shiny::uiOutput(ns('map_container'))
  )
}

#' mod_map server function
#'
#' @details mod_map is in charge of setting the points/polygons (sf) and rasters
#'   in the leaflet projection.
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,main_data_reactives reactives
#' @param parent_session session object to change active tab
#' @param lang lang selected
#'
#' @importFrom terra has.RGB
#'
#' @export
#'
#' @rdname mod_mapOutput
mod_map <- function(
  input, output, session,
  data_reactives, main_data_reactives,
  parent_session, lang
) {

  # attachNamespace("terra")

  ## map output ####
  output$map_daily <- mapdeck::renderMapdeck({
    mapdeck::mapdeck(
      # style = mapdeck::mapdeck_style('dark'),
      # style = "mapbox://styles/mapbox/dark-v10",
      style = "https://raw.githubusercontent.com/CartoDB/basemap-styles/refs/heads/master/mapboxgl/dark-matter-nolabels.json",
      location = c(1.744, 41.726), zoom = 7, pitch = 0
    )
  })

  ## observers to update the map ####
  shiny::observe({
    var_daily <- shiny::req(data_reactives$var_daily)
    raster_daily <- shiny::req(main_data_reactives$raster_selected_daily) |>
      dplyr::filter(var == var_daily)
    display_daily <- shiny::req(data_reactives$display_daily)

    polys_data <- switch(
      display_daily,
      'none' = watersheds_polygons,
      'file' = shiny::req(main_data_reactives$timeseries_data$sf),
      'Watersheds' = watersheds_polygons,
      'Counties' = counties_polygons,
      'Municipalities' = municipalities_polygons
    )

    shiny::validate(
      shiny::need(all(sf::st_is(polys_data, c('MULTIPOLYGON', 'POLYGON'))), "No polygons supplied")
    )

    # custom legend (to be able to show in natural order, high values up)
    legend_js <- mapdeck::legend_element(
      variables = rev(round(seq(
        raster_daily[["min_value"]],
        raster_daily[["max_value"]],
        length.out = 5
      ), 3)),
      colours = scales::col_numeric(
        hcl.colors(10, "ag_GrnYl", alpha = 0.8),
        c(raster_daily[["min_value"]], raster_daily[["max_value"]]),
        na.color = "#FFFFFF00", reverse = TRUE, alpha = TRUE
      )(seq(
        raster_daily[["min_value"]],
        raster_daily[["max_value"]],
        length.out = 5
      )),
      colour_type = "fill", variable_type = "gradient",
      title = translate_app(var_daily, lang())
    ) |>
      mapdeck::mapdeck_legend()
    
    if (display_daily == "none") {
      mapdeck::mapdeck_update(map_id = session$ns("map_daily")) |>
        mapdeck::clear_bitmap(layer_id = "daily_raster") |>
        mapdeck::clear_polygon(layer_id = "daily_polys") |>
        mapdeck::add_polygon(
          data = watersheds_polygons, layer_id = "daily_polys",
          fill_opacity = 0,
          legend = legend_js,
          update_view = FALSE, focus_layer = FALSE
        ) |>
        mapdeck::add_bitmap(
          image = raster_daily$base64_string, layer_id = "daily_raster",
          bounds = c(
            raster_daily$left_ext, raster_daily$down_ext,
            raster_daily$right_ext, raster_daily$up_ext
          ),
          update_view = FALSE, focus_layer = FALSE,
          transparent_colour = "#00000000"
        )
    } else {
      mapdeck::mapdeck_update(map_id = session$ns("map_daily")) |>
        mapdeck::clear_bitmap(layer_id = "daily_raster") |>
        mapdeck::clear_polygon(layer_id = "daily_polys") |>
        mapdeck::add_bitmap(
          image = raster_daily$base64_string, layer_id = "daily_raster",
          bounds = c(
            raster_daily$left_ext, raster_daily$down_ext,
            raster_daily$right_ext, raster_daily$up_ext
          ),
          update_view = FALSE, focus_layer = FALSE,
          transparent_colour = "#00000000"
        ) |>
        mapdeck::add_polygon(
          data = polys_data, layer_id = "daily_polys",
          id = "poly_id", tooltip = "poly_id",
          fill_opacity = 0,
          stroke_colour = "#FF0000FF", stroke_width = 200,
          auto_highlight = TRUE, highlight_colour = "#FDF5EB80",
          legend = legend_js,
          update_view = FALSE, focus_layer = FALSE
        )
    }

  })

  ## reactives to return ####
  map_reactives <- shiny::reactiveValues()

  ## observers to change the active tab and update click reactives####
  shiny::observeEvent(
    eventExpr = input$map_daily_polygon_click,
    handlerExpr = {
      click_json <- shiny::req(input$map_daily_polygon_click) |>
        jsonlite::fromJSON()
      map_reactives$map_daily_shape_click <- list(
        id = click_json$object$properties$id,
        group = "daily_polys",
        lng = click_json$lon,
        lat = click_json$lat
      )
      if (!is.null(click_json$object$properties$id)) {
        map_reactives$map_daily_shape_id <- click_json$object$properties$id
      }
      # go to series
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset',
        selected = 'series_panel'
      )
    },
    priority = 1000
  )
  # shiny::observeEvent(
  #   eventExpr = input$map_daily_bitmap_click,
  #   handlerExpr = {
  #     click_json <- shiny::req(input$map_daily_bitmap_click) |>
  #       jsonlite::fromJSON()
  #     browser()
  #     map_reactives$map_daily_shape_click <- list(
  #       lng = input$map_daily_bitmap_click$lng,
  #       lat = input$map_daily_bitmap_click$lat,
  #       group = "daily_raster"
  #     )
  #     # go to series
  #     shiny::updateTabsetPanel(
  #       parent_session, 'main_panel_tabset',
  #       selected = 'series_panel'
  #     )
  #   },
  #   priority = 1000
  # )
  # shiny::observeEvent(
  #   eventExpr = input$map_daily_shape_click,
  #   handlerExpr = {
  #     # go to series
  #     shiny::updateTabsetPanel(
  #       parent_session, 'main_panel_tabset',
  #       selected = 'series_panel'
  #     )
  #   },
  #   priority = 1000
  # )
  # shiny::observeEvent(
  #   eventExpr = input$map_daily_click,
  #   handlerExpr = {
  #     # go to series
  #     shiny::updateTabsetPanel(
  #       parent_session, 'main_panel_tabset',
  #       selected = 'series_panel'
  #     )
  #   },
  #   priority = 1000
  # )
  # shiny::observeEvent(
  #   eventExpr = input$map_daily_marker_click,
  #   handlerExpr = {
  #     # go to series
  #     shiny::updateTabsetPanel(
  #       parent_session, 'main_panel_tabset',
  #       selected = 'series_panel'
  #     )
  #   },
  #   priority = 1000
  # )

  # shiny::observe({
  #   map_reactives$map_daily_shape_click <- input$map_daily_shape_click
  #   map_reactives$map_daily_marker_click <- input$map_daily_marker_click
  #   map_reactives$map_daily_click <- input$map_daily_click
  #   # map_reactives$map_daily_draw_all_features <-
  #   #   input$map_daily_draw_all_features
  # })
  return(map_reactives)
}
