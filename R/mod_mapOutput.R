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
    leaflet::leafletOutput(ns("map_daily"), height = 600),
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
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_mapOutput
mod_map <- function(
  input, output, session,
  data_reactives, main_data_reactives,
  lang
) {

  ## map output ####
  output$map_daily <- leaflet::renderLeaflet({

    shiny::validate(
      shiny::need(main_data_reactives$raster_selected_daily, 'no raster yet'),
      shiny::need(data_reactives$var_daily, 'no var yet')
    )

    raster_daily <- main_data_reactives$raster_selected_daily
    var_daily_sel <- data_reactives$var_daily

    leaflet_raster <- raster_daily[[var_daily_sel]]

    palette <- leaflet::colorNumeric(
      palette = palettes_dictionary[[var_daily_sel]][['pal']],
      # domain = c(
      #   palettes_dictionary[[var_daily_sel]][['min']],
      #   palettes_dictionary[[var_daily_sel]][['max']]
      # ),
      domain = raster::values(leaflet_raster),
      na.color = 'transparent',
      reverse = palettes_dictionary[[var_daily_sel]][['rev']]
    )

    leaflet::leaflet() %>%
      leaflet::setView(1.744, 41.726, zoom = 8) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldShadedRelief,
        group = 'Relief'
      ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        group = 'Imagery'
      ) %>%
      leaflet::addLayersControl(
        baseGroups = c('Relief', 'Imagery'),
        overlayGroups = c('raster'),
        options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
      ) %>%
      leaflet::addRasterImage(
        leaflet_raster, project = FALSE, group = 'raster',
        colors = palette, opacity = 1
      ) %>%
      leaflet::addLegend(
        pal = palette, values = raster::values(leaflet_raster),
        title = translate_app(input$var_daily, lang()),
        position = 'bottomright',
        opacity = 1
      )
  })

  ## observers to update the map ####
  # draw polygons observer
  shiny::observe({

    shiny::validate(
      shiny::need(data_reactives$display_daily, 'no polygon/plots selected'),
      shiny::need(data_reactives$var_daily, 'no var selected'),
      shiny::need(data_reactives$resolution_daily, 'no res selected')
    )

    display_daily <- data_reactives$display_daily
    var_daily <- data_reactives$var_daily

    if (display_daily == 'none') {
      leaflet::leafletProxy('map_daily') %>%
        leaflet::clearGroup('display_daily')
      return()
    }

    # if plots do markers, if polys do polygons
    if (display_daily == 'IFN plots') {
      leaflet::leafletProxy('map_daily') %>%
        leaflet::clearGroup('display_daily') %>%
        leaflet::addCircleMarkers(
          data = nfi4_plots,
          group = 'display_daily',
          label = ~plot_id,
          layerId = ~plot_id,
          clusterOptions = leaflet::markerClusterOptions()
        )
    } else {
      if (display_daily == 'file') {
        # TODO
      } else {
        polygon_object_name <- glue::glue("{tolower(display_daily)}_polygons")

        leaflet::leafletProxy('map_daily') %>%
          leaflet::clearGroup('display_daily') %>%
          leaflet::addPolygons(
            data = rlang::eval_tidy(rlang::sym(polygon_object_name)),
            group = 'display_daily',
            label = ~poly_id, layerId = ~poly_id,
            weight = 1, smoothFactor = 1,
            opacity = 1.0, fill = TRUE, fillOpacity = 0,
            color = '#6C7A89FF',
            highlightOptions = leaflet::highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = TRUE,
              fill = TRUE, fillOpacity = 0
            )
          )
      }
    }

  })

  ## reactives to return ####
  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    map_reactives$daily_map_shape_click <- input$daily_map_shape_click
    map_reactives$daily_map_marker_click <- input$daily_map_marker_click
    map_reactives$daily_map_click <- input$daily_map_click
    # map_reactives$daily_map_draw_all_features <-
    #   input$daily_map_draw_all_features
  })
  return(map_reactives)

}
