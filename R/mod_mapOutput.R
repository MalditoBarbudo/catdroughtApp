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
#' @param parent_session session object to change active tab
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_mapOutput
mod_map <- function(
  input, output, session,
  data_reactives, main_data_reactives,
  parent_session, lang
) {

  ## map output ####
  output$map_daily <- leaflet::renderLeaflet({

    shiny::req(
      main_data_reactives$raster_selected_daily,
      data_reactives$var_daily
    )

    # shiny::validate(
    #   shiny::need(main_data_reactives$raster_selected_daily, 'no raster yet'),
    #   shiny::need(data_reactives$var_daily, 'no var yet')
    # )

    raster_daily <- main_data_reactives$raster_selected_daily
    var_daily <- data_reactives$var_daily

    leaflet_raster <- raster_daily[var_daily]

    palette <- leaflet::colorNumeric(
      palette = palettes_dictionary[[var_daily]][['pal']],
      # domain = c(
      #   palettes_dictionary[[var_daily]][['min']],
      #   palettes_dictionary[[var_daily]][['max']]
      # ),
      domain = leaflet_raster[[1]] |> as.numeric(),
      na.color = 'transparent',
      reverse = palettes_dictionary[[var_daily]][['rev']]
    )

    # legend palette
    legend_palette <- leaflet::colorNumeric(
      palette = palettes_dictionary[[var_daily]][['pal']],
      # domain = c(
      #   palettes_dictionary[[var_daily]][['min']],
      #   palettes_dictionary[[var_daily]][['max']]
      # ),
      domain = leaflet_raster[[1]] |> as.numeric(),
      na.color = 'transparent',
      reverse = !palettes_dictionary[[var_daily]][['rev']]
    )

    leaflet::leaflet() |>
      leaflet::setView(1.744, 41.726, zoom = 8) |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldShadedRelief,
        group = translate_app('Relief', lang()),
        # avoid raster disappearing when base tiles change
        options = leaflet::providerTileOptions(zIndex = -10)
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        group = translate_app('Imagery', lang()),
        # avoid raster disappearing when base tiles change
        options = leaflet::providerTileOptions(zIndex = -10)
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$OpenStreetMap,
        group = translate_app('OSM', lang()),
        # avoid raster disappearing when base tiles change
        options = leaflet::providerTileOptions(zIndex = -10)
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldGrayCanvas,
        group = translate_app('WorldGrayCanvas', lang()),
        # avoid raster disappearing when base tiles change
        options = leaflet::providerTileOptions(zIndex = -10)
      ) |>
      leaflet::addProviderTiles(
        leaflet::providers$CartoDB.PositronNoLabels,
        group = translate_app('PositronNoLabels', lang()),
        # avoid raster disappearing when base tiles change
        options = leaflet::providerTileOptions(zIndex = -10)
      ) |>
      leaflet::addLayersControl(
        baseGroups = c(
          translate_app('Relief', lang()),
          translate_app('Imagery', lang()),
          translate_app('OSM', lang()),
          translate_app('WorldGrayCanvas', lang()),
          translate_app('PositronNoLabels', lang())
        ),
        overlayGroups = c('raster'),
        options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
      ) |>
      leaflet::addRasterImage(
        terra::rast(leaflet_raster), project = FALSE, group = 'raster',
        colors = palette, opacity = 1
      ) |>
      leaflet::addLegend(
        pal = legend_palette, values = leaflet_raster[[1]] |> as.numeric(),
        title = translate_app(var_daily, lang()),
        position = 'bottomright', opacity = 1,
        labFormat = leaflet::labelFormat(
          transform = function(x) {sort(x, decreasing = TRUE)}
        )
      )
  })

  ## observers to update the map ####
  # draw polygons observer
  shiny::observe({

    shiny::validate(
      shiny::need(data_reactives$display_daily, 'no polygon/plots selected'),
      shiny::need(data_reactives$var_daily, 'no var selected')
    )

    display_daily <- data_reactives$display_daily
    var_daily <- data_reactives$var_daily

    if (display_daily == 'none') {
      leaflet::leafletProxy('map_daily') |>
        leaflet::clearGroup('display_daily')
      return()
    }

    # if plots do markers, if polys do polygons
    if (display_daily == 'IFN plots') {
      leaflet::leafletProxy('map_daily') |>
        leaflet::clearGroup('display_daily') |>
        leaflet::addCircleMarkers(
          data = nfi4_plots,
          group = 'display_daily',
          label = ~plot_id,
          layerId = ~plot_id,
          clusterOptions = leaflet::markerClusterOptions()
        )
    } else {
      if (display_daily == 'file') {
        shiny::validate(
          shiny::need(data_reactives$user_file_sel, 'no file uploaded yet'),
          shiny::need(main_data_reactives$timeseries_data$sf, 'No sf yet')
        )

        file_data <- main_data_reactives$timeseries_data$sf

        # if file is polygons we need to draw polygons, if file are points
        # we need to draw markers
        if (all(sf::st_is(file_data, c('MULTIPOLYGON', 'POLYGON')))) {
          leaflet::leafletProxy('map_daily') |>
            leaflet::clearGroup('display_daily') |>
            leaflet::addPolygons(
              data = file_data,
              group = 'display_daily',
              label = ~file_data |> dplyr::pull(1),
              layerId = ~file_data |> dplyr::pull(1),
              weight = 2, smoothFactor = 1,
              opacity = 1.0, fill = TRUE, fillOpacity = 0,
              color = 'black',
              highlightOptions = leaflet::highlightOptions(
                color = "#CF000F", weight = 2,
                bringToFront = TRUE,
                fill = TRUE, fillOpacity = 0
              )
            )
        }

        if (all(sf::st_is(file_data, c('MULTIPOINT', 'POINT')))) {
          leaflet::leafletProxy('map_daily') |>
            leaflet::clearGroup('display_daily') |>
            leaflet::addCircleMarkers(
              data = file_data,
              group = 'display_daily',
              label = ~file_data |> dplyr::pull(1),
              layerId = ~file_data |> dplyr::pull(1)
            )
        }
      } else {
        polygon_object_name <- glue::glue("{tolower(display_daily)}_polygons")

        leaflet::leafletProxy('map_daily') |>
          leaflet::clearGroup('display_daily') |>
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

  ## observers to change the active tab ####
  shiny::observeEvent(
    eventExpr = input$map_daily_shape_click,
    handlerExpr = {
      # go to series
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset',
        selected = 'series_panel'
      )
    },
    priority = 1000
  )
  shiny::observeEvent(
    eventExpr = input$map_daily_click,
    handlerExpr = {
      # go to series
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset',
        selected = 'series_panel'
      )
    },
    priority = 1000
  )
  shiny::observeEvent(
    eventExpr = input$map_daily_marker_click,
    handlerExpr = {
      # go to series
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset',
        selected = 'series_panel'
      )
    },
    priority = 1000
  )

  ## reactives to return ####
  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    map_reactives$map_daily_shape_click <- input$map_daily_shape_click
    map_reactives$map_daily_marker_click <- input$map_daily_marker_click
    map_reactives$map_daily_click <- input$map_daily_click
    # map_reactives$map_daily_draw_all_features <-
    #   input$map_daily_draw_all_features
  })
  return(map_reactives)

}
