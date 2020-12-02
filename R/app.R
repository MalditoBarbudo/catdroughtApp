#' function to launch the catdrought app
#'
#' @importFrom magrittr %>%
#'
#' @export
catdrought_app <- function() {

  ### DB access ################################################################
  catdroughtdb <- lfcdata::catdrought()

  ## JS code needed ############################################################
  keep_alive_script <- shiny::HTML(
    "var socket_timeout_interval;
var n = 0;

$(document).on('shiny:connected', function(event) {
  socket_timeout_interval = setInterval(function() {
    Shiny.onInputChange('alive_count', n++)
  }, 10000);
});

$(document).on('shiny:disconnected', function(event) {
  clearInterval(socket_timeout_interval)
});"
  )



  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'catdroughtApp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )

  ## UI ####
  ui <- shiny::tagList(
    # shinyjs
    shinyjs::useShinyjs(),

    # css
    shiny::tags$head(
      # js script,
      shiny::tags$script(keep_alive_script),
      # custom css
      shiny::includeCSS(
        system.file('resources', 'catdrought.css', package = 'catdroughtApp')
      ),
      # corporative image css
      shiny::includeCSS(
        system.file('resources', 'corp_image.css', package = 'catdroughtApp')
      )
    ),

    navbarPageWithInputs(
      # opts
      title = 'CatDrought App',
      id = 'nav', collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # navbarPage contents
      shiny::tabPanel(
        title = mod_tab_translateOutput('main_tab_translation'),

        shiny::sidebarLayout(
          ## options
          position = 'left', fluid = TRUE,
          sidebarPanel = shiny::sidebarPanel(
            width = 3,
            # this is gonna be a tabsetPanel, for data selection, save and help.
            # tabset panel
            shiny::tabsetPanel(
              id = 'sidebar_tabset', type = 'pills',
              # data panel
              shiny::tabPanel(
                title = mod_tab_translateOutput('data_translation'),
                value = 'data_inputs_panel',
                mod_dataInput('mod_dataInput')
              ), # end of data panel
              # save panel
              shiny::tabPanel(
                title = mod_tab_translateOutput('save_translation'),
                value = 'save_panel',
                mod_saveOutput('mod_saveOutput')
              )
            )
          ), # end of sidebarPanel
          mainPanel = shiny::mainPanel(
            width = 9,
            shiny::tabsetPanel(
              id = 'main_panel_tabset', type = 'pills',
              shiny::tabPanel(
                title = mod_tab_translateOutput('map_translation'),
                # 'map',
                value = 'map_panel',
                mod_mapOutput('mod_mapOutput')
              ),
              shiny::tabPanel(
                title = mod_tab_translateOutput('series_tab_translation'),
                value = 'series_panel',
                mod_tsOutput('mod_tsOutput')
              )
            )
          )
        )
      )

      # # navbarPage contents
      # shiny::tabPanel(
      #   title = shiny::uiOutput("actual_tab"),
      #   # we need to create the ui in the server to catch the language input
      #   # and redraw all the inputs and texts in the selected lang
      #   shiny::uiOutput('current_ui')
      # ) # end of current tab
    ) # end of navbar
  ) # end of UI

  ## SERVER ####
  server <- function(input, output, session) {
    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   input$map_daily_marker_click
    # })
    # output$debug2 <- shiny::renderPrint({
    #   map_reactives$map_click
    # })
    # output$debug3 <- shiny::renderPrint({
    #   map_reactives$map_shape_click
    # })

    # lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    # modules ####
    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput', lang
    )
    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapOutput',
      data_reactives, main_data_reactives,
      session, lang
    )
    # main data
    main_data_reactives <- shiny::callModule(
      mod_mainData, 'mod_mainDataOutput',
      data_reactives, map_reactives,
      catdroughtdb, lang
    )
    # ts
    timseries_reactives <- shiny::callModule(
      mod_ts, 'mod_tsOutput',
      data_reactives, main_data_reactives,
      lang
    )
    # save
    shiny::callModule(
      mod_save, 'mod_saveOutput',
      main_data_reactives, data_reactives,
      lang
    )

    ## tab translations ####
    shiny::callModule(
      mod_tab_translate, 'main_tab_translation',
      'main_tab_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'data_translation',
      'data_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'map_translation',
      'map_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'series_tab_translation',
      'series_tab_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'save_translation',
      'save_translation', lang
    )


    # output$actual_tab <- renderText({
    #   translate_app('actual_tab_title', lang())
    # })
    #
    # ## proper UI ####
    # output$current_ui <- shiny::renderUI({
    #
    #   lang_declared <- lang()
    #   dates_lang <- switch(
    #     lang_declared,
    #     'cat' = 'ca',
    #     'spa' = 'es',
    #     'eng' = 'en'
    #   )
    #
    #   # browser()
    #
    #   ## choices
    #
    #   date_daily_choices <- seq(
    #     lubridate::ymd(Sys.Date() - 366), lubridate::ymd(Sys.Date() - 1),
    #     by = 'days'
    #   )
    #
    #   # variable names
    #   # climate: 'PET'
    #   # fwb: 'LAI', 'Eplant', 'Esoil',  'Runoff', 'DeepDrainage'
    #   # soil_moisture: 'Psi', 'REW', 'Theta', 'Infiltration',
    #   # drought_stress: 'DDS'
    #   climate_vars <- c("PET") %>%
    #     magrittr::set_names(translate_app(., lang_declared))
    #   fwb_vars <- c('LAI', 'Eplant', 'Esoil',  'Runoff', 'DeepDrainage') %>%
    #     magrittr::set_names(translate_app(., lang_declared))
    #   soil_moisture_vars <- c("REW", "Theta", "Psi", "Infiltration") %>%
    #     magrittr::set_names(translate_app(., lang_declared))
    #   drought_stress_vars <- c("DDS") %>%
    #     magrittr::set_names(translate_app(., lang_declared))
    #
    #   shiny::sidebarLayout(
    #     # sidebar
    #     ## options
    #     position = 'left', fluid = TRUE,
    #     sidebarPanel = shiny::sidebarPanel(
    #       width = 3,
    #       # tabset panel
    #       shiny::tabsetPanel(
    #         id = 'sidebar_tabset', type = 'pills',
    #         # data panel
    #         shiny::tabPanel(
    #           title = mod_tab_translateOutput('data_translation'),
    #           value = 'data_inputs_panel',
    #           mod_dataInput('mod_dataInput')
    #         )
    #
    #
    #       ),
    #
    #       ## variable selectors ####
    #       # var sel
    #       shiny::selectInput(
    #         'var_daily', translate_app('var_daily_label', lang_declared),
    #         choices = list(
    #           'Soil moisture' = soil_moisture_vars,
    #           'Water balance' = fwb_vars,
    #           'Climate' = climate_vars,
    #           'Drought stress' = drought_stress_vars
    #         ) %>% magrittr::set_names(translate_app(names(.), lang_declared))
    #       ),
    #
    #       # date sel
    #       shiny::dateInput(
    #         'date_daily', translate_app('date_daily_label', lang_declared),
    #         value = lubridate::ymd(Sys.Date() - 1),
    #         min = date_daily_choices[1],
    #         max = date_daily_choices[length(date_daily_choices)],
    #         weekstart = 1, language = dates_lang
    #       ),
    #       # polygon sel
    #       shiny::selectInput(
    #         'display_daily', translate_app('display_daily_label', lang_declared),
    #         choices = c('none', "Watersheds", "Counties", "Municipalities", "IFN plots") %>%
    #           magrittr::set_names(translate_app(., lang_declared)),
    #         selected = 'none'
    #       ),
    #       # resoltion sel
    #       shiny::radioButtons(
    #         'resolution_daily', translate_app('resolution_daily_label', lang_declared),
    #         choices = c('Smoothed', '1km', '200m') %>%
    #           magrittr::set_names(translate_app(., lang_declared)),
    #         selected = 'Smoothed'
    #       ),
    #
    #       # download button
    #       shiny::actionButton(
    #         'download_raster_daily', translate_app('download_raster_label', lang_declared)
    #       )
    #     ), # end of sidebar
    #     # main panel
    #     shiny::mainPanel(
    #
    #       ########################################################### debug ####
    #       # shiny::absolutePanel(
    #       #   id = 'debug', class = 'panel panel-default', fixed = TRUE,
    #       #   draggable = TRUE, width = 640, height = 'auto',
    #       #   # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
    #       #   # top = 'auto', left = 'auto', right = 100, bottom = 100,
    #       #   top = 60, left = 'auto', right = 50, bottom = 'auto',
    #       #
    #       #   shiny::textOutput('debug1'),
    #       #   shiny::textOutput('debug2'),
    #       #   shiny::textOutput('debug3')
    #       # ),
    #       ####################################################### end debug ####
    #
    #       shiny::tabsetPanel(
    #         id = 'daily_main_panel',
    #
    #         # map
    #         shiny::tabPanel(
    #           title = translate_app('map_tab_label', lang_declared),
    #           leaflet::leafletOutput('map_daily', height = 600) %>%
    #             shinyWidgets::addSpinner(spin = 'cube', color = '#26a65b')
    #         ),
    #
    #         # series
    #         shiny::tabPanel(
    #           title = translate_app('series_tab_label', lang_declared),
    #           value = 'series',
    #           dygraphs::dygraphOutput('trends_daily') %>%
    #             shinyWidgets::addSpinner(spin = 'cube', color = '#26a65b'),
    #           shiny::br(),
    #           shiny::column(
    #             2, offset = 10,
    #             shiny::downloadButton(
    #               'download_series_daily',
    #               translate_app('download_series_label', lang_declared)
    #             )
    #           )
    #         )
    #       )
    #     ) # end of main panel
    #   )
    # })
    #
    # # data reactive with the raster ####
    # raster_selected_daily <- shiny::reactive({
    #
    #   shiny::validate(
    #     # shiny::need(input$var_daily, 'No variable selected'),
    #     shiny::need(input$date_daily, 'No date selected'),
    #     shiny::need(input$resolution_daily, 'No resolution selected')
    #   )
    #
    #   # date
    #   date_sel <- as.character(input$date_daily)
    #
    #   # resolution
    #   resolution_sel <- tolower(input$resolution_daily)
    #
    #   # raster_res
    #   raster_res <- catdroughtdb$get_raster(date_sel, resolution_sel, 'raster')
    #   return(raster_res)
    # })
    #
    # ## map output ####
    # output$map_daily <- leaflet::renderLeaflet({
    #
    #   raster_daily <- raster_selected_daily()
    #   var_daily_sel <- input$var_daily
    #
    #   leaflet_raster <- raster_daily[[var_daily_sel]]
    #
    #   palette <- leaflet::colorNumeric(
    #     palette = palettes_dictionary[[var_daily_sel]][['pal']],
    #     # domain = c(
    #     #   palettes_dictionary[[var_daily_sel]][['min']],
    #     #   palettes_dictionary[[var_daily_sel]][['max']]
    #     # ),
    #     domain = raster::values(leaflet_raster),
    #     na.color = 'transparent',
    #     reverse = palettes_dictionary[[var_daily_sel]][['rev']]
    #   )
    #
    #   # browser()
    #
    #   leaflet::leaflet() %>%
    #     leaflet::setView(1.744, 41.726, zoom = 8) %>%
    #     leaflet::addProviderTiles(
    #       leaflet::providers$Esri.WorldShadedRelief,
    #       group = 'Relief'
    #     ) %>%
    #     leaflet::addProviderTiles(
    #       leaflet::providers$Esri.WorldImagery,
    #       group = 'Imagery'
    #     ) %>%
    #     leaflet::addLayersControl(
    #       baseGroups = c('Relief', 'Imagery'),
    #       overlayGroups = c('raster'),
    #       options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
    #     ) %>%
    #     leaflet::addRasterImage(
    #       leaflet_raster, project = FALSE, group = 'raster',
    #       colors = palette, opacity = 1
    #     ) %>%
    #     leaflet::addLegend(
    #       pal = palette, values = raster::values(leaflet_raster),
    #       title = translate_app(input$var_daily, lang()),
    #       position = 'bottomright',
    #       opacity = 1
    #     )
    # })
    #
    # ## series polygons observers ####
    #
    # # draw polygons observer
    # shiny::observe({
    #
    #   shiny::validate(
    #     shiny::need(input$display_daily, 'no polygon/plots selected'),
    #     shiny::need(input$var_daily, 'no var selected'),
    #     shiny::need(input$resolution_daily, 'no res selected')
    #   )
    #
    #   var_dummy <- input$var_daily
    #
    #   if (input$display_daily == 'none') {
    #     leaflet::leafletProxy('map_daily') %>%
    #       leaflet::clearGroup('display_daily')
    #     return()
    #   }
    #
    #   # if plots do markers, if polys do polygons
    #   if (input$display_daily == 'IFN plots') {
    #     leaflet::leafletProxy('map_daily') %>%
    #       leaflet::clearGroup('display_daily') %>%
    #       leaflet::addCircleMarkers(
    #         data = nfi4_plots,
    #         group = 'display_daily',
    #         label = ~plot_id,
    #         layerId = ~plot_id,
    #         clusterOptions = leaflet::markerClusterOptions()
    #       )
    #   } else {
    #
    #     polygon_object_name <- glue::glue("{tolower(input$display_daily)}_polygons")
    #
    #     leaflet::leafletProxy('map_daily') %>%
    #       leaflet::clearGroup('display_daily') %>%
    #       leaflet::addPolygons(
    #         data = rlang::eval_tidy(rlang::sym(polygon_object_name)),
    #         group = 'display_daily',
    #         label = ~poly_id, layerId = ~poly_id,
    #         weight = 1, smoothFactor = 1,
    #         opacity = 1.0, fill = TRUE, fillOpacity = 0,
    #         color = '#6C7A89FF',
    #         highlightOptions = leaflet::highlightOptions(
    #           color = "#CF000F", weight = 2,
    #           bringToFront = TRUE,
    #           fill = TRUE, fillOpacity = 0
    #         )
    #       )
    #   }
    #
    # })
    #
    # # clicked polygon reactive. returns the polygon selected
    # clicked_poly <- shiny::reactive({
    #
    #   shiny::validate(
    #     shiny::need(map_reactives$map_daily_shape_click, 'no click on map')
    #   )
    #
    #   clicked_poly <- input$map_daily_shape_click
    #   polygon_object <- rlang::eval_tidy(
    #     rlang::sym(glue::glue("{tolower(input$display_daily)}_polygons"))
    #   ) %>%
    #     dplyr::filter(
    #       poly_id == clicked_poly$id
    #     )
    #
    #   return(polygon_object)
    # })
    #
    # # time series data
    # series_data_for_polys <- shiny::reactive({
    #
    #   # polygon
    #   polygon_sel <- clicked_poly()
    #
    #   # band
    #   var_sel <- input$var_daily
    #
    #   # resolution
    #   resolution_sel <- tolower(input$resolution_daily)
    #
    #   # res
    #   res <- catdroughtdb$get_current_time_series(
    #     polygon_sel, var_sel, resolution_sel
    #   )
    #   return(res)
    #
    # })
    #
    # ## series pixel observers ####
    # series_data_for_pixel <- shiny::reactive({
    #
    #   shiny::validate(
    #     shiny::need(input$map_daily_click, 'no map click')
    #     # shiny::need(input$display_daily == 'none', 'polygons_selected'),
    #     # shiny::need(input$var_daily, 'no var selected'),
    #     # shiny::need(input$resolution_daily, 'no resolution selected')
    #   )
    #
    #   clicked_pixel <- input$map_daily_click
    #   point_sel <- tibble::tibble(
    #     point_id = 'clicked_coords',
    #     long = clicked_pixel$lng,
    #     lat = clicked_pixel$lat
    #   ) %>%
    #     sf::st_as_sf(
    #       coords = c('long', 'lat'),
    #       crs = 4326
    #     )
    #
    #   # band
    #   var_sel <- input$var_daily
    #
    #   # resolution
    #   resolution_sel <- tolower(input$resolution_daily)
    #
    #   # res
    #   res <- catdroughtdb$get_current_time_series(
    #     point_sel, var_sel, resolution_sel
    #   )
    #   return(res)
    #
    # })
    #
    # ## series markers observers ####
    # series_data_for_markers <- shiny::reactive({
    #
    #   shiny::validate(
    #     shiny::need(input$map_daily_click, 'no map click')
    #     # shiny::need(input$display_daily == 'none', 'polygons_selected'),
    #     # shiny::need(input$var_daily, 'no var selected'),
    #     # shiny::need(input$resolution_daily, 'no resolution selected')
    #   )
    #
    #   clicked_marker <- input$map_daily_marker_click
    #
    #   point_sel <- tibble::tibble(
    #     point_id = clicked_marker$id,
    #     long = clicked_marker$lng,
    #     lat = clicked_marker$lat
    #   ) %>%
    #     sf::st_as_sf(
    #       coords = c('long', 'lat'),
    #       crs = 4326
    #     )
    #
    #   # band
    #   var_sel <- input$var_daily
    #
    #   # resolution
    #   resolution_sel <- tolower(input$resolution_daily)
    #
    #   # res
    #   res <- catdroughtdb$get_current_time_series(
    #     point_sel, var_sel, resolution_sel
    #   )
    #   return(res)
    #
    # })
    #
    #
    # ## series output ####
    # output$trends_daily <- dygraphs::renderDygraph({
    #
    #   # variable
    #   var_id <- input$var_daily
    #
    #   # here we have to check if marker or poly or pixel
    #   if (input$display_daily != 'none') {
    #
    #     if (input$display_daily == 'IFN plots') {
    #       # click
    #       clicked_marker <- input$map_daily_marker_click
    #
    #       # data and plot
    #       plot_data <- series_data_for_markers()
    #       res <- plot_data %>%
    #         dplyr::select({{ var_id }}) %>%
    #         xts::as.xts(order.by = plot_data$day) %>%
    #         dygraphs::dygraph(
    #           main = glue::glue("{translate_app(var_id, lang())} - {glue::glue(translate_app('daily_trends_ifn_title', lang()))}"),
    #           ylab = glue::glue("{translate_app(var_id, lang())}")
    #         ) %>%
    #         dygraphs::dySeries(
    #           var_id, label = clicked_marker$id,
    #           color = '#448714', strokeWidth = 2
    #         )
    #     } else {
    #       # polygon id
    #       poly_id <- input$map_daily_shape_click$id
    #
    #       # data and plot
    #       plot_data <- series_data_for_polys()
    #       res <- plot_data %>%
    #         dplyr::mutate(low = avg_pval - se_pval, high = avg_pval + se_pval) %>%
    #         dplyr::select(avg_pval, low, high) %>%
    #         xts::as.xts(order.by = plot_data$day) %>%
    #         dygraphs::dygraph(
    #           main = glue::glue("{translate_app(var_id, lang())} - {poly_id}"),
    #           ylab = glue::glue("{translate_app(var_id, lang())}")
    #         ) %>%
    #         dygraphs::dySeries(
    #           c('low', 'avg_pval', 'high'), label = poly_id,
    #           color = '#448714', strokeWidth = 2
    #         )
    #     }
    #
    #   } else {
    #     # click
    #     clicked_pixel <- input$map_daily_click
    #
    #     # data and plot
    #     plot_data <- series_data_for_pixel()
    #     res <- plot_data %>%
    #       dplyr::select({{ var_id }}) %>%
    #       xts::as.xts(order.by = plot_data$day) %>%
    #       dygraphs::dygraph(
    #         main = glue::glue("{translate_app(var_id, lang())} - {glue::glue(translate_app('daily_trends_other_title', lang()))}"),
    #         ylab = glue::glue("{translate_app(var_id, lang())}")
    #       ) %>%
    #       dygraphs::dySeries(
    #         c(var_id), label = 'Pixel',
    #         color = '#448714', strokeWidth = 2
    #       )
    #   }
    #
    #   res <- res %>%
    #     dygraphs::dyAxis(
    #       name = 'y',
    #       valueRange = c(
    #         palettes_dictionary[[input$var_daily]][['min']],
    #         palettes_dictionary[[input$var_daily]][['max']]
    #       )
    #     )
    #
    #   return(res)
    #
    # })
    #
    # ## observers to change the active tab
    # shiny::observeEvent(
    #   eventExpr = input$map_daily_shape_click,
    #   handlerExpr = {
    #     # go to series
    #     shiny::updateTabsetPanel(
    #       session, 'daily_main_panel', selected = 'series'
    #     )
    #   }
    # )
    #
    # shiny::observeEvent(
    #   eventExpr = input$map_daily_click,
    #   handlerExpr = {
    #     # go to series only if none selected in display_daily
    #     if (input$display_daily == 'none') {
    #       shiny::updateTabsetPanel(
    #         session, 'daily_main_panel', selected = 'series'
    #       )
    #     }
    #   }
    # )
    #
    # shiny::observeEvent(
    #   eventExpr = input$map_daily_marker_click,
    #   handlerExpr = {
    #     shiny::updateTabsetPanel(
    #       session, 'daily_main_panel', selected = 'series'
    #     )
    #   }
    # )
    #
    # # modal to show a warning about time consuming steps
    # shiny::observe({
    #
    #   # dirty check of any click in the map to show the modal
    #   shiny::validate(
    #     if (is.null(shiny::need(input$map_daily_shape_click, 'no shape click')) ||
    #         is.null(shiny::need(input$map_daily_click, 'no map click')) ||
    #         is.null(shiny::need(input$map_daily_marker_click, 'no marker click'))) {
    #       NULL
    #     } else {FALSE}
    #   )
    #
    #   # modal waiting time
    #   shiny::showModal(
    #     ui = shiny::modalDialog(
    #       shiny::tagList(
    #         shiny::fluidRow(
    #           shiny::column(
    #             12,
    #             shiny::p(
    #               translate_app('modal_waiting_p', lang())
    #             )
    #           )
    #         )
    #       ),
    #       easyClose = TRUE,
    #       footer = shiny::tagList(
    #         # shiny::modalButton(translate_app('modal_dismiss_label', lang_declared)),
    #         shiny::modalButton(translate_app('dismiss_btn', lang()))
    #       )
    #     )
    #   )
    # })
    #
    # ## download handlers ####
    # # modal for saving the raster data
    # shiny::observeEvent(
    #   eventExpr = input$download_raster_daily,
    #   handlerExpr = {
    #
    #     lang_declared = lang()
    #
    #     shiny::showModal(
    #       ui = shiny::modalDialog(
    #         shiny::tagList(
    #
    #           shiny::fluidRow(
    #             shiny::column(
    #               12,
    #               # format options
    #               shiny::selectInput(
    #                 'data_format',
    #                 translate_app('download_raster_format', lang_declared),
    #                 choices = list(
    #                   'GIS' = c('gtiff', 'gpkg') %>%
    #                     magrittr::set_names(translate_app(., lang_declared))
    #                 ),
    #                 selected = 'gtiff'
    #               )
    #               # length options
    #               # shiny::radioButtons(
    #               #   'data_length', translate_app('data_length_label', lang_declared),
    #               #   # text_translate('data_length', lang(), texts_thes),
    #               #   choices = c('visible', 'all_columns') %>%
    #               #     magrittr::set_names(translate_app(., lang_declared)),
    #               #   selected = 'visible', width = '100%'
    #               # )
    #             )
    #           )
    #         ),
    #         easyClose = TRUE,
    #         footer = shiny::tagList(
    #           # shiny::modalButton(translate_app('modal_dismiss_label', lang_declared)),
    #           shiny::modalButton(translate_app('dismiss_btn', lang())),
    #           shiny::downloadButton(
    #             'download_data_with_options',
    #             label = translate_app('download_raster_label', lang()),
    #             class = 'btn-success'
    #           )
    #         )
    #       )
    #     )
    #   }
    # )
    #
    # # download logic for series data
    # output$download_series_daily <- shiny::downloadHandler(
    #   filename = function() {
    #     file_name <- glue::glue(
    #       'catdrought_series_for_{input$map_daily_shape_click$id}.csv'
    #     )
    #   },
    #   content = function(file) {
    #     series_data_for_polys() %>%
    #       readr::write_csv(path = file)
    #   }
    # )
    #
    #
    # output$download_data_with_options <- shiny::downloadHandler(
    #   filename = function() {
    #
    #     file_name <- switch(
    #       input$data_format,
    #       'gtiff' = 'catdrought_data.tif',
    #       'gpkg' = 'catdrought_data.gpkg'
    #     )
    #
    #     return(file_name)
    #   },
    #   content = function(file) {
    #
    #     # data length
    #     result_data <- raster_selected_daily()
    #
    #     # data format
    #
    #     # shapefile
    #     if (input$data_format == 'gtiff') {
    #       raster::writeRaster(
    #         result_data, filename = file,
    #         format = 'GTiff', overwrite = TRUE
    #       )
    #     } else {
    #       # geopackage
    #       if (input$data_format == 'gpkg') {
    #         raster::writeRaster(
    #           result_data, filename = file,
    #           format = 'GPKG'
    #         )
    #       }
    #     }
    #   }
    # )

  } # end of server function

  # Run the application
  catdroughtapp <- shiny::shinyApp(
    ui = ui, server = server
  )

  # shiny::runApp(nfi_app)
  return(catdroughtapp)

}
