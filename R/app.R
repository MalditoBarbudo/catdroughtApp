#' function to launch the catdrought app
#'
#' @importFrom magrittr %>%
#'
#' @export
catdrought_app <- function(
  user = 'ifn', password = 'IFN2018creaf',
  host = NULL, port = NULL, dbname = 'catdrought_db'
) {

  ### DB access ################################################################
  catdrought_db <- pool::dbPool(
    RPostgreSQL::PostgreSQL(),
    user = user,
    password = password,
    dbname = dbname,
    host = host,
    port = port
  )

  ### Variables names inter ####################################################
  # sp_daily_choices <- c(
  #   "All woody species", "Pinus halepensis", "Pinus nigra", "Pinus sylvestris",
  #   "Pinus uncinata", "Pinus pinea", "Pinus pinaster", "Quercus ilex",
  #   "Quercus suber", "Quercus humilis", "Quercus faginea", "Fagus sylvatica"
  # )



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
      # custom css
      shiny::includeCSS(
        system.file('resources', 'catdrought.css', package = 'catdroughtApp')
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
        title = 'Current',
        # we need to create the ui in the server to catch the language input
        # and redraw all the inputs and texts in the selected lang
        shiny::uiOutput('current_ui')
      ) # end of current tab
    ) # end of navbar
  ) # end of UI

  ## SERVER ####
  server <- function(input, output, session) {
    ## debug #####
    output$debug1 <- shiny::renderPrint({
      input$map_daily_click
    })
    # output$debug2 <- shiny::renderPrint({
    #   map_reactives$map_click
    # })
    # output$debug3 <- shiny::renderPrint({
    #   map_reactives$map_shape_click
    # })

    ## lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    ## proper UI ####
    output$current_ui <- shiny::renderUI({

      lang_declared <- lang()
      dates_lang <- switch(
        lang_declared,
        'cat' = 'ca',
        'spa' = 'es',
        'eng' = 'en'
      )

      # browser()

      ## choices

      ####### correct way of doing it if we have all the dates
      # date_daily_choices <- seq(
      #   lubridate::ymd(Sys.Date() - 365), lubridate::ymd(Sys.Date() - 1),
      #   by = 'days'
      # )
      ####### incorrect way of doing it, but working with the local sample of data I have
      date_daily_choices <- seq(
        lubridate::ymd("2019-01-01"), lubridate::ymd("2019-06-02"), by = 'days'
      )
      ## TODO change to the correct way when available

      climate_vars <- c("Rain", "PET") %>%
        magrittr::set_names(translate_app(., lang_declared))
      fwb_vars <- c("NetPrec", "Eplant", "Esoil", "Runoff", "DeepDrainage") %>%
        magrittr::set_names(translate_app(., lang_declared))
      soil_moisture_vars <- c("REW", "Theta", "Psi") %>%
        magrittr::set_names(translate_app(., lang_declared))
      drought_stress_vars <- c("DDS", "NDD") %>%
        magrittr::set_names(translate_app(., lang_declared))

      shiny::sidebarLayout(
        # sidebar
        shiny::sidebarPanel(
          width = 3,
          ## variable selectors ####
          # var sel
          shiny::selectInput(
            'var_daily', translate_app('var_daily_label', lang_declared),
            choices = list(
              'Soil moisture' = soil_moisture_vars,
              'Water balance' = fwb_vars,
              'Climate' = climate_vars,
              'Drought stress' = drought_stress_vars
            ) %>% magrittr::set_names(translate_app(names(.), lang_declared))
          ),

          # date sel
          shiny::dateInput(
            'date_daily', translate_app('date_daily_label', lang_declared),
            value = date_daily_choices[1],
            min = date_daily_choices[1],
            max = date_daily_choices[length(date_daily_choices)],
            # TODO dates disabled for 2018, as the data is missing. This must be
            # removed when we have all the year data available
            # datesdisabled = seq(
            #   lubridate::ymd(date_daily_choices[1]), lubridate::ymd("2018-12-31"),
            #   by = 'days'
            # ),
            weekstart = 1, language = dates_lang
          ),
          # polygon sel
          shiny::selectInput(
            'display_daily', translate_app('display_daily_label', lang_declared),
            choices = c('none', "Watersheds", "Counties", "Municipalities", "IFN plots") %>%
              magrittr::set_names(translate_app(., lang_declared)),
            selected = 'none'
          ),
          # resoltion sel
          shiny::radioButtons(
            'resolution_daily', translate_app('resolution_daily_label', lang_declared),
            choices = c('Smoothed', '1km', '200m') %>%
              magrittr::set_names(translate_app(., lang_declared)),
            selected = 'Smoothed'
          ),

          # download button
          shiny::actionButton(
            'download_raster_daily', translate_app('download_raster_label', lang_declared)
          )
        ), # end of sidebar
        # main panel
        shiny::mainPanel(

          ########################################################### debug ####
          shiny::absolutePanel(
            id = 'debug', class = 'panel panel-default', fixed = TRUE,
            draggable = TRUE, width = 640, height = 'auto',
            # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
            # top = 'auto', left = 'auto', right = 100, bottom = 100,
            top = 60, left = 'auto', right = 50, bottom = 'auto',

            shiny::textOutput('debug1'),
            shiny::textOutput('debug2'),
            shiny::textOutput('debug3')
          ),
          ####################################################### end debug ####

          shiny::tabsetPanel(
            id = 'daily_main_panel',

            # map
            shiny::tabPanel(
              title = translate_app('map_tab_label', lang_declared),
              leaflet::leafletOutput('map_daily', height = 600) %>%
                shinyWidgets::addSpinner(spin = 'cube', color = '#26a65b')
            ),

            # series
            shiny::tabPanel(
              title = translate_app('series_tab_label', lang_declared),
              dygraphs::dygraphOutput('trends_daily'),
              shiny::downloadButton(
                'download_series_daily',
                translate_app('download_series_label', lang_declared)
              )
            )
          )
        ) # end of main panel
      )
    })

    # data reactive with the raster ####
    raster_selected_daily <- shiny::reactive({

      shiny::validate(
        shiny::need(input$var_daily, 'No variable selected'),
        shiny::need(input$date_daily, 'No date selected'),
        shiny::need(input$resolution_daily, 'No resolution selected')
      )
      # browser()

      # date
      date_sel <- input$date_daily
      date_sel_parsed <- stringr::str_remove_all(date_sel, pattern = '-')

      # band
      band_sel <- switch(
        input$var_daily,
        "DeepDrainage" = 3,
        "Eplant" = 4,
        "Esoil" = 5,
        "LAI" = 6,
        "NetPrec" = 7,
        "PET" = 8,
        "Psi" = 9,
        "Rain" = 10,
        "REW" = 11,
        "Runoff" = 12,
        "Theta" = 13,
        "DDS" = 2,
        "NDD" = 1
      )

      # resolution
      resolution_sel <- switch(
        input$resolution_daily,
        'Smoothed' = 'smooth',
        '1km' = 'low',
        '200m' = 'high'
      )

      # table name
      table_name <- glue::glue(
        "catdrought_{resolution_sel}_{date_sel_parsed}"
      )

      # temp conn and raster getter
      # browser()
      temp_postgresql_conn <- pool::poolCheckout(catdrought_db)
      raster_res <- rpostgis::pgGetRast(
        temp_postgresql_conn, name = c('daily', table_name), bands = band_sel
      )
      pool::poolReturn(temp_postgresql_conn)

      return(raster_res)


      # switch for transforming the date to band
      ####### correct way of doing it if we have all the dates
      # dates_avail <- seq(
      #   lubridate::ymd(Sys.Date() - 365), lubridate::ymd(Sys.Date() - 1),
      #   by = 'days'
      # )
      ####### incorrect way of doing it, but working for the local sample I have at the moment
      # dates_avail <- seq(
      #   lubridate::ymd("2019-01-01"), lubridate::ymd("2019-06-02"), by = 'days'
      # ) ## TODO change to the correct way when available
      # band_sel <- which(input$date_daily == dates_avail)
      # selected_var <- input$var_daily
      #
      # # raster intermediates
      # temp_postgresql_conn <- pool::poolCheckout(catdrought_db)
      # raster_res <- rpostgis::pgGetRast(
      #   temp_postgresql_conn, selected_var, bands = band_sel
      # )
      # pool::poolReturn(temp_postgresql_conn)
      #
      # return(raster_res)
    })

    ## map output ####
    output$map_daily <- leaflet::renderLeaflet({

      raster_daily <- raster_selected_daily()

      palette <- leaflet::colorNumeric(
        viridis::plasma(100),
        # raster::values(basal_area_raster),
        raster::values(raster_daily),
        na.color = 'transparent'
      )

      # browser()

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
          raster_daily, project = TRUE, group = 'raster',
          colors = palette, opacity = 1
        ) %>%
        leaflet::addLegend(
          pal = palette, values = raster::values(raster_daily),
          title = translate_app(input$var_daily, lang()),
          position = 'bottomright',
          opacity = 1
        )
    })

    ## series polygons observers ####

    # draw polygons observer
    shiny::observe({

      shiny::validate(
        shiny::need(input$display_daily, 'no polygon/plots selected'),
        shiny::need(input$var_daily, 'no var selected'),
        shiny::need(input$resolution_daily, 'no res selected')
      )

      var_dummy <- input$var_daily

      if (input$display_daily == 'none') {
        leaflet::leafletProxy('map_daily') %>%
          leaflet::clearGroup('display_daily')
        return()
      }

      # if plots do markers, if polys do polygons
      if (input$display_daily == 'IFN plots') {
        leaflet::leafletProxy('map_daily') %>%
          leaflet::clearGroup('display_daily') %>%
          leaflet::addCircleMarkers(
            data = nfi4_plots,
            group = 'display_daily',
            label = ~plot_id,
            clusterOptions = leaflet::markerClusterOptions()
          )
      } else {

        polygon_object_name <- glue::glue("{tolower(input$display_daily)}_polygons")

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

    })

    # clicked polygon reactive. returns the polygon selected
    clicked_poly <- shiny::reactive({

      shiny::validate(
        shiny::need(input$map_daily_shape_click, 'no click on map')
      )

      # browser()

      clicked_poly <- input$map_daily_shape_click
      polygon_object <- rlang::eval_tidy(
        rlang::sym(glue::glue("{tolower(input$display_daily)}_polygons"))
      ) %>%
        dplyr::filter(
          poly_id == clicked_poly$id
        ) %>%
        dplyr::pull(geometry) %>%
        sf::st_as_text()

      return(polygon_object)
    })

    # time series data
    series_data_for_polys <- shiny::reactive({

      # polygon
      polygon_sel <- clicked_poly()

      # band
      band_sel <- switch(
        input$var_daily,
        "DeepDrainage" = 3,
        "Eplant" = 4,
        "Esoil" = 5,
        "LAI" = 6,
        "NetPrec" = 7,
        "PET" = 8,
        "Psi" = 9,
        "Rain" = 10,
        "REW" = 11,
        "Runoff" = 12,
        "Theta" = 13,
        "DDS" = 2,
        "NDD" = 1
      )

      # resolution
      resolution_sel <- switch(
        input$resolution_daily,
        'Smoothed' = 'smooth',
        '1km' = 'low',
        '200m' = 'high'
      )

      # table name
      table_name <- glue::glue(
        "daily.catdrought_{resolution_sel}"
      )



      # data query to get the dump of the data
      data_query <- glue::glue(
        "
          with
          feat as (select st_geomfromtext('{polygon_sel}', 4326) as geom),
          b_stats as (select day, (stats).* from (select day, ST_SummaryStats(st_clip(rast, {band_sel}, geom, true)) as stats
            from {table_name}
            inner join feat
            on st_intersects(feat.geom,rast)
          ) as foo
          )
          select day, sum(mean*count)/sum(count) as avg_pval from b_stats
          where count > 0
          group by day;
        "
      )

      res <- pool::dbGetQuery(catdrought_db, data_query)

      return(res)

    })

    ## series pixel observers ####
    series_data_for_pixel <- shiny::reactive({

      shiny::validate(
        shiny::need(input$map_daily_click, 'no map click')
        # shiny::need(input$display_daily == 'none', 'polygons_selected'),
        # shiny::need(input$var_daily, 'no var selected'),
        # shiny::need(input$resolution_daily, 'no resolution selected')
      )

      clicked_pixel <- input$map_daily_click

      # band
      band_sel <- switch(
        input$var_daily,
        "DeepDrainage" = 3,
        "Eplant" = 4,
        "Esoil" = 5,
        "LAI" = 6,
        "NetPrec" = 7,
        "PET" = 8,
        "Psi" = 9,
        "Rain" = 10,
        "REW" = 11,
        "Runoff" = 12,
        "Theta" = 13,
        "DDS" = 2,
        "NDD" = 1
      )

      # resolution
      resolution_sel <- switch(
        input$resolution_daily,
        'Smoothed' = 'smooth',
        '1km' = 'low',
        '200m' = 'high'
      )

      # table name
      table_name <- glue::glue(
        "daily.catdrought_{resolution_sel}"
      )

      pixel_query <- glue::glue(
        "
          SELECT day, ST_Value(
            rast,
            {band_sel},
            ST_SetSRID(ST_Makepoint({clicked_pixel$lng},{clicked_pixel$lat}),4326)
          ) As pixel_value
          FROM {table_name}
          WHERE ST_Intersects(
            rast,
            ST_SetSRID(ST_Makepoint({clicked_pixel$lng},{clicked_pixel$lat}),4326)
          )
        "
      )

      res <- pool::dbGetQuery(catdrought_db, pixel_query)
      return(res)

    })


    ## series output ####
    output$trends_daily <- dygraphs::renderDygraph({

      # variable
      var_id <- input$var_daily

      # here we have to check if poly or pixel
      if (input$display_daily != 'none') {
        # polygon id
        poly_id <- input$map_daily_shape_click$id

        # data and plot
        plot_data <- series_data_for_polys()
        res <- plot_data %>%
          dplyr::select(avg_pval) %>%
          xts::as.xts(order.by = plot_data$day) %>%
          dygraphs::dygraph(
            main = glue::glue("{translate_app(var_id, lang())} - {poly_id}"),
            ylab = glue::glue("{translate_app(var_id, lang())}")
          )
      } else {
        # click
        clicked_pixel <- input$map_daily_click

        # data and plot
        plot_data <- series_data_for_pixel()
        res <- plot_data %>%
          dplyr::select(pixel_value) %>%
          xts::as.xts(order.by = plot_data$day) %>%
          dygraphs::dygraph(
            main = glue::glue("{translate_app(var_id, lang())} - pixel at [{round(clicked_pixel$lng, 3)}, {round(clicked_pixel$lat, 3)}]"),
            ylab = glue::glue("{translate_app(var_id, lang())}")
          )
      }

      return(res)

    })

    ## observer to change the active tab
    tab_change_events <- shiny::reactiveValues()
    shiny::observe({
      tab_change_events$shape <- input$map_daily_shape_click
      tab_change_events$pixel <- input$map_daily_click
    })

    shiny::observeEvent(
      ignoreInit = TRUE,
      eventExpr = tab_change_events,
      handlerExpr = {

        # go to series
        shiny::updateTabsetPanel(
          session, 'daily_main_panel', selected = translate_app('map_tab_label', lang())
        )

        # modal waiting time
        shiny::showModal(
          ui = shiny::modalDialog(
            shiny::tagList(
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::p(
                    translate_app('modal_waiting_p', lang())
                  )
                )
              )
            ),
            easyClose = TRUE,
            footer = shiny::tagList(
              # shiny::modalButton(translate_app('modal_dismiss_label', lang_declared)),
              shiny::modalButton(translate_app('dismiss_btn', lang()))
            )
          )
        )
      }
    )


    ## download handlers ####
    # modal for saving the raster data
    shiny::observeEvent(
      eventExpr = input$download_raster_daily,
      handlerExpr = {

        lang_declared = lang()

        shiny::showModal(
          ui = shiny::modalDialog(
            shiny::tagList(

              shiny::fluidRow(
                shiny::column(
                  12,
                  # format options
                  shiny::selectInput(
                    'data_format',
                    translate_app('download_raster_format', lang_declared),
                    choices = list(
                      'GIS' = c('gtiff', 'gpkg') %>%
                        magrittr::set_names(translate_app(., lang_declared)),
                    ),
                    selected = 'gtiff'
                  )
                  # length options
                  # shiny::radioButtons(
                  #   'data_length', translate_app('data_length_label', lang_declared),
                  #   # text_translate('data_length', lang(), texts_thes),
                  #   choices = c('visible', 'all_columns') %>%
                  #     magrittr::set_names(translate_app(., lang_declared)),
                  #   selected = 'visible', width = '100%'
                  # )
                )
              )
            ),
            easyClose = TRUE,
            footer = shiny::tagList(
              # shiny::modalButton(translate_app('modal_dismiss_label', lang_declared)),
              shiny::modalButton(translate_app('dismiss_btn', lang())),
              shiny::downloadButton(
                'download_data_with_options',
                label = translate_app('download_raster_label', lang()),
                class = 'btn-success'
              )
            )
          )
        )
      }
    )

    # download logic for series data
    output$download_series_daily <- shiny::downloadHandler(
      filename = function() {
        file_name <- glue::glue(
          'catdrought_series_for_{input$map_daily_shape_click$id}.csv'
        )
      },
      content = function(file) {
        series_data_for_polys() %>%
          readr::write_csv(path = file)
      }
    )


    output$download_data_with_options <- shiny::downloadHandler(
      filename = function() {

        file_name <- switch(
          input$data_format,
          'gtiff' = 'catdrought_data.tif',
          'gpkg' = 'catdrought_data.gpkg'
        )

        return(file_name)
      },
      content = function(file) {

        # data length
        result_data <- raster_selected_daily()

        # data format

        # shapefile
        if (input$data_format == 'gtiff') {
          raster::writeRaster(
            result_data, filename = file,
            format = 'GTiff', overwrite = TRUE
          )
        } else {
          # geopackage
          if (input$data_format == 'gpkg') {
            raster::writeRaster(
              result_data, filename = file,
              format = 'GPKG'
            )
          }
        }
      }
    )

  } # end of server function

  # Run the application
  catdroughtapp <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {

      ## on stop routine to cloose the db pool
      shiny::onStop(function() {
        pool::poolClose(catdrought_db)
      })
    }
  )

  # shiny::runApp(nfi_app)
  return(catdroughtapp)

}
