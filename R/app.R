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
    # output$debug1 <- shiny::renderPrint({
    #   input$raster_map_shape_click
    # })
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

      climate_vars <- c("prec", "pet") %>%
        magrittr::set_names(translate_app(., lang_declared))
      fwb_vars <- c("netprec", "planttrans", "soilevap", "runoff", "deepdrainage") %>%
        magrittr::set_names(translate_app(., lang_declared))
      soil_moisture_vars <- c("rew", "smc", "swp") %>%
        magrittr::set_names(translate_app(., lang_declared))
      drought_stress_vars <- c("stressin", "stressdur") %>%
        magrittr::set_names(translate_app(., lang_declared))

      shiny::sidebarLayout(
        # sidebar
        shiny::sidebarPanel(
          width = 3,
          ## variable selectors ####
          # var sel
          shiny::selectInput(
            'var_daily', 'Choose variable',
            choices = list(
              'Soil moisture' = soil_moisture_vars,
              'Water balance' = fwb_vars,
              'Climate' = climate_vars,
              'Drought stress' = drought_stress_vars
            ) %>% magrittr::set_names(translate_app(names(.), lang_declared)),
            selected = 'netprec'
          ),

          # date sel
          shiny::dateInput(
            'date_daily', 'Date',
            value = date_daily_choices[length(date_daily_choices)],
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
            'display_daily', 'Selection type',
            choices = c('none', "Watersheds", "Counties", "Municipalities", "IFN plots") %>%
              magrittr::set_names(translate_app(., lang_declared)),
            selected = 'none'
          ),
          # resoltion sel
          shiny::radioButtons(
            'resolution_daily', 'Raster res',
            choices = c('Smoothed', '1km', '200m') %>%
              magrittr::set_names(translate_app(., lang_declared)),
            selected = '200m'
          ),

          # download button
          shiny::actionButton(
            'download_raster_daily', 'Download raster'
          )
        ), # end of sidebar
        # main panel
        shiny::mainPanel(
          shiny::tabsetPanel(

            # map
            shiny::tabPanel(
              title = 'Map',
              leaflet::leafletOutput('map_daily', height = 600) %>%
                shinyWidgets::addSpinner(spin = 'cube', color = '#26a65b')
            ),

            # series
            shiny::tabPanel(
              title = 'Series',
              dygraphs::dygraphOutput('trends_daily'),
              shiny::downloadButton(
                'donwload_series_daily', 'Download trend'
              )
            )
          )
        ) # end of main panel
      )
    })

    # reactive with the raster
    raster_selected_daily <- shiny::reactive({

      shiny::validate(
        shiny::need(input$var_daily, 'No variable selected'),
        shiny::need(input$date_daily, 'No date selected')
      )

      # browser()

      # switch for transforming the date to band
      ####### correct way of doing it if we have all the dates
      # dates_avail <- seq(
      #   lubridate::ymd(Sys.Date() - 365), lubridate::ymd(Sys.Date() - 1),
      #   by = 'days'
      # )
      ####### incorrect way of doing it, but working for the local sample I have at the moment
      dates_avail <- seq(
        lubridate::ymd("2019-01-01"), lubridate::ymd("2019-06-02"), by = 'days'
      ) ## TODO change to the correct way when available
      band_sel <- which(input$date_daily == dates_avail)
      selected_var <- input$var_daily

      # raster intermediates
      temp_postgresql_conn <- pool::poolCheckout(catdrought_db)
      raster_res <- rpostgis::pgGetRast(
        temp_postgresql_conn, selected_var, bands = band_sel
      )
      pool::poolReturn(temp_postgresql_conn)

      return(raster_res)
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
          raster_daily, project = FALSE, group = 'raster',
          colors = palette, opacity = 1
        ) %>%
        leaflet::addLegend(
          pal = palette, values = raster::values(raster_daily),
          title = input$var_daily, position = 'bottomright',
          opacity = 1
        )
    })

    ## series polygons observers ####

    # draw polygons observer
    shiny::observeEvent(
      eventExpr = input$display_daily,
      handlerExpr = {

        shiny::validate(
          shiny::need(input$display_daily, 'no polygon/plots selected')
        )

        if (input$display_daily == 'none') {
          leaflet::leafletProxy('map_daily') %>%
            leaflet::clearGroup('display_daily')
          return()
        }

        # if plots do something, if polys do something else
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
              fillOpacity = 0, color = 'black', stroke = TRUE,
              label = ~poly_id
            )
        }
      }
    )

    ## download handler ####
    # modal for saving the data
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
                    'data_format', 'format',#translate_app('data_format_label', lang_declared),
                    choices = list(
                      'GIS' = c('shp', 'wkt', 'gpkg'),# %>%
                        # magrittr::set_names(translate_app(., lang_declared)),
                      'TABLE' = c('csv', 'xlsx') #%>%
                        # magrittr::set_names(translate_app(., lang_declared))
                    ), #%>% magrittr::set_names(translate_app(names(.), lang_declared)),
                    selected = 'gpkg'
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
              shiny::modalButton('dismiss'),
              shiny::downloadButton(
                'download_data_with_options',
                label = 'download',#translate_app('sidebar_h4_download', lang_declared),
                class = 'btn-success'
              )
            )
          )
        )
      }
    )

    output$download_data_with_options <- shiny::downloadHandler(
      filename = function() {

        file_name <- switch(
          input$data_format,
          'shp' = 'catdrought_data.zip',
          'wkt' = 'catdrought_data.csv',
          'gpkg' = 'catdrought_data.gpkg',
          'csv' = 'catdrought_data.csv',
          'xlsx' = 'catdrought_data.xlsx'
        )

        return(file_name)
      },
      content = function(file) {

        # data length
        result_data <- raster_selected_daily()

        # data format

        # shapefile
        if (input$data_format == 'shp') {

        } else {
          # well known text
          if (input$data_format == 'wkt') {

          } else {
            # geopackage
            if (input$data_format == 'gpkg') {

            } else {
              # csv text (no geometry)
              if (input$data_format == 'csv') {

              } else {
                # xlsx (no geometry)

              }
            }
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
