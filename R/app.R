#' function to launch the catdrought app
#'
#' @importFrom magrittr %>%
#'
#' @export
catdrought_app <- function(
  user = 'guest', password = 'guest',
  host = NULL, port = NULL, dbname = 'catdrought_db'
) {

  ### DB access ################################################################
  # catdrought_db <- pool::dbPool(
  #   RPostgreSQL::PostgreSQL(),
  #   user = user,
  #   password = password,
  #   dbname = dbname,
  #   host = host,
  #   port = port
  # )

  ### Variables names inter ####################################################
  sp_daily_choices <- c(
    "All woody species", "Pinus halepensis", "Pinus nigra", "Pinus sylvestris",
    "Pinus uncinata", "Pinus pinea", "Pinus pinaster", "Quercus ilex",
    "Quercus suber", "Quercus humilis", "Quercus faginea", "Fagus sylvatica"
  )

  date_daily_choices <- c(Sys.Date()-4, Sys.Date()-3, Sys.Date()-2, Sys.Date()-1, Sys.Date())

  climate_vars <- c(
    "Precipitation (mm)", "Potential evapo-transpiration (mm)"
    # "SPEI (k=3)", "SPEI (k=6)", "SPEI (k=12)"
  )
  fwb_vars <- c(
    "Net precipitation (mm)", "LAI (m2/m2)","Plant transpiration (mm)",
    "Soil evaporation (mm)", "Run-off (mm)", "Deep drainage (mm)"
  )
  soil_moisture_vars <- c(
    "Soil depth (cm)", "Water content at field capacity (mm)",
    "Water content at wilting point (mm)"  ,"Water holding capacity (mm)",
    "Topsoil texture type", "Subsoil texture type","Topsoil rock fragment content (%)",
    "Subsoil rock fragment content (%)"
  )
  drought_stress_vars <- c("Stress intensity", "Stress duration")

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

        shiny::sidebarLayout(
          # sidebar
          shiny::sidebarPanel(
            width = 3,
            ## variable selectors ####
            # var type
            shiny::selectInput(
              'mode_daily', 'Variable type',
              choices = c(
                "Climate", "Forest water balance",
                "Soil moisture", "Drought stress"
              ),
              selected = "Forest water balance"
            ),
            # var sel
            shiny::selectInput(
              'var_daily', 'Choose variable',
              choices = ''
            ),
            # species sel
            shinyjs::hidden(
              shiny::selectInput(
                'sp_daily', 'Choose species',
                choices = sp_daily_choices,
                selected = sp_daily_choices[1]
              )
            ),
            # date sel
            shiny::dateInput(
              'date_daily', 'Date',
              value = date_daily_choices,
              min = date_daily_choices[1],
              max = date_daily_choices[length(date_daily_choices)],
              weekstart = 1, language = "ca"
            ),
            # polygon sel
            shiny::selectInput(
              'display_daily', 'Selection type',
              choices = c('none', "Watersheds", "Counties", "Municipalities", "IFN plots"),
              selected = 'none'
            ),
            # resoltion sel
            shiny::radioButtons(
              'resolution_daily', 'Raster res',
              choices = c('Smoothed', '1km', '200m'),
              selected = '200m'
            ),

            # download button
            shiny::downloadButton(
              'download_raster_daily', 'Download raster'
            )
          ), # end of sidebar
          # main panel
          shiny::mainPanel(
            shiny::tabsetPanel(

              # map
              shiny::tabPanel(
                title = 'Map',
                leaflet::leafletOutput('map_daily', height = 600)
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

    # observer to update var_daily
    shiny::observe({
      var_daily_choices <- switch(
        input$mode_daily,
        'Climate' = climate_vars,
        'Forest water balance' = fwb_vars,
        'Soil moisture' = soil_moisture_vars,
        'Drought stress' = drought_stress_vars
      )
      shiny::updateSelectInput(
        session, inputId = 'var_daily',
        label = 'Choose variable', choices = var_daily_choices,
        selected = var_daily_choices[1]
      )
    })

    # observer to show sp_daily
    shiny::observe({
      if (!is.null(input$mode_daily) && input$mode_daily == 'Drought stress') {
        shinyjs::show('sp_daily')
      } else {
        shinyjs::hide('sp_daily')
      }
    })

    # reactive with the raster
    raster_selected_daily <- shiny::reactive({

      # switch for transforming the date to band
      # HOW????
      band_sel <- 1


      # raster intermediates
      temp_postgresql_conn <- pool::poolCheckout(catdrought_db)
      raster_res <- rpostgis::pgGetRast(
        temp_postgresql_conn, input$var_daily, bands = band_sel
      )
      pool::poolReturn(temp_postgresql_conn)

      return(raster_res)
    })

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
