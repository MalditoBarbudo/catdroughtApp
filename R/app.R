#' function to launch the catdrought app
#'
#' @importFrom terra has.RGB
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



  matomo_script <- shiny::HTML(
    "var _paq = window._paq = window._paq || [];
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
(function() {
  var u='https://stats-emf.creaf.cat/';
  _paq.push(['setTrackerUrl', u+'matomo.php']);
  _paq.push(['setSiteId', '6']);
  var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
  g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
})();

// Event Tracking Code
$(document).on('shiny:inputchanged', function(event) {
  if (/^mod_data*/.test(event.name)) {
    _paq.push(['trackEvent', 'dataInputs', event.name, event.value, 1, {dimension1: event.value}]);
  }
  if (/^mod_save*/.test(event.name)) {
    _paq.push(['trackEvent', 'saveInputs', event.name, event.value, 2, {dimension1: event.value}]);
  }
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
    # waiter
    waiter::use_waiter(),
    waiter::use_hostess(),

    # css
    shiny::tags$head(
      # js script,
      shiny::tags$script(matomo_script),
      shiny::tags$script(keep_alive_script),
      # corporative image css
      shiny::includeCSS(
        system.file('apps_css', 'corp_image.css', package = 'lfcdata')
      ),
      # custom css
      shiny::includeCSS(
        system.file('apps_css', 'catdrought.css', package = 'lfcdata')
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

      # footer
      footer = shiny::tags$footer(
        shiny::fluidRow(
          shiny::column(
            width = 12, align = "right",
            shiny::HTML(glue::glue(
              '<img src="images/emf_white_logo.svg" width="120px" class="d-inline-block" alt="" loading="lazy">
              <img src="images/creaf_white_logo.svg" width="135px" class="d-inline-block" alt="" loading="lazy">
              <span>({lubridate::year(Sys.Date())})</span>'
            ))
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
            width = 4,
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
            width = 8,
            shiny::div(
              id = 'overlay_div',
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
      ),
      shiny::tabPanel(
        title = mod_tab_translateOutput('tech_specs_translation'),
        value = 'tech_spec_panel',
        mod_techSpecsOutput('mod_techSpecsOutput')
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

    ## mapbox token
    mapdeck::set_token(Sys.getenv("MAPBOX_TOKEN"))

    ###### Maintenance notice
    # send an alarm when loading app or changes langs
    # shiny::observeEvent(
    #   lang(),
    #   {
    #     shinyWidgets::show_alert(
    #       title = translate_app('under_construction_title', lang()),
    #       text = translate_app('under_construction_text', lang())
    #     )
    #   }
    # )
    ######

    # modules ####
    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput', catdroughtdb, lang
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
      main_data_reactives, data_reactives, catdroughtdb,
      lang
    )
    # technical specifications module
    shiny::callModule(
      mod_techSpecs, 'mod_techSpecsOutput',
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
    shiny::callModule(
      mod_tab_translate, 'tech_specs_translation',
      'tech_specs_translation', lang
    )

  } # end of server function

  # Run the application
  catdroughtapp <- shiny::shinyApp(
    ui = ui, server = server
  )

  # shiny::runApp(nfi_app)
  return(catdroughtapp)

}
