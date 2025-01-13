#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
mod_dataInput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_data_container')
    )
  )
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#'
#' @export
mod_data <- function(
  input, output, session,
  catdroughtdb, lang
) {

  # renderUI ####
  output$mod_data_container <- shiny::renderUI({
    # ns
    ns <- session$ns

    lang_declared <- lang()
    dates_lang <- switch(
      lang_declared,
      'cat' = 'ca',
      'spa' = 'es',
      'eng' = 'en'
    )

    ## choices
    # dates
    date_daily_choices <- seq(
      lubridate::ymd(Sys.Date() - 366), lubridate::ymd(Sys.Date() - 1),
      # lubridate::ymd(Sys.Date() - 366), lubridate::ymd('2020-12-12'),
      by = 'days'
    )

    # check dates available in the db and limit the imput to the first missing
    # one minus 1:
    date_daily_choices_stripped <- date_daily_choices |>
      as.character() |>
      stringr::str_remove_all('-')

    dates_available <- pool::dbGetQuery(
      catdroughtdb$.__enclos_env__$private$pool_conn, glue::glue(
        "SELECT * FROM information_schema.tables
        WHERE table_schema = 'daily'"
      )
    ) |>
      dplyr::select(table_name) |>
      dplyr::filter(stringr::str_detect(table_name, 'catdrought_low')) |>
      dplyr::arrange() |>
      dplyr::pull(table_name) |>
      stringr::str_extract('[0-9]+')

    first_missing <-
      dplyr::first(which(!date_daily_choices_stripped %in% dates_available))

    if (!is.na(first_missing)) {
      date_daily_choices <- date_daily_choices[1:(first_missing - 1)]
    }


    # variable groups as per Miquel
    # soil moisture: Theta, Psi, REW
    # climate: PET
    # evaporative surface: LAI
    # water balance: Infiltration, RunOff, DeepDrainage, Esoil, Eplant
    # drought stress: DDS
    soil_moisture_vars <- c("Theta", "Psi", "REW") |>
      purrr::set_names(translate_app(c("Theta", "Psi", "REW"), lang_declared))
    climate_vars <- c("PET", "Precipitation") |>
      purrr::set_names(translate_app(c("PET", "Precipitation"), lang_declared))
    evap_surface_vars <- c('LAI') |>
      purrr::set_names(translate_app(c('LAI'), lang_declared))
    fwb_vars <- c("Interception", "Infiltration", 'Runoff', 'DeepDrainage', 'Esoil', 'Eplant') |>
      purrr::set_names(translate_app(c("Interception", "Infiltration", 'Runoff', 'DeepDrainage', 'Esoil', 'Eplant'), lang_declared))
    drought_stress_vars <- c("DDS", "LMFC") |>
      purrr::set_names(translate_app(c("DDS", "LMFC"), lang_declared))

    shiny::tagList(
      ## variable selectors ####
      # var sel
      shiny::selectInput(
        ns('var_daily'), translate_app('var_daily_label', lang_declared),
        choices = list(
          'Soil moisture' = soil_moisture_vars,
          'Climate' = climate_vars,
          'Evaporative surface' = evap_surface_vars,
          'Water balance' = fwb_vars,
          'Drought stress' = drought_stress_vars
        ) |> purrr::set_names(translate_app(c(
          'Soil moisture', 'Climate', 'Evaporative surface', 'Water balance', 'Drought stress'
        ), lang_declared))
      ),

      # date sel
      shiny::dateInput(
        ns('date_daily'), translate_app('date_daily_label', lang_declared),
        value = date_daily_choices[length(date_daily_choices)],
        max = date_daily_choices[length(date_daily_choices)],
        # value = "2023/05/10",
        # max = "2023/05/10",
        min = date_daily_choices[1],
        weekstart = 1, language = dates_lang
      ),
      # polygon sel
      shiny::selectInput(
        ns('display_daily'), translate_app('display_daily_label', lang_declared),
        choices = c(
          'none', "Watersheds", "Counties", "Municipalities", "file"
        ) |>
          purrr::set_names(translate_app(c(
            'none', "Watersheds", "Counties", "Municipalities", "file"
          ), lang_declared)),
        selected = 'none'
      ),
      shinyjs::hidden(
        shiny::div(
          id = ns('file_upload_panel'),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::fileInput(
                ns('user_file_sel'),
                translate_app('user_file_sel_label', lang()),
                accept = c('zip', 'gpkg'),
                buttonLabel = translate_app(
                  'user_file_sel_buttonLabel', lang()
                ),
                placeholder = translate_app(
                  'user_file_sel_placeholder', lang()
                )
              )
            ),
            shiny::column(
              6,
              shiny::p(translate_app('file_text', lang()))
            )
          )
        )
      ), # end of hidden file selector
      shiny::br(),
      shiny::p(translate_app('using_3d', lang()))
    )

  })

  ## observers ####
  # observer to show the file upload panel if needed
  shiny::observe({

    shiny::validate(
      shiny::need(input$display_daily, 'no type')
    )
    display_daily <- input$display_daily

    if (display_daily == 'file') {
      shinyjs::show('file_upload_panel')
    } else {
      shinyjs::hide('file_upload_panel')
    }
  })

  # sweet alert to take visitors to sitedrought
  shiny::observe({
    shiny::validate(
      shiny::need(input$display_daily, 'no type')
    )
    display_daily <- input$display_daily

    if (display_daily == 'IFN plots') {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = translate_app('sweet_alert_IFN_plots_title', lang()),
        text = shiny::tags$span(
          shiny::tags$p(translate_app('sweet_alert_IFN_plots_text', lang())),
          shiny::tags$br(),
          shiny::tags$a(href = "https://laboratoriforestal.creaf.cat/sitedroughtapp", "SiteDrought App")
        ),
        type = "warn",
        btn_label = translate_app("dismiss_btn", lang()),
        html = TRUE
      )
    }
  })

  ## returning inputs ####
  # reactive values to return and use in other modules
  data_reactives <- shiny::reactiveValues()

  shiny::observe({
    data_reactives$var_daily <- input$var_daily
    data_reactives$date_daily <- input$date_daily
    data_reactives$display_daily <- input$display_daily
    # data_reactives$resolution_daily <- input$resolution_daily
    data_reactives$user_file_sel <- input$user_file_sel
  })

  return(data_reactives)
}
