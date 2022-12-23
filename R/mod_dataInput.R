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
    date_daily_choices_stripped <- date_daily_choices %>%
      as.character() %>%
      stringr::str_remove_all('-')

    dates_available <- pool::dbGetQuery(
      catdroughtdb$.__enclos_env__$private$pool_conn, glue::glue(
        "SELECT * FROM information_schema.tables
        WHERE table_schema = 'daily'"
      )
    ) %>%
      dplyr::select(table_name) %>%
      dplyr::filter(stringr::str_detect(table_name, 'catdrought_low')) %>%
      dplyr::arrange() %>%
      dplyr::pull(table_name) %>%
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
    soil_moisture_vars <- c("Theta", "Psi", "REW") %>%
      magrittr::set_names(translate_app(., lang_declared))
    climate_vars <- c("PET", "Precipitation") %>%
      magrittr::set_names(translate_app(., lang_declared))
    evap_surface_vars <- c('LAI') %>%
      magrittr::set_names(translate_app(., lang_declared))
    fwb_vars <- c("Interception", "Infiltration", 'Runoff', 'DeepDrainage', 'Esoil', 'Eplant') %>%
      magrittr::set_names(translate_app(., lang_declared))
    drought_stress_vars <- c("DDS", "LMFC") %>%
      magrittr::set_names(translate_app(., lang_declared))

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
        ) %>% magrittr::set_names(translate_app(names(.), lang_declared))
      ),

      # date sel
      shiny::dateInput(
        ns('date_daily'), translate_app('date_daily_label', lang_declared),
        value = date_daily_choices[length(date_daily_choices)],
        min = date_daily_choices[1],
        max = date_daily_choices[length(date_daily_choices)],
        weekstart = 1, language = dates_lang
      ),
      # polygon sel
      shiny::selectInput(
        ns('display_daily'), translate_app('display_daily_label', lang_declared),
        choices = c(
          'none', "Watersheds", "Counties", "Municipalities", "IFN plots", "file"
        ) %>%
          magrittr::set_names(translate_app(., lang_declared)),
        selected = 'none'
      ),

      shiny::fluidRow(
        shiny::column(6, align = 'center',

          # ..... RADIO BUT LEGEND COLOR ......
          # ...................................

          #      .) Dejo COMENTADA el CANVIO de COLOR de LEYENDA
          #      .) Me espero a hablar-lo con Miquel y Víctor

          shinyWidgets::radioGroupButtons(
            ns("legend_modify"),
            translate_app("type_legend_label", lang_declared),
            size = 'normal',
            choices = c( "1st_label" = "tip_1",
                         "estandard_label" = "estandard",
                         "2nd_label" = "tip_2"
            ) %>% magrittr::set_names(translate_app(names(.), lang_declared)),
            selected = 'estandard', direction = 'vertical',
            status = 'lfc_radiogroupbuttons'
          )
        ),

        shiny::column(6, align = 'center',

          # ... CHEK BUTTON LEGEND INVERT .....
          # ...................................

          #      .) Check Button
          #      .) Para invertir Leyenda

          shinyWidgets::prettyCheckbox(
            ns('legend_check'),
            translate_app('reverse_legend', lang_declared),
            status = 'success', shape = 'curve', fill = TRUE
          )
        )
      ), # end of Fluid Row
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
      ) # end of hidden file selector
      # # resoltion sel
      # shiny::radioButtons(
      #   ns('resolution_daily'), translate_app('resolution_daily_label', lang_declared),
      #   choices = c('Smoothed', '1km', '200m') %>%
      #     magrittr::set_names(translate_app(., lang_declared)),
      #   selected = 'Smoothed'
      # )
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

  ## returning inputs ####
  # reactive values to return and use in other modules
  data_reactives <- shiny::reactiveValues()

  shiny::observe({
    data_reactives$var_daily <- input$var_daily
    data_reactives$date_daily <- input$date_daily
    data_reactives$display_daily <- input$display_daily
    # data_reactives$resolution_daily <- input$resolution_daily
    data_reactives$user_file_sel <- input$user_file_sel
    data_reactives$legend_check <- input$legend_check
    data_reactives$legend_modify_reactive <- input$legend_modify

  })

  return(data_reactives)
}
