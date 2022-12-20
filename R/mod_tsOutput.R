#' mod_tsOutput and mod_ts
#'
#' @description A shiny module to generate and populate the time series output
#'
#' @param id shiny id
#'
#' @export
mod_tsOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    dygraphs::dygraphOutput(ns('timeseries_daily')),
    shiny::uiOutput(
      ns('divisions_container')
    )
  )
}

#' mod_ts server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,main_data_reactives reactives needed
#' @param lang lang value
#'
#' @export
#'
#' @rdname mod_tsOutput
mod_ts <- function(
  input, output, session,
  data_reactives, main_data_reactives,
  lang
) {

  output$timeseries_daily <- dygraphs::renderDygraph({

    # shiny::req(
    #   main_data_reactives$timeseries_data, data_reactives$var_daily
    # )
    shiny::validate(
      shiny::need(main_data_reactives$timeseries_data, 'no ts data yet'),
      shiny::need(data_reactives$var_daily, 'no inputs yet')
    )

    # browser()

    var_daily <- data_reactives$var_daily
    display_daily <- data_reactives$display_daily
    timeseries_data <- main_data_reactives$timeseries_data$dygraph

  })


  # ......... OUTPUT / EXPLICACIÓN .........
  # ........................................

  #      .) DIV que muestra explicacion sobre percentiles históricos
  #      .) Solo se visualiza con las variables que tiene percentil

  output$divisions_container <- shiny::renderUI({

    # ......... INICIALIZAR .............
    # ...................................

    #       .) NS = IDs únicos
    #       .) LANG = F(x) definida en APP.R
    #       .) DATES_LANG = Cambio de nomenclatura de lengua

    ns <- session$ns
    lang_declared <- lang()
    dates_lang <- switch(
      lang_declared,
      'cat' = 'ca',
      'spa' = 'es',
      'eng' = 'en'
    )

    shinyjs::hidden(
      shiny::div(
        id = ns('explanation_divisions'),
        shiny::HTML(translate_app('expl_divisions', lang_declared))
      )
    )





  })



  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ----------------------------    OBVSERVES    ---------------------------------
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # ....... SHOW/HIDDEN EXPLICACION ........
  # ........................................

  #      .) Activar menú explicativo
  #      .) SOLO cuando se haya activado DIVISION (Comarques, Provincies,...)


  shiny::observe({

    shiny::validate(
      shiny::need(data_reactives$var_daily, 'no variable')
    )
    display_daily <- data_reactives$display_daily

    print(display_daily)

    if (display_daily == 'none') {
      shinyjs::hide('explanation_divisions')
    } else {
      shinyjs::show('explanation_divisions')
    }


  })




}
