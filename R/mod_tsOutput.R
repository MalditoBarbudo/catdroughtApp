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
    dygraphs::dygraphOutput(ns('timeseries_daily'))
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

    timeseries_data
  })

}
