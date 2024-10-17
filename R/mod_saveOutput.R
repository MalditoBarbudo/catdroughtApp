#' @title mod_saveOutput and mod_save
#'
#' @description module for saving the data (raster or ts)
#'
#' @param id shiny id
#'
#' @export
mod_saveOutput <- function(id) {
  # ns
  ns <- shiny::NS(id)
  # ui
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(ns("save_container"))
  )
}

#' mod_save
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param main_data_reactives,data_reactives reactives needed
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_saveOutput
mod_save <- function(
  input, output, session,
  main_data_reactives, data_reactives,
  lang
) {

  ## renderUI ####
  output$save_container <- shiny::renderUI({

    ns <- session$ns

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6, align = 'center',
          # download button
          shiny::downloadButton(
            ns('download_raster_daily'),
            translate_app('download_raster_label', lang())
          )
        ),
        shiny::column(
          6, align = 'center',
          shiny::downloadButton(
            ns('download_series_daily'),
            translate_app('download_series_label', lang())
          )
        )
      )
    )
  })

  # Download handlers
  output$download_raster_daily <- shiny::downloadHandler(
    filename = function() {

      date_daily <- data_reactives$date_daily

      file_name <- glue::glue(
        "catdrought_raster_1km_{date_daily}.tif"
      )

      return(file_name)
    },
    content = function(file) {

      # data length
      # result_data <- main_data_reactives$raster_selected_daily
      date_daily <- data_reactives$date_daily
      result_data <- catdroughtdb$get_raster(date_daily, 'raster')
      terra::writeRaster(
        terra::rast(result_data), filename = file,
        filetype = 'GTiff', overwrite = TRUE
      )
    }
  )

  output$download_series_daily <- shiny::downloadHandler(
    filename = function() {
      var_daily <- data_reactives$var_daily

      file_name <- glue::glue(
        "catdrought_ts_{var_daily}_1km.csv"
      )

      return(file_name)
    },
    content = function(file) {
      result_data <- main_data_reactives$timeseries_data$data
      readr::write_csv(
        result_data, file = file
      )
    }
  )


}
