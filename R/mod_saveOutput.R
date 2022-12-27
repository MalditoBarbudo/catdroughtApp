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

    print(names(main_data_reactives$raster_selected_daily))
    print(main_data_reactives$raster_selected_daily)

    ns <- session$ns

    lang_declared <- lang()
    dates_lang <- switch(
      lang_declared,
      'cat' = 'ca',
      'spa' = 'es',
      'eng' = 'en'
    )

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(12,
          shiny::fluidRow(shiny::HTML(translate_app("raster_download", lang_declared)), style = "text-align: center; padding: 15px;"),
            shiny::br(),
            shiny::fluidRow(align = 'center',
                # download button
                shiny::downloadButton(
                  ns('download_raster_daily'),
                  translate_app('download_raster_label', lang())
                )
             )
          ),
          shiny::br(),
          shiny::column(12,
            shiny::fluidRow(shiny::HTML(translate_app("csv_download", lang_declared)), style = "text-align: center; padding: 15px;"),
            shiny::br(),
            shiny::fluidRow(align = 'center',
                shiny::downloadButton(
                  ns('download_series_daily'),
                  translate_app('download_series_label', lang())
                )
            )
          )

      ) # end Fluid Row
    ) # end TAGLIST

  }) # END Render UI


  # ................  funcion DATAINPUT ...............
  # ...................................................

  #      .) Función OBTIENE DATOS para el CSV
  #      .) La usaré en el readr::write_csv
  #      .) CONDICION
  #               .) SIN DIVISIONES -> NONE
  #               .) CON DIVISIONES -> !NONE

  #      .) SIN DIVISIONES
  #               .) Obtengo la geometría
  #               .) Obtengo los datos
  #                    .) Los datos creo columna LON LAT
  #                    .) Elimino POINT ID, que no aporta nada

  #      .) CON DIVISIONES (comaracs, municipios,..)
  #               .) Obtengo datos

  dataInput <- reactive({

       display_daily <- data_reactives$display_daily

       if (display_daily == 'none') {

         result_geom <- main_data_reactives$timeseries_data$sf$geometry
         result_data <- main_data_reactives$timeseries_data$data %>%
                          dplyr::mutate(
                            lon_WGS84 = sf::st_coordinates(result_geom)[,1],
                            lat_WGS84 = sf::st_coordinates(result_geom)[,2]
                           ) %>% dplyr::select(-point_id)

       } else {
         result_data <- main_data_reactives$timeseries_data$data
       }

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
      result_data <- main_data_reactives$raster_selected_daily

      raster::writeRaster(
        result_data, filename = file,
        format = 'GTiff', overwrite = TRUE, # bylayer = TRUE # suffix = 'names',
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

      readr::write_csv(
        dataInput(), file = file
      )


    }
  )


}
