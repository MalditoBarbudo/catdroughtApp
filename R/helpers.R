# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang) {

  app_translations

  id %>%
    purrr::map_chr(
      ~ app_translations %>%
        dplyr::filter(text_id == .x) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            .x
          } else {

            # ........ SI PROBLEM ENCODING .......
            # ....................................

            #    .) A veces SHINY no transforma a UTF-8
            #    .) La fórmula para hacerlo es
            #    .) Encoding(text) <- "UTF-8"

            text <- dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))

            Encoding(text) <- "UTF-8"
            text

          }
        }
    )
}

# ...... FUNCION TRANSLATE THESUAURUS .......
# ...........................................

#       .) Función que traducirá
#       .) ARGUMENTOS.
#                .) LANG = Lengua per definida en menu del NAV
#                .) ID = código que usarà el DICCIONARIO para saber QUE TRADUCIR
#                .) TYPE = description o units


translate_thesaurus_app <- function(id, lang, type) {
  id %>%
    purrr::map_chr(
      ~ catdrought_var_thes %>%
        dplyr::filter(var_id == .x ) %>% {
          data_filtered <- .

          if (nrow(data_filtered) < 1) {
            .x
          } else {

            switch (type,
                    'description' = text <- dplyr::pull(data_filtered, !! rlang::sym(glue::glue("var_description_help_{lang}"))),
                    'units' = text <- dplyr::pull(data_filtered, !! rlang::sym(glue::glue("var_units_{lang}")))
            )

            Encoding(text) <- "UTF-8"
            text

          }
        }
    )
}
