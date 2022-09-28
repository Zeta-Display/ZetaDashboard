mod_single_data_subset1_sidepanel_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(h3(tags$u(tags$em("Butik and Reference Unit:"))),
          shiny.semantic::selectInput(ns("sel_butik"),
                                      "Butik:",
                                      choices = list_butik),
          mod_single_break("small"),
          shiny.semantic::selectInput(ns("sel_ref_unit"),
                                      "Reference Unit:",
                                      choices = list_ref_unit)

  )
}
mod_single_data_subset1_srv <- function(id, data_subsets) {
  stopifnot(is.reactive(data_subsets[[1]]) || is.reactive(data_subsets[[2]]))
  shiny::moduleServer(id, function(input, output, session) {
    data_out_weekly <- shiny::reactive({
      data_out <- data_subsets[[1]]() %>%
        dplyr::filter(.data$butik == input$sel_butik) %>%
        dplyr::select(.data$butik,
                      .data$vecka,
                      .data$antal_kvitton,
                      tidyselect::contains(input$sel_ref_unit))
      data_out
    })
    data_out_daily <- shiny::reactive({
      data_out <- data_subsets[[2]]() %>%
        dplyr::filter(.data$butik == input$sel_butik) %>%
        dplyr::select(.data$butik,
                      .data$vecka,
                      .data$datum,
                      .data$antal_kvitton,
                      tidyselect::contains(input$sel_ref_unit))
      data_out
    })
    list(data_weekly = data_out_weekly,
         data_daily  = data_out_daily)
  }
  )
}
mod_single_get_ref_unit_srv <- function(id) {
  moduleServer(id, function(input, output, session) {
    shiny::reactive({input$sel_ref_unit})})
}
