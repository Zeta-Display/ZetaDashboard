mod_comparison_weeks_sidepanel_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(h3(tags$u(tags$em("Select test week and reference unit:"))),
          mod_single_weeks_inp_testing_ui(id),
          shiny.semantic::selectInput(ns("sel_ref_unit"),
                                      "Reference Unit:",
                                      choices = list_ref_unit),
          mod_break_vspace("small")
          )
}
mod_comparison_data_subset0_srv <- function(id) {
  moduleServer(id, function(input, output, session) {
    range_weeks <- shiny::reactive({
      range_weeks <- paste0("2022", input[["week_testing"]])
      range_weeks <- as.numeric(range_weeks)
      range_weeks
    })
    data_weekly <- shiny::reactive({
      id_data_weekly <- grep(paste0("weekly_v", input[["week_testing"]]),
                             names(data_list))
      data_list[[id_data_weekly]] %>%
        dplyr::filter(.data$vecka %in% range_weeks()) %>%
        dplyr::select(.data$butik,
                      .data$vecka,
                      .data$antal_kvitton,
                      tidyselect::contains(input[["sel_ref_unit"]]))
    })
    data_daily <- shiny::reactive({
      id_data_daily <- grep(paste0("daily_v", input[["week_testing"]]),
                            names(data_list))
      data_list[[id_data_daily]] %>%
        dplyr::filter(.data$vecka %in% range_weeks()) %>%
        dplyr::select(.data$butik,
                      .data$vecka,
                      .data$datum,
                      .data$antal_kvitton,
                      tidyselect::contains(input[["sel_ref_unit"]]))
    })
    list(data_weekly = data_weekly,
         data_daily =  data_daily)
  }
  )
}
mod_comparison_get_ref_unit_srv <- function(id) {
  moduleServer(id, function(input, output, session) {
    shiny::reactive({input$sel_ref_unit})})
}
