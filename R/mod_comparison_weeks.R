mod_comparison_weeks_sidepanel_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(h3(tags$u(tags$em("Select test week and reference unit:"))),
          mod_single_weeks_inp_testing_ui(id),
          shiny.semantic::selectInput(ns("sel_ref_unit"),
                                      "Reference Unit:",
                                      choices = list_ref_unit),
          mod_single_break("small")
          )
}
