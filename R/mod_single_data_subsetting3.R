mod_single_data_subset3_main_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h3(tags$u(tags$em("Time scale (frequency) and sales unit:"))),
    shiny.semantic::selectInput(ns("dep_vars_select"),
                                "Variable to plot:",
                                choices = "TOTAL",
                                multiple = FALSE,
                                width = "400px"),
    shiny.semantic::flowLayout(
      tags$label(tags$b(`for` = "radio_time_frequency",
                        style = "font-size:12.5px",
                        "Frequency:")),
      shiny.semantic::multiple_radio(
        ns("time_frequency"),
        "",
        choices = c("weekly", "daily"),
        choices_value = c("weekly", "daily"),
        selected = "weekly", position = "grouped",
        type = "radio"),
      mod_single_break("small"),
      tags$label(tags$b(`for` = "yscale",
                        style = "font-size:12.5px",
                        "Unit:")),
      shiny.semantic::multiple_radio(
        ns("yscale"),
        "",
        choices = c("count", "fraction"),
        choices_value = c("count", "fraction"),
        selected = "count",
        position = "inline"
      ),
      min_cell_width = "50px",
      max_cell_width = "70px",
      column_gap = "10px"
    )
    # shiny.semantic::selectInput(
    #   ns("yscale"),
    #   "Unit:",
    #   choices = list(count = "cnt", fraction = "frac"),
    #   selected = "frac")
    # tags$label(tags$b(`for` = "yscale",
    #                   style = "font-size:12.5px",
    #                   "Unit:")),
    # multiple_radio(
    #   ns("yscale"),
    #   "",
    #   choices = c("count", "fraction"),
    #   choices_value = c("cnt", "frac"),
    #   selected = "frac",
    #   position = "inline"
    # )
  )
}
mod_single_data_subset3_srv <- function(id, data_subsets) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      names_to_choose <- names(data_subsets)
      id_data_set1 <- grep(input[["yscale"]], names_to_choose)
      id_data_set2 <- grep(input[["time_frequency"]], names_to_choose)
      id <- intersect(id_data_set1, id_data_set2)
      data_subsets[[id]]()
    })
  })
}
adjust_input_data_subset <- function(id, data_subset) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data_subset(), {
      choices_vars <- setdiff(names(data_subset()),
                              c("butik", "vecka", "datum", "antal_kvitton"))
      shiny.semantic::updateSelectInput(session,
                                        inputId = "dep_vars_select",
                                        choices = choices_vars)
    })
  })
}
