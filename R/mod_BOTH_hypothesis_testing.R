#' Inputs related to data settings
#'
#' @param id character to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a  \code{htmltools::\link[htmltools]{tagList}} container with
#'   \itemize{
#'      \item radio buttons to set time series frequency of displayed sales
#'      \item range input of weeks to use in control periods
#'      \item radio buttons to set the unit of the dependent variable (y-axis)
#'   }
mod_hypothesis_testing_in <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h3(tags$u(tags$em("Hypthesis Testing: "))),
    shiny.semantic::multiple_radio(
      ns("type_test"),
      "Select hypothesis type:",
      choices = c("Proportional-standard", "Fisher"),
      choices_value = c("proportional", "fisher"),
      position = "grouped"
    ),
    shiny.semantic::multiple_radio(
      ns("alternative_type"),
      "Select alternative hypothesis type:",
      choices = c("equal", "greater", "less"),
      choices_value = c("two.sided", "greater", "less"),
      position = "grouped"
    ),
    shiny.semantic::numeric_input(
      ns("confidence"),
      "Select confidence level:",
      value = 0.95,
      min = 0.0001,
      max = 0.9999
    )
  )
}
mod_hypothesis_testing_srv <- function(id,
                                       data_subset,
                                       ref_unit,
                                       test_groups,
                                       type_group_var) {
  stopifnot(shiny::is.reactive(data_subset))
  stopifnot(shiny::is.reactive(test_groups))
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      options <- list()
      if (type_group_var == "vecka") {
        test_group_full <- as.numeric(paste0("2022",  test_groups()))
        cntr_group_full <- data_subset() %>% dplyr::pull(.data[[type_group_var]])
        cntr_group_full <- setdiff(as.numeric(cntr_group_full),
                                   test_group_full)
      } else if (type_group_var == "butik") {
        test_group_full <- test_groups()
        cntr_group_full <- data_subset() %>% dplyr::pull(.data[[type_group_var]])
        cntr_group_full <- setdiff(cntr_group_full,
                                   test_group_full)
      }

      options_taken <- list(conf_level = input[["confidence"]],
                            alternative = input[["alternative_type"]])
      test_out <- ZetaAnalyticsTB::perform_tests(data_set = data_subset(),
                                                 type_test = input[["type_test"]],
                                                 type_group_var =type_group_var,
                                                 cntrl_group = cntr_group_full,
                                                 test_group  = test_group_full,
                                                 options = options_taken)
      test_out
    })
  })
}
mod_hypothesis_testing_write_ou_srv <- function(id, list_reactive_htests) {
  stopifnot(shiny::is.reactive(list_reactive_htests))
  shiny::moduleServer(id, function(input, output, session) {
    output$testresults <- renderUI({
      tmp_names <- names(list_reactive_htests())
      num_names <- length(tmp_names)
      tmp_out <- mapply(list,
                        tmp_names, list_reactive_htests(),
                        SIMPLIFY = FALSE)
      out <- lapply(tmp_out, function(x) {
        list(htmltools::h5(x[[1]]),
             shiny::renderPrint({x[[2]][[1]]}),
             shiny::renderPrint({x[[2]][[2]]}))
      })
      out
    })
  })
}
mod_hypothesis_testing_ou <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("testresults"))
}
