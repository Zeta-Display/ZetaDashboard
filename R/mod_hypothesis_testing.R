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
    multiple_radio(
      ns("radio"),
      "Select hypothesis type:",
      choices = c("Binomial", "Fisher", "Boschloo", "Barnard"),
      values = c("htest_bin", "htest_fsh", "htest_bos", "htest_bar"),
      "first", position = "grouped"
    )
  )
}
mod_hypothesis_testing_srv <- function(id,
                                       data_subset,
                                       ref_unit,
                                       test_periods) {
  stopifnot(shiny::is.reactive(data_subset))
  stopifnot(shiny::is.reactive(test_periods))
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      test_period_full <- as.numeric(paste0("2022",  test_periods()))
      cntr_period_full <- data_subset() %>% dplyr::pull(.data$vecka)
      cntr_period_full <- setdiff(as.numeric(cntr_period_full),
                                  test_periods())
      test_out <- ZetaAnalyticsTB::perform_tests(data_set = data_subset(),
                                                 type = "vecka",
                                                 cntrl = cntr_period_full,
                                                 test  = test_period_full)
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
        list(htmltools::h3(x[[1]]),
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
