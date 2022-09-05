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
