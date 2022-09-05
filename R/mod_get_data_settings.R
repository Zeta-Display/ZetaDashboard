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
mod_data_settings_in <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    h3(tags$u(tags$em("Data Settings: "))),
    div(
      shiny.semantic::multiple_radio(
        ns("radio_time_frequency"),
        "Frequency:",
        c("daily", "weekly"),
        c("time_frq_dly", "time_frq_wly"),
        "time_frq_dly", position = "grouped",
        type = "radio"
      ),
      tags$br(),
      multiple_radio(
        ns("radio_scale_y"),
        "Unit:",
        c("count", "fraction"),
        c("count_dly", "count_wly"),
        "count_wly", position = "grouped"
      ),
      tags$br(),
      tags$label(`for` = "range_time_frq",
                 style = "font-size:12.5px",
                 "Control weeks in 2022:"),
      shiny.semantic::slider_input(ns("range_time_frq"),
                                   min = 1,
                                   max = 10,
                                   value = 3,
                                   step = 1),
    )
  )
}
mod_data_settings_out_cntrl_weeks <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    textOutput(ns("date_num_weeks")),
    # p("Selected time - control weeks:"),
    textOutput(ns("date_begin")),
    textOutput(ns("date_end"))
  )
}
mod_data_settings_get_date_srv <- function(id, start_week_test = 34) {
  moduleServer(id, function(input, output, session) {
    tmp_date_pattern <- paste0("%Y%W%u")
    tmp_date_str <- reactive({
      start_week_control <- start_week_test - 1 - input[["range_time_frq"]]
      as.Date(paste0("2022", start_week_control, "1"),
              tmp_date_pattern)
    })
    tmp_num_weeks <- reactive({input[["range_time_frq"]]})
    tmp_date_end <- as.Date(paste0("2022", start_week_test, "7"),
                            tmp_date_pattern)
    list(start = tmp_date_str,
         end = tmp_date_end,
         num_weeks = tmp_num_weeks)
  }
  )
}
mod_data_settings_date_out_srv <- function(id,
                                           dates_start,
                                           dates_end,
                                           dates_num_weeks) {
  stopifnot(is.reactive(dates_start), is.reactive(dates_num_weeks))
  moduleServer(id, function(input, output, session) {
    output$date_num_weeks <- renderText({
      paste("No. weeks: ", dates_num_weeks())
      })
    output$date_begin <- renderText({
      paste("Start date: ", dates_start())
    })
    output$date_end   <- renderText({
      paste("End date: ", dates_end)
    })
  })
}
