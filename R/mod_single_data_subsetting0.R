mod_single_weeks_sidepanel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    mod_single_weeks_inp_control_ui(id),
    mod_single_weeks_out_dates_ui(id),
    mod_break_vspace("small"),
    mod_single_weeks_inp_testing_ui(id),
  )
}
mod_single_weeks_inp_control_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h3(tags$u(tags$em("Control vs. Test Weeks in 2022:"))),
    tags$br(),
    tags$label(tags$b(`for` = "range_time_frq",
                      style = "font-size:12.5px",
                      "Control Weeks:")),
    shiny.semantic::slider_input(ns("range_time_frq"),
                                 min = 1,
                                 max = 10,
                                 value = 3,
                                 step = 1)
  )
}
mod_single_weeks_inp_testing_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny.semantic::selectInput(ns("week_testing"),
                              "Testing Week:",
                              choices = c("week 34" = 34,
                                          "week 35" = 35,
                                          "week 36" = 36),
                              # "week 37" = 37),
                              selected = 34,
                              multiple = FALSE)
}
mod_single_weeks_out_dates_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    textOutput(ns("date_num_weeks")),
    # p("Selected time - control weeks:"),
    textOutput(ns("date_begin")),
    textOutput(ns("date_end"))
  )
}
mod_single_weeks_get_dates_srv <- function(id, start_test_weeks_all = 34) {
  shiny::moduleServer(id, function(input, output, session) {
    tmp_date_pattern <- paste0("%Y%W%u")
    tmp_date_start <- shiny::reactive({
      start_week_control <- start_test_weeks_all - 1 - input[["range_time_frq"]]
      as.Date(paste0("2022", start_week_control, "1"),
              tmp_date_pattern)
    })
    tmp_date_end <- as.Date(paste0("2022", start_test_weeks_all, "7"),
                            tmp_date_pattern)

    tmp_num_weeks <- shiny::reactive({input[["range_time_frq"]]})
    tmp_test_week <- shiny::reactive({input[["week_testing"]]})

    list(start = tmp_date_start,
         end = tmp_date_end,
         num_weeks = tmp_num_weeks,
         test_week = tmp_test_week)
  }
  )
}
mod_single_weeks_out_dates_srv <- function(id,
                                           dates_start,
                                           dates_end,
                                           dates_num_weeks) {
  stopifnot(is.reactive(dates_start), is.reactive(dates_num_weeks))
  shiny::moduleServer(id, function(input, output, session) {
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
mod_single_data_subset0_srv <- function(id) {
  # stopifnot(is.reactive(butik) && is.reactive(ref_unit))
  moduleServer(id, function(input, output, session) {

    range_weeks <- shiny::reactive({
      range_weeks <- c(input[["week_testing"]],
                       seq(from = 33, to = 33 - input[["range_time_frq"]] + 1))
      range_weeks <- paste0("2022", range_weeks)
      range_weeks <- as.numeric(range_weeks)
      range_weeks
    })
    data_weekly <- shiny::reactive({
      id_data_weekly <- grep(paste0("weekly_v", input[["week_testing"]]),
                             names(data_list))
      data_list[[id_data_weekly]] %>%
        dplyr::filter(.data$vecka %in% range_weeks())
    })
    data_daily <- shiny::reactive({
      id_data_daily <- grep(paste0("daily_v", input[["week_testing"]]),
                            names(data_list))
      data_list[[id_data_daily]] %>%
        dplyr::filter(.data$vecka %in% range_weeks())
    })
    list(data_weekly = data_weekly,
         data_daily =  data_daily)
  }
  )
}
