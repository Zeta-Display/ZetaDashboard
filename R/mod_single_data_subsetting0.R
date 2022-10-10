mod_single_weeks_sidepanel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    mod_weeks_inp_control_ui(id),
    mod_weeks_out_dates_ui(id),
    mod_break_vspace("small"),
    mod_weeks_inp_testing_ui(id),
  )
}
mod_single_data_subset0_srv <- function(id) {
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
