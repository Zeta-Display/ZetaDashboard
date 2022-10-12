mod_comparison_weeks_sidepanel_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(mod_weeks_inp_control_ui(id),
          mod_weeks_out_dates_ui(id),
          mod_weeks_inp_testing_ui(id),
          mod_break_vspace("small")
          )
}
mod_comparison_data_subset0_srv <- function(id) {
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
        dplyr::filter(.data$vecka %in% range_weeks()) %>%
        dplyr::select(.data$butik,
                      .data$vecka,
                      .data$antal_kvitton,
                      dplyr::everything())
                      # ,
                      # tidyselect::contains(input[["sel_ref_unit"]]))
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
                      dplyr::everything())
                      # ,
                      # tidyselect::contains(input[["sel_ref_unit"]]))
    })
    # browser()
    # data_weekly_sma <- shiny::reactive({
    #   id_data_weekly <- grep(paste0("sma_weekly_v", input[["week_testing"]]),
    #                          names(data_list))
    #   data_list[[id_data_weekly]] %>%
    #     dplyr::filter(.data$vecka %in% range_weeks()) %>%
    #     dplyr::select(.data$butik,
    #                   .data$vecka,
    #                   .data$antal_kvitton,
    #                   tidyselect::contains(input[["sel_ref_unit"]]))
    # })
    # data_daily_sma <- shiny::reactive({
    #   id_data_daily <- grep(paste0("sma_daily_v", input[["week_testing"]]),
    #                         names(data_list))
    #   data_list[[id_data_daily]] %>%
    #     dplyr::filter(.data$vecka %in% range_weeks()) %>%
    #     dplyr::select(.data$butik,
    #                   .data$vecka,
    #                   .data$datum,
    #                   .data$antal_kvitton,
    #                   tidyselect::contains(input[["sel_ref_unit"]]))
    # })
    # data_weekly_stora <- shiny::reactive({
    #   id_data_weekly <- grep(paste0("sto_weekly_v", input[["week_testing"]]),
    #                          names(data_list))
    #   data_list[[id_data_weekly]] %>%
    #     dplyr::filter(.data$vecka %in% range_weeks()) %>%
    #     dplyr::select(.data$butik,
    #                   .data$vecka,
    #                   .data$antal_kvitton,
    #                   tidyselect::contains(input[["sel_ref_unit"]]))
    # })
    # data_daily_stora <- shiny::reactive({
    #   id_data_daily <- grep(paste0("sto_daily_v", input[["week_testing"]]),
    #                         names(data_list))
    #   data_list[[id_data_daily]] %>%
    #     dplyr::filter(.data$vecka %in% range_weeks()) %>%
    #     dplyr::select(.data$butik,
    #                   .data$vecka,
    #                   .data$datum,
    #                   .data$antal_kvitton,
    #                   tidyselect::contains(input[["sel_ref_unit"]]))
    # })
    # list(sma = list(data_weekly = data_weekly_sma,
    #                 data_daily =  data_daily_sma),
    #      stora = list(data_weekly = data_weekly_stora,
    #                   data_daily =  data_daily_stora))
    list(data_weekly = data_weekly,
         data_daily =  data_daily)
  }
  )
}
# mod_comparison_get_ref_unit_srv <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     shiny::reactive({input$sel_ref_unit})})
# }
