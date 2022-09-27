#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dates_taken <- mod_weeks_get_dates_srv("control_testing_weeks")
  mod_weeks_out_dates_srv("control_testing_weeks",
                          dates_taken$start,
                          dates_taken$end,
                          dates_taken$num_weeks)
  data_subsets0 <- mod_data_subset0_srv("control_testing_weeks")

  ref_unit_taken <- mod_data_subset1_get_ref_unit_srv("data_subset1")
  data_subsets1 <- mod_data_subset1_srv("data_subset1",
                                        data_subsets0)

  data_subsets2 <- mod_data_subset2_srv("data_subset1",
                                        data_subsets1)

  data_subset3 <- mod_data_subset3_srv("data_subset3", data_subsets2)
  adjust_input_data_subset("data_subset3", data_subset3)

  output$table <- reactable::renderReactable({
    reactable::reactable(data_subset3())
  })

  mod_plot_data_srv("data_subset3",
                    data_subset3,
                    dates_taken$test_week)
  htest_out <- mod_hypothesis_testing_srv(id = "hypothesis_testing",
                                          data_subsets2$data_weekly_count,
                                          ref_unit_taken,
                                          dates_taken$test_week)
  mod_hypothesis_testing_write_ou_srv(id = "hypothesis_testing",
                                      htest_out)
}
