#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dates_taken <- mod_single_weeks_get_dates_srv("control_test_weeks")
  mod_single_weeks_out_dates_srv("control_test_weeks",
                                 dates_taken$start,
                                 dates_taken$end,
                                 dates_taken$num_weeks)
  data_subsets0 <- mod_single_data_subset0_srv("control_test_weeks")
  ref_unit_taken <- mod_single_get_ref_unit_srv("control_test_weeks")

  data_subsets1 <- mod_single_data_subset1_srv("data_subset1",
                                               data_subsets0)

  data_subsets2 <- mod_single_data_subset2_srv("data_subset1",
                                               data_subsets1)

  data_subset3 <- mod_data_subset_time_unit_srv("data_subset3", data_subsets2)
  adjust_input_data_subset("data_subset3", data_subset3)

  output$table <- reactable::renderReactable({
    reactable::reactable(data_subset3())
  })

  mod_single_plot_data_srv("data_subset3",
                           data_subset3,
                           dates_taken$test_week)
  htest_out <- mod_hypothesis_testing_srv(id = "hypothesis_tests_tab1",
                                          data_subsets2$data_weekly_count,
                                          ref_unit_taken,
                                          dates_taken$test_week,
                                          type = "vecka")
  mod_hypothesis_testing_write_ou_srv(id = "hypothesis_tests_single",
                                      htest_out)
  #
  #
  #
  #
  #
  adjust_input_control_stores("testcase1")
  adjust_input_control_stores("testcase2")
  data0 <- mod_comparison_data_subset0_srv("comparison_weeks")
  ref_unit_taken2  <- mod_comparison_get_ref_unit_srv("comparison_weeks")

  data_1_TC1 <- mod_comparison_data_subset1_srv("testcase1",
                                                data0,
                                                ref_unit_taken2)
  data_1_TC2 <- mod_comparison_data_subset1_srv("testcase2",
                                                data0,
                                                ref_unit_taken2)
  test_butik_taken1 <- mod_comparison_get_test_butik_srv("testcase1")
  test_butik_taken2 <- mod_comparison_get_test_butik_srv("testcase2")

  data_2_TC1 <- mod_data_subset_time_unit_srv("data_subset2_c",
                                              data_1_TC1)
  adjust_input_data_subset("data_subset2_c", data_2_TC1)
  data_2_TC2 <- mod_data_subset_time_unit_srv("data_subset2_c",
                                              data_1_TC2)

  mod_comparison_plot_data_srv("data_subset2_c",
                               data_2_TC1,
                               test_butik_taken1,
                               "data_plot_sc1")
  mod_comparison_plot_data_srv("data_subset2_c",
                               data_2_TC2,
                               test_butik_taken2,
                               "data_plot_sc2")

  output$table_c1 <- reactable::renderReactable({
    reactable::reactable(data_2_TC1())
  })
  htest_out1 <- mod_hypothesis_testing_srv(id = "hypothesis_tests_tab2",
                                           data_1_TC1$data_weekly_count,
                                           ref_unit_taken2,
                                           test_butik_taken1,
                                           type = "butik")
  mod_hypothesis_testing_write_ou_srv(id = "hypothesis_tests_TC1",
                                      htest_out1)
  output$table_c2 <- reactable::renderReactable({
    reactable::reactable(data_2_TC2())
  })
  htest_out2 <- mod_hypothesis_testing_srv(id = "hypothesis_tests_tab2",
                                           data_1_TC2$data_weekly_count,
                                           ref_unit_taken2,
                                           test_butik_taken2,
                                           type = "butik")
  mod_hypothesis_testing_write_ou_srv(id = "hypothesis_tests_TC2",
                                      htest_out2)
}
