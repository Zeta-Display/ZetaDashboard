#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dates_taken <- mod_weeks_get_dates_srv("control_test_weeks")
  mod_weeks_out_dates_srv("control_test_weeks",
                          dates_taken$start,
                          dates_taken$end,
                          dates_taken$num_weeks)
  data_subsets0  <- mod_single_data_subset0_srv("control_test_weeks")
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
  adjust_input_stores("testcase1")
  adjust_input_stores_control("testcase1")
  adjust_input_stores_test("testcase1")
  #
  adjust_input_stores("testcase2")
  adjust_input_stores_control("testcase2")
  adjust_input_stores_test("testcase2")
  #
  dates_taken <- mod_weeks_get_dates_srv("comparison_weeks")
  mod_weeks_out_dates_srv("comparison_weeks",
                          dates_taken$start,
                          dates_taken$end,
                          dates_taken$num_weeks)
  data0            <- mod_comparison_data_subset0_srv("comparison_weeks")
  # ref_unit_taken2  <- mod_comparison_get_ref_unit_srv("comparison_weeks")

  data_1_TC1 <- mod_comparison_data_subset1_srv("testcase1", data0)
  data_1_TC2 <- mod_comparison_data_subset1_srv("testcase2", data0)
  test_butik_taken1 <- mod_comparison_get_test_butik_srv("testcase1")
  test_butik_taken2 <- mod_comparison_get_test_butik_srv("testcase2")

  data_2_TC1 <- mod_comparison_data_descriptive_srv("data_descriptive_1",
                                                    data_1_TC1)
  data_2_TC2 <- mod_comparison_data_descriptive_srv("data_descriptive_2",
                                                    data_1_TC2)
  adjust_input_data_subset("data_descriptive_1", data_2_TC1)
  adjust_input_data_subset("data_descriptive_2", data_2_TC2)
  mod_comparison_plot_data_srv("data_descriptive_1",
                               data_2_TC1,
                               test_butik_taken1,
                               dates_taken$test_week,
                               "data_plot_sc1",
                               "table_c1")
  mod_comparison_plot_data_srv("data_descriptive_2",
                               data_2_TC2,
                               test_butik_taken2,
                               dates_taken$test_week,
                               "data_plot_sc2",
                               "table_c2")
  data_ANOVA_TC1 <- mod_comparison_data_anova("anova_1",
                                              data_subsets = data_1_TC1,
                                              test_butiks = test_butik_taken1,
                                              test_week = dates_taken$test_week)
  anova_summary_1 <- mod_comparison_data_run_anova("anova_1",
                                                   data_ANOVA_TC1,
                                                   "anova_out_1")
  mod_comparison_data_plot_anova("anova_1",
                                 data_ANOVA_TC1,
                                 "plot_anova_1",
                                 anova_summary_1)
  output$table_a1 <- reactable::renderReactable({
    reactable::reactable(data_ANOVA_TC1())
  })
  data_ANOVA_TC2 <- mod_comparison_data_anova("anova_2",
                                              data_subsets = data_1_TC2,
                                              test_butiks = test_butik_taken2,
                                              test_week = dates_taken$test_week)
  anova_summary_2 <- mod_comparison_data_run_anova("anova_2",
                                                   data_ANOVA_TC2,
                                                   "anova_out_2")
  mod_comparison_data_plot_anova("anova_2",
                                 data_ANOVA_TC2,
                                 "plot_anova_2",
                                 anova_summary_2)
  output$table_a2 <- reactable::renderReactable({
    reactable::reactable(data_ANOVA_TC2())
  })
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  #
  # htest_out1 <- mod_hypothesis_testing_srv(id = "hypothesis_tests_tab2",
  #                                          data_1_TC1$data_weekly_count,
  #                                          ref_unit_taken2,
  #                                          test_butik_taken1,
  #                                          type = "butik")
  # mod_hypothesis_testing_write_ou_srv(id = "hypothesis_tests_TC1",
  #                                     htest_out1)

  # htest_out2 <- mod_hypothesis_testing_srv(id = "hypothesis_tests_tab2",
  #                                          data_1_TC2$data_weekly_count,
  #                                          ref_unit_taken2,
  #                                          test_butik_taken2,
  #                                          type = "butik")
  # mod_hypothesis_testing_write_ou_srv(id = "hypothesis_tests_TC2",
  #                                     htest_out2)
}
