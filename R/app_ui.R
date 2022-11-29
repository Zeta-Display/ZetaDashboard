#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny.semantic::semanticPage(
      titlePanel(title = div(img(src = "www/logo.png",
                                 height = "10%",
                                 width = "10%",
                                 align = "right"),
                             "Hemköp sales analysis")),
      sidebar_layout(
        sidebar_panel = shiny.semantic::sidebar_panel(
          conditionalPanel("input.analysis_all == 'tab_1'",
                           mod_single_weeks_sidepanel_ui("control_test_weeks"),
                           mod_break_vspace("medium"),
                           mod_single_data_subset1_sidepanel_ui("data_subset1")#,
                           # mod_break_vspace("medium"),
                           # mod_hypothesis_testing_in("hypothesis_tests_tab1")
          ),
          conditionalPanel("input.analysis_all == 'tab_2'",
                           mod_comparison_weeks_sidepanel_ui("comparison_weeks"),
                           # mod_comparison_data_anova_ui("comparison_weeks"),
                           mod_comparison_testcase_ui("testcase1", 1),
                           mod_comparison_testcase_ui("testcase2", 2),
                           mod_break_vspace("medium"),
                           # mod_hypothesis_testing_in("hypothesis_tests_tab2")
                           auth0::logoutButton(label = "Log out",
                                               id = "my_logout")
                           )
        ),
        main_panel = shiny.semantic::main_panel(
          shiny.semantic::tabset(
            tabs =
              list(
                list(menu = "Analysis - single shop",
                     content = div(
                       mod_break_vspace("small"),
                       mod_data_subset_time_unit_mainpanel_ui("data_subset3"),
                       mod_break_vspace("medium"),
                       mod_single_data_plot_ou("data_subset3"),
                       mod_break_vspace("medium"),
                       h3("Data subset used"),
                       reactable::reactableOutput("table")#,
                       # mod_break_vspace("medium"),
                       # h3("Hypothesis tests"),
                       # mod_hypothesis_testing_ou("hypothesis_tests_single")
                       ),
                     id = "tab_1"),
                list(menu = "Analysis - comparison of shops",
                     content = div(
                       shiny.semantic::flow_layout(
                         htmltools::tagList(
                           h3(tags$u(tags$em("Test scenario 1:"))),
                           mod_comparison_data_descriptive_mainpanel_ui("data_descriptive_1"),
                           mod_comparison_data_descriptive_ou("data_descriptive_1",
                                                              1,
                                                              "data_plot_sc1",
                                                              "table_c1"),
                           mod_comparison_data_anova_ui("anova_1"),
                           mod_comparison_data_anova_ou("anova_1",
                                                        "plot_anova_1",
                                                        "anova_out_1")
                           # h4("Hypothesis tests for scenario 1")
                           # mod_hypothesis_testing_ou("hypothesis_tests_TC1")
                         ),
                         htmltools::tagList(
                           h3(tags$u(tags$em("Test scenario 2:"))),
                           mod_comparison_data_descriptive_mainpanel_ui("data_descriptive_2"),
                           mod_comparison_data_descriptive_ou("data_descriptive_2",
                                                              2,
                                                              "data_plot_sc2",
                                                              "table_c2"),
                           mod_comparison_data_anova_ui("anova_2"),
                           mod_comparison_data_anova_ou("anova_2",
                                                        "plot_anova_2",
                                                        "anova_out_2")
                           # h4("Hypothesis tests for scenario 2"),
                           # mod_hypothesis_testing_ou("hypothesis_tests_TC2")
                           ),
                         min_cell_width = "450px",
                         max_cell_width = "1fr"
                       )
                     ),
                     id = "tab_2")
              ),
            active = "tab_2",
            id = "analysis_all"
          )
        )
      )
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext="png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Hemköp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
