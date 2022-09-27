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
                           mod_single_weeks_sidepanel_ui("control_testing_weeks"),
                           mod_single_break("medium"),
                           mod_single_data_subset1_sidepanel_ui("data_subset1"),
                           mod_single_break("medium"),
                           mod_single_hypothesis_testing_in("hypothesis_testing")
                           ),
          conditionalPanel("input.analysis_all == 'tab_2'",
                           mod_comparison_weeks_sidepanel_ui("mod_comparison_weeks_sidepanel_ui"))
        ),
        main_panel = shiny.semantic::main_panel(
          shiny.semantic::tabset(
            tabs =
              list(
                list(menu = "Analysis - single shop",
                     content = div(
                       mod_single_break("small"),
                       mod_single_data_subset3_main_panel_ui("data_subset3"),
                       mod_single_break("medium"),
                       mod_single_data_plot_ou("data_subset3"),
                       mod_single_break("medium"),
                       h3("Data subset used"),
                       reactable::reactableOutput("table"),
                       mod_single_break("medium"),
                       h3("Hypothesis tests"),
                       mod_single_hypothesis_testing_ou("hypothesis_testing")),
                     id = "tab_1"),
                list(menu = "Analysis - comparison of shops",
                     content = "true content",
                     id = "tab_2")
              ),
            active = "tab_1",
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


library(shiny.semantic)
