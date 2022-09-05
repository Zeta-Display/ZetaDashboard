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
    semanticPage(
      titlePanel(title = div(img(src = "www/logo.png",
                                 height = "10%",
                                 width = "10%",
                                 align = "right"),
                             "Hemköp sales analysis")),
      sidebar_layout(
        sidebar_panel(
          mod_data_settings_in("data_settings"),
          mod_data_settings_out_cntrl_weeks("data_settings"),
          mod_break("large"),
          mod_hypothesis_testing_in("hypothesis_settings"),
          mod_break("large"),
          div(
            action_button("radio_update", "Update Selection"),
            textOutput("radio_result"),
          )
        ),
        main_panel()
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
