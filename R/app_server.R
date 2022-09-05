#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dates_taken <- mod_data_settings_get_date_srv("data_settings")
  mod_data_settings_date_out_srv("data_settings",
                                 dates_taken$start,
                                 dates_taken$end,
                                 dates_taken$num_weeks)
}
