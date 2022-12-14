#' Barplot of sales
#'
#' Either in convrates or counts
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_single_data_plot_ou <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h3("Sales over time"),
    plotly::plotlyOutput(ns("data_plot")),
    mod_break_vspace("small")
  )
}
#' plot_benchmark Server Functions
#'
#' @noRd
mod_single_plot_data_srv <- function(id, data_subset, test_week) {
  stopifnot(shiny::is.reactive(data_subset))
  stopifnot(shiny::is.reactive(test_week))
  shiny::moduleServer(id, function(input, output, session) {
    output$data_plot <- plotly::renderPlotly({
      data_names_info  <- names(data_subset())
      var_dep_taken   <- data_names_info[grepl(input[["dep_vars_select"]],
                                               data_names_info)]
      frequency     <- ifelse(any(grepl("datum", data_names_info)),
                              "daily", "weekly")

      data_ggplot <- get_ggplot_data(data_subset,
                                     y_var = var_dep_taken,
                                     frequency = frequency,
                                     test_week = test_week)
      plot_out <- generate_ggplot(data_set = data_ggplot,
                                  y_var = var_dep_taken,
                                  y_lab = input[["yscale"]],
                                  frequency)
      generate_plotly(ggplot_to_use = plot_out)
    })
  })
}
generate_ggplot <- function(data_set,
                            y_var,
                            y_lab,
                            frequency) {
  ggplot_out <- ggplot2::ggplot(data_set, ggplot2::aes(fill = group)) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$vecka_time,
                                   y = .data[[y_var]]),
                      stat = "identity",
                      na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = zeta_color_palette_02[c(1, 2)]) +
    zeta_theme_02 +
    ggplot2::ggtitle(y_var)
  if (frequency == "weekly") {
    ggplot_out <- ggplot_out + ggplot2::labs(y = y_lab, x = "weeks") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 80))
  } else if (frequency == "daily") {
    date_breaks_taken <- compute_date_breaks(data_set, num_breaks = 12)
    ggplot_out <- ggplot_out + ggplot2::labs(y = y_lab, x = "days") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 80)) +
      ggplot2::scale_x_discrete(breaks = date_breaks_taken)
      # ggplot2::scale_x_date(date_breaks = date_breaks_taken)
  }
  ggplot_out
}
compute_date_breaks <- function(data_set, num_breaks) {
  nrow_data_uniq_dates <- length(unique(data_set$vecka_time))
  tmp_num <- max(1, ceiling(nrow_data_uniq_dates/num_breaks))
  ind_dates <- c(TRUE, rep(FALSE, times = tmp_num - 1))

  num_rep_f <- nrow_data_uniq_dates %/% tmp_num
  num_rep_r <- nrow_data_uniq_dates %% tmp_num
  ind_dates <- c(rep(ind_dates, times = num_rep_f),
                 ind_dates[1:num_rep_r])
  dates_out <- sort(unique(as.character(data_set$vecka_time)),
                    decreasing = TRUE)
  # paste0(tmp_num, " days")
  dates_out[ind_dates]
}
get_ggplot_data <- function(data_set,
                            y_var,
                            frequency,
                            test_week) {
  stopifnot(shiny::is.reactive(data_set))
  stopifnot(shiny::is.reactive(test_week))

  year_const <- 202200
  range_testweek <- as.numeric(test_week()) + year_const

  data_out <- data_set()
  data_out$group <- ifelse(data_out$vecka %in% range_testweek,
                           "testing",
                           "control")
  if(frequency == "daily") {
    # data_out$`vecka_time` <- as.Date(data_out$datum)
    data_out$`vecka_time` <- as.factor(data_out$datum)
  } else if (frequency == "weekly") {
    data_out$`vecka_time`    <- paste0("2022-", data_out$vecka - year_const)
  }
  data_out %>% dplyr::select(tidyselect::all_of(c(y_var,
                                                  "vecka_time",
                                                  "group")))
}
generate_plotly <- function(ggplot_to_use) {
  plotly::ggplotly(ggplot_to_use) %>%
    plotly::layout(legend = list(
      # orientation = "h",
      #                            x = -0.5,
      #                            y = 1.5,
      font = list(size = 10)))
}
