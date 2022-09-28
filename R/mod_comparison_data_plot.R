#' Barplot of sales
#'
#' Either in fractions or counts
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comparison_data_plot_ou <- function(id, plot_name) {
  ns <- shiny::NS(id)
  tagList(
    plotly::plotlyOutput(ns(plot_name)),
    mod_break_vspace("small")
  )
}
#' plot_benchmark Server Functions
#'
#' @noRd
mod_comparison_plot_data_srv <- function(id, data_subset,
                                         test_group, plot_name) {
  stopifnot(shiny::is.reactive(data_subset))
  stopifnot(shiny::is.reactive(test_group))
  shiny::moduleServer(id, function(input, output, session) {
    output[[plot_name]] <- plotly::renderPlotly({
      data_names_info  <- names(data_subset())
      var_dep_taken   <- data_names_info[grepl(input[["dep_vars_select"]],
                                               data_names_info)]
      frequency     <- ifelse(any(grepl("datum", data_names_info)),
                              "daily", "weekly")

      data_ggplot <- get_ggplot_data_c(data_subset,
                                       y_var = var_dep_taken,
                                       frequency = frequency,
                                       test_group = test_group)
      plot_out <- generate_ggplot_c(data_set = data_ggplot,
                                    y_var = var_dep_taken,
                                    y_lab = input[["yscale"]],
                                    frequency)
      generate_plotly(ggplot_to_use = plot_out)
    })
  })
}
generate_ggplot_c <- function(data_set,
                              y_var,
                              y_lab,
                              frequency) {
  ggplot_out <- ggplot2::ggplot(data_set, ggplot2::aes(fill = group)) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$vecka_time,
                                   y = .data[[y_var]]),
                      stat = "identity",
                      position = "dodge",
                      na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = zeta_color_palette_02[c(1, 2)]) +
    zeta_theme_02 +
    ggplot2::ggtitle(label = paste0("Sales - ", y_var))
  if (frequency == "weekly") {
    ggplot_out <- ggplot_out + ggplot2::labs(y = y_lab, x = "weeks") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 80))
  } else if (frequency == "daily") {
    # date_breaks_taken <- compute_date_breaks(data_set, num_breaks = 12)
    ggplot_out <- ggplot_out + ggplot2::labs(y = y_lab, x = "days") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 80)) +
      ggplot2::scale_x_date(date_breaks = "1 day")
  }
  ggplot_out
}
get_ggplot_data_c <- function(data_set,
                              y_var,
                              frequency,
                              test_group) {
  stopifnot(shiny::is.reactive(data_set))
  stopifnot(shiny::is.reactive(test_group))

  data_out <- data_set()
  data_out$`group` <- ifelse(data_out$butik %in% test_group(),
                             "testing", "control")
  if(frequency == "daily") {
    data_out$`vecka_time` <- as.Date(data_out$datum)
  } else if (frequency == "weekly") {
    data_out$`vecka_time`    <- paste0("2022-", data_out$vecka - 202200)
  }
  if (attr(data_out, which = "var_type") == "count") {
    data_out %>%
      dplyr::select(tidyselect::all_of(c(y_var,
                                         "vecka_time",
                                         "group"))) %>%
      dplyr::group_by(group, vecka_time) %>%
      dplyr::summarise("{y_var}" := sum(.data[[y_var]]))
  } else if (attr(data_out, which = "var_type") == "fraction") {
    data_out %>%
      dplyr::select(tidyselect::all_of(c("antal_kvitton",
                                         y_var,
                                         "vecka_time",
                                         "group"))) %>%
      dplyr::group_by(group, vecka_time) %>%
      dplyr::mutate(weights = antal_kvitton/sum(antal_kvitton)) %>%
      dplyr::mutate("{y_var}" := sum(.data[[y_var]] * .data[["weights"]])) %>%
      dplyr::select(-c("antal_kvitton", "weights")) %>%
      dplyr::distinct()
  }
}
generate_plotly <- function(ggplot_to_use) {
  plotly::ggplotly(ggplot_to_use)
}
