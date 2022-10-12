mod_comparison_data_descriptive_mainpanel_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h4(tags$u(tags$em("Descriptive Analysis - sales over time:"))),
    shiny.semantic::selectInput(ns("dep_vars_select"),
                                "Variable to plot:",
                                choices = "TOTAL",
                                multiple = FALSE,
                                width = "400px"),
    shiny.semantic::flowLayout(
      tags$label(tags$b(`for` = "radio_time_frequency",
                        style = "font-size:12.5px",
                        "Frequency:")),
      shiny.semantic::multiple_radio(
        ns("time_frequency"),
        "",
        choices = c("weekly", "daily"),
        choices_value = c("weekly", "daily"),
        selected = "weekly", position = "grouped",
        type = "radio"),
      mod_break_vspace("small"),
      tags$label(tags$b(`for` = "yscale",
                        style = "font-size:12.5px",
                        "Unit:")),
      shiny.semantic::multiple_radio(
        ns("yscale"),
        "",
        choices = c("count", "convrate"),
        choices_value = c("count", "convrate"),
        selected = "count",
        position = "inline"
      ),
      min_cell_width = "50px",
      max_cell_width = "70px",
      column_gap = "10px"
    )
  )
}
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
mod_comparison_data_descriptive_ou <- function(id, num,
                                               plot_name,
                                               tabl_name) {
  ns <- shiny::NS(id)
  tagList(
    plotly::plotlyOutput(ns(plot_name)),
    h4(paste0("Data subset used for scenario ", num)),
    reactable::reactableOutput(ns(tabl_name)),
    mod_break_vspace("small")
  )
}
mod_comparison_data_descriptive_srv <- function(id, data_subsets) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      names_to_choose <- names(data_subsets)
      id <- grep(input[["time_frequency"]], names_to_choose)
      data_out <- data_subsets[[id]]()
      ref_unit_taken <- paste0(attr(data_out, which = "ref_unit"),
                               "_", input[["yscale"]])
      digits_round <- ifelse(input[["yscale"]] == "count", 0, 4)
      data_out <- data_out %>%
        dplyr::select(dplyr::any_of(c("butik",
                                      "vecka",
                                      "datum",
                                      "timme",
                                      "antal_kvitton")),
                      dplyr::contains(ref_unit_taken)) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains(ref_unit_taken)),
                         round, digits_round)
      id_first_zoner_var <- min(which(grepl(ref_unit_taken, names(data_out))))
      data_out <- rename_data_set_with_var(data_out,
                                           skip = 1:(id_first_zoner_var - 1),
                                           ref_unit = ref_unit_taken)
      attr(data_out, which = "frequency") <- input[["time_frequency"]]
      attr(data_out, which = "yscale")    <- input[["yscale"]]
      data_out
    })
  })
}
adjust_comparison_input_data_subset <- function(id, data_subset) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data_subset(), {
      choices_vars <- setdiff(names(data_subset()),
                              c("butik", "vecka", "datum", "antal_kvitton"))
      shiny.semantic::updateSelectInput(session,
                                        inputId = "dep_vars_select",
                                        choices = choices_vars)
    })
  })
}
#' plot_benchmark Server Functions
#'
#' @noRd
mod_comparison_plot_data_srv <- function(id, data_subset,
                                         test_group,
                                         test_week,
                                         plot_name,
                                         tabl_name) {
  stopifnot(shiny::is.reactive(data_subset))
  stopifnot(shiny::is.reactive(test_group))
  shiny::moduleServer(id, function(input, output, session) {
    output[[plot_name]] <- plotly::renderPlotly({
      data_names_info  <- names(data_subset())
      var_dep_taken   <- data_names_info[grepl(input[["dep_vars_select"]],
                                               data_names_info)]
      data_ggplot <- get_ggplot_data_c(data_subset,
                                       y_var = var_dep_taken,
                                       test_group = test_group,
                                       test_week = test_week)
      plot_out <- generate_ggplot_c(data_set = data_ggplot,
                                    y_var = var_dep_taken)
      generate_plotly_c(ggplot_to_use = plot_out)
    })
    output[[tabl_name]] <- reactable::renderReactable({
      reactable::reactable(data_subset())
    })
  })
}
generate_ggplot_c <- function(data_set,
                              y_var) {
  frequency   <- attr(data_set, which = "frequency")
  y_lab_taken <- attr(data_set, which = "yscale")
  ggplot_out <- ggplot2::ggplot(data_set, ggplot2::aes(fill = group,
                                                       alpha = vecka_type)) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$vecka_time,
                                   y = .data[[y_var]]),
                      stat = "identity",
                      position = "dodge",
                      na.rm = TRUE) +
    zeta_theme_02 +
    ggplot2::ggtitle(label = paste0("Sales - ", y_var)) +
    ggplot2::scale_fill_manual(values = zeta_color_palette_02[c(1, 2)]) +
    ggplot2::scale_alpha_discrete(range = c(1.0, 0.5), guide = "none") +
    ggplot2::theme(legend.position = "top")

  if (frequency == "weekly") {
    ggplot_out <- ggplot_out + ggplot2::labs(y = y_lab_taken, x = "weeks") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 80))
  } else if (frequency == "daily") {
    date_breaks_taken <- compute_date_breaks(data_set, num_breaks = 10)
    ggplot_out <- ggplot_out + ggplot2::labs(y = y_lab_taken, x = "days") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 80)) +
      ggplot2::scale_x_discrete(breaks = date_breaks_taken)
    # ggplot2::scale_x_date(date_breaks = "1 day")
  }
  ggplot_out
}
get_ggplot_data_c <- function(data_set,
                              y_var,
                              test_group,
                              test_week) {
  stopifnot(shiny::is.reactive(data_set))
  stopifnot(shiny::is.reactive(test_group))

  frequency <- attr(data_set(), which = "frequency")
  yscale    <- attr(data_set(), which = "yscale")
  year_const <- 202200
  range_testweek <- as.numeric(test_week()) + year_const

  data_out <- data_set()
  data_out$`group` <- ifelse(data_out$butik %in% test_group(),
                             "testing", "control")
  data_out$`vecka_type` <- ifelse(data_out$vecka %in% range_testweek,
                                  "kampanj", "no-kampanj")

  if(frequency == "daily") {
    # data_out$`vecka_time` <- as.Date(data_out$datum)
    data_out$`vecka_time` <- as.factor(data_out$datum)
  } else if (frequency == "weekly") {
    data_out$`vecka_time`    <- paste0("2022-", data_out$vecka - year_const)
  }
  if (yscale == "count") {
    data_out <- data_out %>%
      dplyr::select(tidyselect::all_of(c(y_var,
                                         "vecka_time",
                                         "group",
                                         "vecka_type"))) %>%
      dplyr::group_by(group, vecka_type, vecka_time) %>%
      dplyr::summarise("{y_var}" := sum(.data[[y_var]]))
  } else if (yscale == "convrate") {
    data_out <- data_out %>%
      dplyr::select(tidyselect::all_of(c("antal_kvitton",
                                         y_var,
                                         "vecka_time",
                                         "group",
                                         "vecka_type"))) %>%
      dplyr::group_by(group, vecka_type, vecka_time) %>%
      dplyr::mutate(weights = antal_kvitton/sum(antal_kvitton)) %>%
      dplyr::mutate("{y_var}" := sum(.data[[y_var]] * .data[["weights"]])) %>%
      dplyr::select(-c("antal_kvitton", "weights")) %>%
      dplyr::distinct()
  }
  attr(data_out, which = "frequency") <- attr(data_set(), which = "frequency")
  attr(data_out, which = "yscale")    <- attr(data_set(), which = "yscale")

  data_out
}
generate_plotly_c <- function(ggplot_to_use) {
  plotly::ggplotly(ggplot_to_use) %>%
    plotly::layout(legend = list(
      # orientation = "h",
      #                            x = -0.5,
      #                            y = 1.5,
      font = list(size = 8)))
}
