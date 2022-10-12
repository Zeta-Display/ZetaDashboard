mod_comparison_data_anova_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    h4(tags$u(tags$em("Select ANOVA settings: "))),
    mod_break_vspace("small"),
    shiny.semantic::flowLayout(
      tags$label(tags$b(`for` = "radio_time_frequency",
                        style = "font-size:12.5px",
                        "Frequency:")),
      shiny.semantic::multiple_radio(
        ns("time_frequency"),
        "",
        choices = c("daily", "hourly"),
        choices_value = c("daily", "hourly"),
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
    ),
    mod_break_vspace("small"),
    shiny.semantic::flowLayout(
      shiny.semantic::checkbox_input(ns("logical_yscale"),
                                     "Logarithmic y-scale?",
                                     is_marked = FALSE),
      shiny.semantic::numeric_input(ns("log_scl_cor"),
                                    "Downscale by: ",
                                    value = 0,
                                    min = 0,
                                    max = 100)
      ,
      min_cell_width = "100px",
      max_cell_width = "200px",
      column_gap = "10px"),
    mod_break_vspace("small")
  )
}
mod_comparison_data_anova_ou <- function(id, plot_name, out_name) {
  ns <- shiny::NS(id)
  shiny::tagList(
    h5(tags$u(tags$em("ANOVA - estimated effects: "))),
    shiny::plotOutput(ns(plot_name)),
    mod_break_vspace("small"),
    htmltools::tags$head(htmltools::tags$style(paste0("#", ns(out_name),
                                                      "{color: black;
                                                        font-size: 10px;
                                                        font-style: verbatim;}")
    )
    ),
    h5(tags$u(tags$em("ANOVA - estimation output: "))),
    shiny::verbatimTextOutput(ns(out_name))
  )
}
mod_comparison_data_anova <-  function(id, data_subsets,
                                       test_butiks,
                                       test_week) {
  shiny::moduleServer(id, function(input, output, session) {
    data_anova_subset <- shiny::reactive({
      names_to_choose <- names(data_subsets)
      id <- grep("daily", names_to_choose)
      data_subsets[[id]]()
    })
    shiny::reactive({
      data_anova_out <- data_anova_subset()
      id_total <- which(grepl("TOTAL", names(data_anova_out)))
      data_anova_out <- data_anova_out[-c(id_total)]
      id_total <- which(grepl("antal_kvitton", names(data_anova_out)))
      data_anova_out <- data_anova_out[-c(id_total)]

      id_no_pivot <- which(!grepl("(butik|vecka|datum)",
                                  names(data_anova_out)))
      data_anova_out <- data_anova_out %>%
        tidyr::pivot_longer(id_no_pivot,
                            names_to = "unit",
                            values_to = "y")

      test_week_range  <- as.numeric(test_week()) + 202200

      data_anova_out <- data_anova_out %>%
        dplyr::group_by(across(any_of(c("butik",
                                        "unit",
                                        "vecka",
                                        "datum")))) %>%
        dplyr::summarize(y_m = mean(.data$y)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(butik_type = ifelse(.data$butik %in%  test_butiks(),
                                          "test", "cntrl")) %>%
        dplyr::mutate(vecka_type = ifelse(.data$vecka %in% test_week_range,
                                          "kampanj", "no-kampanj"))

      if (isTRUE(input[["logical_yscale"]])) {
        tmp_val <- log(data_anova_out$y_m)
        rep_val <- min(tmp_val[is.finite(tmp_val)]) - input[["log_scl_cor"]]
        tmp_val <- replace(tmp_val, tmp_val == 0, rep_val)
        tmp_val <- replace(tmp_val, is.infinite(tmp_val), rep_val)
        data_anova_out$y_m <- tmp_val
      }
      data_anova_out$datum <- NULL
      data_anova_out$butik_type <- factor(data_anova_out$butik_type,
                                          levels = c("cntrl", "test"))
      data_anova_out$vecka_type <- factor(data_anova_out$vecka_type,
                                          levels = c("no-kampanj", "kampanj"))
      attr(data_anova_out, which = "ref_unit") <- attr(data_anova_subset(),
                                                       which = "ref_unit")
      data_anova_out
    })
  })
}
mod_comparison_data_run_anova <- function(id, data_subset, anova_name) {
  stopifnot(is.reactive(data_subset))
  shiny::moduleServer(id, function(input, output, session) {
    mod_out <- shiny::reactive({
      options(constrasts = c("contr.sum", "contr.poly"))
      mod_form <- as.formula(y_m ~ vecka_type * butik_type + (1|butik:unit))
      model_out <- lmerTest::lmer(formula = mod_form,
                                  data = data_subset())
    })
    output[[anova_name]] <- shiny::renderPrint({
      summary(mod_out())
    })
    return(mod_out)
  })
}
mod_comparison_data_plot_anova <- function(id,
                                           data_subset,
                                           plot_name,
                                           anova_summary) {
  stopifnot(shiny::is.reactive(data_subset))
  shiny::moduleServer(id, function(input, output, session) {
    output[[plot_name]] <- shiny::renderPlot({
      coef_anova <- lme4::fixef(anova_summary())
      y_start <- coef_anova[1] + coef_anova[3]
      y_end   <- coef_anova[2] + coef_anova[2] + coef_anova[3]
      data_plot <- data_subset()
      ref_unit <- attr(data_plot, which = "ref_unit")


      ggplot2::ggplot(data_plot,
                      ggplot2::aes(x = vecka_type, y = y_m,
                                   group = butik_type,
                                   col = butik_type)) +
        ggplot2::stat_summary(fun = mean, geom = "line") +
        zeta_theme_02 +
        ggplot2::scale_color_manual(values = zeta_color_palette_02[c(1, 2)],
                                    labels = c("control", "testing")) +
        ggplot2::labs(title = "Esimated effects",
                      subtitle = "(solid: with signage - dashed: without signage)",
                      x = "vecka type",
                      y = paste0("average of ", ref_unit),
                      col = "Butik type") +
        ggplot2::theme(legend.position = "top") +
        ggplot2::geom_segment(ggplot2::aes(x = "no-kampanj",
                                           xend = "kampanj",
                                           y = y_start,
                                           yend = y_end),
                              linetype = 2,
                              colour = zeta_color_palette_02[c(2)]) +
        ggplot2::scale_x_discrete(drop = FALSE)
    })
  })
}

