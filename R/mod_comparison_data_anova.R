mod_comparison_data_anova_ui <- function(id, plot_name, out_name) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns(plot_name)),
    mod_break_vspace("small"),
    htmltools::tags$head(htmltools::tags$style(paste0("#", ns(out_name),
                                                      "{color: black;
                                                        font-size: 10px;
                                                        font-style: verbatim;}")
    )
    ),
    shiny::verbatimTextOutput(ns(out_name))
  )
}
mod_comparison_data_anova <-  function(id, data_subsets,
                                       test_butiks,
                                       test_week,
                                       take_log) {
  shiny::moduleServer(id, function(input, output, session) {
    data_anova_subset <- shiny::reactive({
      names_to_choose <- names(data_subsets)
      id_data_set1 <- grep(input[["yscale"]], names_to_choose)
      id_data_set2 <- grep("daily", names_to_choose)
      id <- intersect(id_data_set1, id_data_set2)
      data_subsets[[id]]()
    })
    shiny::reactive({
      data_anova_out <- data_anova_subset()

      id_total <- which(grepl("TOTAL", names(data_anova_out)))
      data_anova_out <- data_anova_out[-c(id_total)]
      id_total <- which(grepl("antal_kvitton", names(data_anova_out)))
      data_anova_out <- data_anova_out[-c(id_total)]

      id_no_pivot <- which(!grepl("(butik|vecka|datum)", names(data_anova_out)))
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

      if (isTRUE(take_log)) {
        data_anova_out$y_m <- log(data_anova_out$y_m)
      }
      data_anova_out$datum <- NULL

      data_anova_out
    })
  })
}
mod_comparison_data_run_anova <- function(id, data_subset, anova_name) {
  stopifnot(is.reactive(data_subset))
  shiny::moduleServer(id, function(input, output, session) {
    output[[anova_name]] <- shiny::renderPrint({
      mod_form <- as.formula(y_m ~ vecka_type * butik_type + (1|butik:unit))
      model_out <- lmerTest::lmer(formula = mod_form,
                                  data = data_subset())
      summary(model_out)
    })
  })
}
mod_comparison_data_plot_anova <- function(id,
                                           data_subset,
                                           ref_unit,
                                           plot_name) {
  stopifnot(shiny::is.reactive(data_subset))
  shiny::moduleServer(id, function(input, output, session) {
    output[[plot_name]] <- shiny::renderPlot({
      data_plot <- data_subset()
      data_plot$butik_type <- factor(data_plot$butik_type,
                                     levels = c("test", "cntrl"),
                                     ordered = TRUE)
      data_plot$vecka_type <- factor(data_plot$vecka_type,
                                     levels = c("no-kampanj", "kampanj"),
                                     ordered = TRUE)
      ggplot2::ggplot(data_plot,
                      ggplot2::aes(x = vecka_type, y = y_m,
                                   group = butik_type,
                                   col = butik_type)) +
        ggplot2::stat_summary(fun = mean, geom = "line") +
        zeta_theme_02 +
        ggplot2::scale_color_manual(values = zeta_color_palette_02[c(2, 1)]) +
        ggplot2::labs(title = "Esimated effects",
                      x = "vecka type",
                      y = paste0("average of ", ref_unit()))
    })
  })
}

