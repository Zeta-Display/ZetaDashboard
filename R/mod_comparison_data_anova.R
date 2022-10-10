mod_comparison_data_anova_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    htmltools::tags$head(htmltools::tags$style(paste0("#", id,
                                                      "{color: black;
                                                        font-size: 11px;
                                                        font-style: verbatim;}")
    )
    ),
    shiny::verbatimTextOutput(id)
  )
}
mod_comparison_data_anova <-  function(id, data_subsets,
                                       # week_num,
                                       # ref_type,
                                       # var_type,
                                       test_butiks,
                                       # cntrl_week_range,
                                       test_week,
                                       # num_test,
                                       take_log) {
  # stopifnot(is.reactive(data_set))
  shiny::moduleServer(id, function(input, output, session) {
    data_anova_subset <- shiny::reactive({
      names_to_choose <- names(data_subsets)
      id_data_set1 <- grep(input[["yscale"]], names_to_choose)
      # id_data_set2 <- grep(input[["time_frequency"]], names_to_choose)
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
                                          "test", "cntrl"))

      if (isTRUE(take_log)) {
        data_anova_out$y_m <- log(data_anova_out$y_m)
      }
      data_anova_out$datum <- NULL
      data_anova_out
    })
  })
}
mod_comparison_data_get_anova <- function(id, data_subset) {
  stopifnot(is.reactive(data_subset))
  shiny::moduleServer(id, function(input, output, session) {
    data_anova_subset <- shiny::reactive({
      mod_form <- as.formula(y_m ~ vecka_type * butik_type + (1|butik:unit))
      model_out <- lmerTest::lmer(formula = mod_form,
                                  data = data_subset())
      summary(model_out)
    })
  })
}
