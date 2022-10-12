mod_single_data_subset2_srv <- function(id, data_subsets) {
  shiny::moduleServer(id, function(input, output, session) {
    data_weekly_count <- shiny::reactive({
      data_out <- data_subsets[[1]]() %>%
        dplyr::select(-dplyr::contains("convrate")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("count")), round, 0)
      repl_tmp <- "_count"
      data_out$butik <- replace_butik_lab_with_var(data_out$butik)

      data_out <- rename_data_set_with_var(data_out, skip = 1:3,
                                           ref_unit = paste0(input$sel_ref_unit,
                                                             repl_tmp))
      data_out
    })
    data_daily_count <- shiny::reactive({
      data_out <- data_subsets[[2]]() %>%
        dplyr::select(-dplyr::contains("convrate")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("count")), round, 0)
      data_out$butik <- replace_butik_lab_with_var(data_out$butik)

      repl_tmp <- "_count"
      data_out <- rename_data_set_with_var(data_out, skip = 1:4,
                                           ref_unit = paste0(input$sel_ref_unit,
                                                             repl_tmp))
      data_out
    })
    data_weekly_convrate <- shiny::reactive({
      data_out <- data_subsets[[1]]() %>%
        dplyr::select(-dplyr::contains("count")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("convrate")), round, 4)
      repl_tmp <- "_convrate"
      data_out$butik <- replace_butik_lab_with_var(data_out$butik)

      data_out <- rename_data_set_with_var(data_out, skip = 1:3,
                                           ref_unit = paste0(input$sel_ref_unit,
                                                             repl_tmp))
      data_out
    })
    data_daily_convrate <- shiny::reactive({
      data_out <- data_subsets[[2]]() %>%
        dplyr::select(-dplyr::contains("count")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("convrate")), round, 4)
      repl_tmp <- "_convrate"
      data_out$butik <- replace_butik_lab_with_var(data_out$butik)

      data_out <- rename_data_set_with_var(data_out, skip = 1:4,
                                           ref_unit = paste0(input$sel_ref_unit,
                                                             repl_tmp))
      data_out
    })
    list(data_weekly_count = data_weekly_count,
         data_weekly_convrate = data_weekly_convrate,
         data_daily_count = data_daily_count,
         data_daily_convrate = data_daily_convrate)
  })
}

