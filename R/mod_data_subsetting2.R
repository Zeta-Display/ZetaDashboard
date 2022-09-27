mod_data_subset2_srv <- function(id, data_subsets) {
  shiny::moduleServer(id, function(input, output, session) {
    data_weekly_count <- shiny::reactive({
      data_out <- data_subsets[[1]]() %>%
        dplyr::select(-dplyr::contains("frac")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("count")), round, 0)
      repl_tmp <- "_count"
      butik_taken <-  names(match_list_butik) == unique(data_out$butik)
      data_out$butik <- unname(unlist(match_list_butik[butik_taken]))

      ref_unit2 <- paste0("sales_",input$sel_ref_unit, repl_tmp)
      names(data_out)[-c(1,2,3)] <- substring(names(data_out)[-c(1,2,3)],
                                              first = nchar(ref_unit2) + 2)
      data_out
    })
    data_daily_count <- shiny::reactive({
      data_out <- data_subsets[[2]]() %>%
        dplyr::select(-dplyr::contains("frac")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("count")), round, 0)
      repl_tmp <- "_count"
      butik_taken <-  names(match_list_butik) == unique(data_out$butik)
      data_out$butik <- unname(unlist(match_list_butik[butik_taken]))

      ref_unit2 <- paste0("sales_",input$sel_ref_unit, repl_tmp)
      names(data_out)[-c(1,2,3,4)] <- substring(names(data_out)[-c(1,2,3,4)],
                                              first = nchar(ref_unit2) + 2)
      data_out
    })
    data_weekly_fraction <- shiny::reactive({
      data_out <- data_subsets[[1]]() %>%
        dplyr::select(-dplyr::contains("count")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("frac")), round, 2)
      repl_tmp <- "_frac"
      butik_taken <-  names(match_list_butik) == unique(data_out$butik)
      data_out$butik <- unname(unlist(match_list_butik[butik_taken]))

      ref_unit2 <- paste0("sales_",input$sel_ref_unit, repl_tmp)
      names(data_out)[-c(1,2,3)] <- substring(names(data_out)[-c(1,2,3)],
                                              first = nchar(ref_unit2) + 2)
      data_out
    })
    data_daily_fraction <- shiny::reactive({
      data_out <- data_subsets[[2]]() %>%
        dplyr::select(-dplyr::contains("count")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("frac")), round, 2)
      repl_tmp <- "_frac"
      butik_taken <-  names(match_list_butik) == unique(data_out$butik)
      data_out$butik <- unname(unlist(match_list_butik[butik_taken]))

      ref_unit2 <- paste0("sales_",input$sel_ref_unit, repl_tmp)
      names(data_out)[-c(1,2,3,4)] <- substring(names(data_out)[-c(1,2,3,4)],
                                              first = nchar(ref_unit2) + 2)
      data_out
    })
    list(data_weekly_count = data_weekly_count,
         data_weekly_fraction = data_weekly_fraction,
         data_daily_count = data_daily_count,
         data_daily_fraction = data_daily_fraction)
  })
}
