mod_comparison_testcase_ui <- function(id, num) {
  ns <- shiny::NS(id)
  tagList(h3(tags$u(tags$em(paste0("Define test scenario ", num, ":")))),
          shiny.semantic::selectInput(ns("sel_butik_cntrl"),
                                      "Control butik:",
                                      choices = list_butik[c(1, 4)]),
          mod_break_vspace("small"),
          shiny.semantic::selectInput(ns("sel_butik_test"),
                                      "Test butik:",
                                      choices = list_butik[c(2, 3)],
                                      selected = list_butik[c(2, 3)],
                                      multiple = TRUE)
  )
}
adjust_input_control_stores <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input[["sel_butik_cntrl"]], {
      if (input[["sel_butik_cntrl"]] == "HEMKÖP FALUN C") {
        choices_vars <- list_butik[c(5, 6)]
        selected_vars <- list_butik[5]
      } else if (input[["sel_butik_cntrl"]] == "HEMKÖP OSKARSTRÖM BLÅKLINTSVÄGEN") {
        choices_vars <- list_butik[c(2, 3)]
        selected_vars <- list_butik[2]
      }
      shiny.semantic::updateSelectInput(session,
                                        inputId = "sel_butik_test",
                                        choices = choices_vars,
                                        selected = selected_vars)
    })
  })
}
mod_comparison_data_subset1_srv <- function(id, data_subsets, ref_unit_taken) {
  stopifnot(is.reactive(data_subsets[[1]]) || is.reactive(data_subsets[[2]]))
  shiny::moduleServer(id, function(input, output, session) {
    range_butik <- shiny::reactive({
      range_butik <-c(input[["sel_butik_cntrl"]],
                      input[["sel_butik_test"]])
      range_butik
    })
    data_weekly_count <- shiny::reactive({
      data_out <- data_subsets[[1]]() %>%
        dplyr::filter(.data$butik %in% range_butik()) %>%
        dplyr::select(-dplyr::contains("frac")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("count")), round, 0)
      repl_tmp <- "_count"
      data_out$butik <- unlist(unname(match_list_butik[data_out$butik]))

      ref_unit2 <- paste0("sales_", ref_unit_taken(), repl_tmp)
      names(data_out)[-c(1,2,3)] <- substring(names(data_out)[-c(1,2,3)],
                                              first = nchar(ref_unit2) + 2)
      attr(data_out, which = "var_type") <- "count"
      data_out
    })
    data_daily_count <- shiny::reactive({
      # browser()
      data_out <- data_subsets[[2]]() %>%
        dplyr::filter(.data$butik %in% range_butik()) %>%
        dplyr::select(-dplyr::contains("frac")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("count")), round, 0)
      repl_tmp <- "_count"
      data_out$butik <- unlist(unname(match_list_butik[data_out$butik]))

      ref_unit2 <- paste0("sales_", ref_unit_taken(), repl_tmp)
      names(data_out)[-c(1,2,3,4)] <- substring(names(data_out)[-c(1,2,3,4)],
                                                first = nchar(ref_unit2) + 2)
      attr(data_out, which = "var_type") <- "count"
      data_out
    })
    data_weekly_fraction <- shiny::reactive({
      data_out <- data_subsets[[1]]() %>%
        dplyr::filter(.data$butik %in% range_butik())  %>%
        dplyr::select(-dplyr::contains("count")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("frac")), round, 4)
      repl_tmp <- "_frac"
      data_out$butik <- unlist(unname(match_list_butik[data_out$butik]))

      ref_unit2 <- paste0("sales_", ref_unit_taken(), repl_tmp)
      names(data_out)[-c(1,2,3)] <- substring(names(data_out)[-c(1,2,3)],
                                              first = nchar(ref_unit2) + 2)
      attr(data_out, which = "var_type") <- "fraction"
      data_out
    })
    data_daily_fraction <- shiny::reactive({
      data_out <- data_subsets[[2]]() %>%
        dplyr::filter(.data$butik %in% range_butik())  %>%
        dplyr::select(-dplyr::contains("count")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("frac")), round, 4)
      repl_tmp <- "_frac"
      data_out$butik <- unlist(unname(match_list_butik[data_out$butik]))

      ref_unit2 <- paste0("sales_", ref_unit_taken(), repl_tmp)
      names(data_out)[-c(1,2,3,4)] <- substring(names(data_out)[-c(1,2,3,4)],
                                                first = nchar(ref_unit2) + 2)
      attr(data_out, which = "var_type") <- "fraction"
      data_out
    })
    list(data_weekly_count = data_weekly_count,
         data_weekly_fraction = data_weekly_fraction,
         data_daily_count = data_daily_count,
         data_daily_fraction = data_daily_fraction)
  })
}
mod_comparison_get_test_butik_srv <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      unlist(unname(match_list_butik[list_butik %in% input$sel_butik_test]))
    })
  })
}
