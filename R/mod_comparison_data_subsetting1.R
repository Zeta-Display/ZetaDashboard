mod_comparison_testcase_ui <- function(id, num) {
  ns <- shiny::NS(id)
  tagList(h3(tags$u(tags$em(paste0("Define test scenario ", num, ":")))),
          shiny.semantic::selectInput(ns("sel_butik_cntrl"),
                                      "Control butik:",
                                      choices = butik_lab_cntr[[num]],
                                      selected = butik_lab_cntr[[num]][1],
                                      multiple = TRUE),
          mod_break_vspace("small"),
          shiny.semantic::selectInput(ns("sel_butik_test"),
                                      "Test butik:",
                                      choices = butik_lab_test[[num]],
                                      selected = butik_lab_test[[num]],
                                      multiple = TRUE)
  )
}
adjust_input_control_stores <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input[["sel_butik_cntrl"]], {
      # if (input[["sel_butik_cntrl"]] %in% butik_lab_cntr[["sma"]]) {
        # choices_vars <- butik_lab_test[["sma"]]
        # selected_vars <- butik_lab_test[["sma"]][1]
      # } else if (input[["sel_butik_cntrl"]] %in% butik_lab_cntr[["sto"]]) {
        # choices_vars <- butik_lab_test[["sto"]]
        # selected_vars <- butik_lab_test[["sto"]][1]
      # }
      # shiny.semantic::updateSelectInput(session,
      #                                   inputId = "sel_butik_test",
      #                                   choices = choices_vars,
      #                                   selected = selected_vars)
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
        dplyr::select(-dplyr::contains("convrate")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("count")), round, 0)
      repl_tmp <- "_count"
      data_out$butik <- replace_butik_lab_with_var(data_out$butik)

      data_out <- rename_data_set_with_var(data_out, skip = 3,
                                           ref_unit = paste0(ref_unit_taken(),
                                                             repl_tmp))
      attr(data_out, which = "var_type") <- "count"
      data_out
    })
    data_daily_count <- shiny::reactive({
      # browser()
      data_out <- data_subsets[[2]]() %>%
        dplyr::filter(.data$butik %in% range_butik()) %>%
        dplyr::select(-dplyr::contains("convrate")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("count")), round, 0)
      repl_tmp <- "_count"
      data_out$butik <- replace_butik_lab_with_var(data_out$butik)

      data_out <- rename_data_set_with_var(data_out, skip = 4,
                                           ref_unit = paste0(ref_unit_taken(),
                                                             repl_tmp))
      attr(data_out, which = "var_type") <- "count"
      data_out
    })
    data_weekly_convrate <- shiny::reactive({
      data_out <- data_subsets[[1]]() %>%
        dplyr::filter(.data$butik %in% range_butik())  %>%
        dplyr::select(-dplyr::contains("count")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("convrate")), round, 4)
      repl_tmp <- "_convrate"
      data_out$butik <- replace_butik_lab_with_var(data_out$butik)

      data_out <- rename_data_set_with_var(data_out, skip = 3,
                                           ref_unit = paste0(ref_unit_taken(),
                                                             repl_tmp))
      attr(data_out, which = "var_type") <- "convrate"
      data_out
    })
    data_daily_convrate <- shiny::reactive({
      data_out <- data_subsets[[2]]() %>%
        dplyr::filter(.data$butik %in% range_butik())  %>%
        dplyr::select(-dplyr::contains("count")) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("convrate")), round, 4)
      repl_tmp <- "_convrate"
      data_out$butik <- replace_butik_lab_with_var(data_out$butik)

      data_out <- rename_data_set_with_var(data_out, skip = 4,
                                           ref_unit = paste0(ref_unit_taken(),
                                                             repl_tmp))
      attr(data_out, which = "var_type") <- "convrate"
      data_out
    })
    list(data_weekly_count = data_weekly_count,
         data_weekly_convrate = data_weekly_convrate,
         data_daily_count = data_daily_count,
         data_daily_convrate = data_daily_convrate)
  })
}
mod_comparison_get_test_butik_srv <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      unlist(unname(butik_val_all[butik_lab_all %in% input$sel_butik_test]))
    })
  })
}
