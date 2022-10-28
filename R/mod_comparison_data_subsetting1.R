mod_comparison_testcase_ui <- function(id, num) {
  ns <- shiny::NS(id)
  tagList(h3(tags$u(tags$em(paste0("Define test scenario ", num, ":")))),

          shiny.semantic::selectInput(ns("ref_unit"),
                                      "Reference Unit:",
                                      choices = list_ref_unit),
          tags$label(tags$b(`for` = "butik_size",
                            style = "font-size:12.5px",
                            "Size of butik:")),
          shiny.semantic::multiple_radio(ns("butik_size"),
                                         "",
                                         choices = c("sma", "stora"),
                                         position = "grouped"),
          shiny.semantic::selectInput(ns("sel_butik_cntrl"),
                                      "Control butik:",
                                      choices = butik_lab_both[[1]],
                                      selected = butik_lab_both[[1]][1],
                                      multiple = TRUE),
          shiny.semantic::selectInput(ns("sel_butik_test"),
                                      "Test butik:",
                                      choices = butik_lab_both[[1]],
                                      selected = butik_lab_both[[1]][2],
                                      multiple = TRUE),
          mod_break_vspace("small")
  )
}
adjust_input_stores <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input[["butik_size"]], {
      # browser()
      if (input[["butik_size"]] == "sma") {
      choices_vars_test   <- butik_lab_both[["sma"]]
      selected_vars_test  <- butik_lab_both[["sma"]][1]
      choices_vars_cntrl  <- butik_lab_both[["sma"]]
      selected_vars_cntrl <- butik_lab_both[["sma"]][2]
      # choices_vars_test <- butik_lab_test[["sma"]]
      # selected_vars_test <- butik_lab_test[["sma"]][1]
      # choices_vars_cntrl <- butik_lab_cntr[["sma"]]
      # selected_vars_cntrl <- butik_lab_cntr[["sma"]][1]
      } else if (input[["butik_size"]] == "stora") {
        choices_vars_test   <- butik_lab_both[["stora"]]
        selected_vars_test  <- butik_lab_both[["stora"]][1]
        choices_vars_cntrl  <- butik_lab_both[["stora"]]
        selected_vars_cntrl <- butik_lab_both[["stora"]][2]
        # choices_vars_test <- butik_lab_test[["stora"]]
        # selected_vars_test <- butik_lab_test[["stora"]][1]
        # choices_vars_cntrl <- butik_lab_cntr[["stora"]]
        # selected_vars_cntrl <- butik_lab_cntr[["stora"]][1]
      }
      shiny.semantic::updateSelectInput(session,
                                        inputId = "sel_butik_test",
                                        choices = choices_vars_test,
                                        selected = selected_vars_test)
      shiny.semantic::updateSelectInput(session,
                                        inputId = "sel_butik_cntrl",
                                        choices = choices_vars_cntrl,
                                        selected = selected_vars_cntrl)
    })
  })
}
adjust_input_stores_control <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input[["sel_butik_test"]], {
      selected_vars_cntrl <- setdiff(input[["sel_butik_cntrl"]],
                                     input[["sel_butik_test"]])
      if (!isTRUE(all.equal(input[["sel_butik_cntrl"]],
                            selected_vars_cntrl))) {
        shiny.semantic::updateSelectInput(session,
                                          inputId = "sel_butik_cntrl",
                                          selected = selected_vars_cntrl)
      }
    })
  })
}
adjust_input_stores_test <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input[["sel_butik_cntrl"]], {
      selected_vars_test <- setdiff(input[["sel_butik_test"]],
                                    input[["sel_butik_cntrl"]])
      if (!isTRUE(all.equal(input[["sel_butik_test"]], selected_vars_test))) {
        shiny.semantic::updateSelectInput(session,
                                          inputId = "sel_butik_test",
                                          selected = selected_vars_test)
      }
    })
  })
}
mod_comparison_data_subset1_srv <- function(id, data_subsets) {
  stopifnot(is.reactive(data_subsets[[1]]) || is.reactive(data_subsets[[2]]))
  shiny::moduleServer(id, function(input, output, session) {
    range_butik <- shiny::reactive({
      range_butik <-c(input[["sel_butik_cntrl"]],
                      input[["sel_butik_test"]])
      range_butik
    })
    data_weekly <- shiny::reactive({
      data_out <- data_subsets[[1]]() %>%
        dplyr::filter(.data$butik %in% range_butik()) %>%
        dplyr::select(dplyr::any_of(c("butik", "vecka", "datum",
                                      "timme", "antal_kvitton")),
                      dplyr::contains(input[["ref_unit"]]))

      data_out$butik <- replace_butik_lab_with_var(data_out$butik)
      attr(data_out, which = "ref_unit")  <- input[["ref_unit"]]
      data_out
    })
    data_daily <- shiny::reactive({

      data_out <- data_subsets[[2]]() %>%
        dplyr::filter(.data$butik %in% range_butik()) %>%
        dplyr::select(dplyr::any_of(c("butik", "vecka", "datum",
                                      "timme", "antal_kvitton")),
                      dplyr::contains(input[["ref_unit"]]))
      data_out$butik <- replace_butik_lab_with_var(data_out$butik)
      attr(data_out, which = "ref_unit")  <- input[["ref_unit"]]
      data_out
    })
    list(data_weekly = data_weekly,
         data_daily= data_daily)
  })
}
mod_comparison_get_test_butik_srv <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      unlist(unname(butik_val_all[butik_lab_all %in% input$sel_butik_test]))
    })
  })
}
