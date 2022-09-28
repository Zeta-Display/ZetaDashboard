# mod_comparison_data_subset1_srv <- function(id, data_subsets) {
#   shiny::moduleServer(id, function(input, output, session) {
#     data_weekly_scenario1 <- shiny::reactive({
#       browser()
#       data_subsets[["data_weekly"]] %>% dplyr::select(.data$butik,
#                                                       .data$vecka,
#                                                       .data$antal_kvitton,
#                                                       contains(input[[""]]))
#     })
#     data_daily_scenario1 <- shiny::reactive({
#       browser()
#       data_subsets[["data_daily"]] %>% dplyr::select(.data$butik,
#                                                      .data$vecka,
#                                                      .data$datum,
#                                                      .data$antal_kvitton,
#                                                      contains(input[[""]]))
#     })
#     data_weekly_scenario2 <- shiny::reactive({
#       browser()
#       data_subsets[["data_weekly"]] %>% dplyr::select(.data$butik,
#                                                       .data$vecka,
#                                                       .data$antal_kvitton,
#                                                       contains(input[[""]]))
#     })
#     data_daily_scenario2 <- shiny::reactive({
#       browser()
#       data_subsets[["data_daily"]] %>% dplyr::select(.data$butik,
#                                                      .data$vecka,
#                                                      .data$datum,
#                                                      .data$antal_kvitton,
#                                                      contains(input[[""]]))
#     })
#   })
# }
#   dataset_tests <- data_weekly_all[[1]] %>% dplyr::filter(.data$vecka == 202200 + 34,
#                                                       .data$butik %in% group1) %>%
# dplyr::select(.data$butik, .data$vecka, .data$antal_kvitton,
#               contains(paste0("kampanj", "_", "count")))

# tests_out <- perform_tests(dataset_tests,
#                            type = "butik",
#                            cntrl = group1[1],
#                            test = group1[2])
