# Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiZHJqYW5hcHBlbCIsImEiOiJjazljdXN4bjEwMWFwM2ZtdzlicnN5d3UxIn0.inbHEfpk1fUFBOid3hE6uw')
global <- quote({
  library(ZetaAnalyticsTB)
  fn_names <- list.files("data/")
  fn_pths  <- file.path("data", fn_names)
  fn_num   <- length(fn_pths)

  data_list <- vector("list", fn_num)

  for (i in 1:fn_num) {
    data_list[[i]]  <- tibble::as_tibble(read.csv(fn_pths[i],
                                                  stringsAsFactors = FALSE,
                                                  fileEncoding = "utf8"))
  }
  names(data_list) <- substr(fn_names, 1, stop = (nchar(fn_names) - 4))



  weeks_inp_choices <- c("week 34" = 34, "week 35" = 35,
                         "week 36" = 36, "week 37" = 37,
                         "week 38" = 38, "week 39" = 39,
                         "week 40" = 40, "week 41" = 41)
  weeks_inp_selected <- 34

  match_list <- jsonlite::fromJSON("inst/meta/match_list.json")
  # list_butik <- unlist(match_list$butik)
  match_list_butik <- match_list$butik

  match_list_butik_sma <-match_list_butik$sma
  match_list_butik_sma_cntr <- match_list_butik_sma$control
  match_list_butik_sma_test <- match_list_butik_sma$testing
  match_list_butik_sma_all  <- c(match_list_butik_sma_cntr,
                                 match_list_butik_sma_test)
  match_list_butik_sto <-match_list_butik$stora
  match_list_butik_sto_cntr <- match_list_butik_sto$control
  match_list_butik_sto_test <- match_list_butik_sto$testing
  match_list_butik_sto_all  <- c(match_list_butik_sto_cntr,
                                 match_list_butik_sto_test)

  match_list_butik_all <- c(match_list_butik_sma_all,
                            match_list_butik_sto_all)
  # butik_lab_all <- names(match_list_butik_sto_all)
  # butik_val_all <- match_list_butik_sto_all
  butik_lab_all <- names(match_list_butik_all)
  butik_val_all <- match_list_butik_all

  butik_lab_cntr <- list(sma = names(match_list_butik_sma_cntr),
                         stora = names(match_list_butik_sto_cntr))
  butik_lab_test <- list(sma = names(match_list_butik_sma_test),
                         stora = names(match_list_butik_sto_test))
  butik_lab_both <- list(sma = c(names(match_list_butik_sma_cntr),
                                 names(match_list_butik_sma_test)),
                         stora = c(names(match_list_butik_sto_cntr),
                                   names(match_list_butik_sto_test)))

  butik_val_cntr <- list(sma = unlist(match_list_butik_sma_cntr),
                         stora = unlist(match_list_butik_sto_cntr))
  butik_val_test <- list(sma = unlist(match_list_butik_sma_test),
                         stora = unlist(match_list_butik_sto_test))
  butik_val_both <- list(sma = c(unlist(match_list_butik_sma_cntr),
                                 unlist(match_list_butik_sma_test)),
                         stora = c(unlist(match_list_butik_sto_cntr),
                                   unlist(match_list_butik_sto_test)))

  # list_butik <- names(match_list_butik)
  list_ref_unit  <- c(Zoner = "zoner",
                      Kampanj = "kampanj",
                      Skylttyp = "skylttyp")
  get_zeta_display_color_settings()
  }
  )
