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

  match_list <- jsonlite::fromJSON("inst/meta/match_list.json")
  # list_butik <- unlist(match_list$butik)
  match_list_butik <- match_list$butik
  list_butik <- names(match_list_butik)
  list_ref_unit  <- c(Zoner = "zoner",
                      Kampanj = "kampanj",
                      Skylttyp = "skylttyp")
  get_zeta_display_color_settings()
  }
  )
