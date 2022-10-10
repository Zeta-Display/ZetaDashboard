mod_break_vspace <- function(size) {
  if (size == "small") {
    return(tagList(tags$br()))
  } else if (size == "medium") {
    return(tagList(tags$br(), tags$br(), tags$br()))
  } else if (size == "large") {
    return(tagList(tags$br(), tags$br(), tags$br(),
                   tags$br(), tags$br(), tags$br()))
  } else {
    stop("Unknown size argument or typo!")
  }
}

replace_butik_lab_with_var <- function(butik_val) {
  unlist(unname(butik_val_all[butik_val]))
}

rename_data_set_with_var <- function(data_out,
                                     skip,
                                     ref_unit) {
  data_tmp <- data_out
  names(data_tmp)[-c(1:skip)] <- substring(names(data_tmp)[-c(1:skip)],
                                           first = nchar(ref_unit) + 2)
  data_tmp
}
