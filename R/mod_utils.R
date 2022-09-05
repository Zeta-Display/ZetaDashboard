mod_break <- function(size) {
  if (size == "small") {
    return(tagList(tags$br()))
  } else if (size == "medium") {
    return(tagList(tags$br(), tags$br()))
  } else if (size == "large") {
    return(tagList(tags$br(), tags$br(), tags$br(),
                   tags$br(), tags$br(), tags$br()))
  } else {
    stop("Unknown size argument or typo!")
  }
}
