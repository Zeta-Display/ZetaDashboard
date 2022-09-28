#' Returns the BK colour setting
#'
#' @return invisble return; function is called for side effects only
#' @export
get_zeta_display_color_settings <- function() {
  zeta_color_palette_01 <- c(rgb(red = 64, green = 64,  blue = 64,
                               maxColorValue = 255),
                           rgb(red = 0,   green = 64,  blue = 112,
                               maxColorValue = 255),
                           rgb(red = 105, green = 153, blue = 209,
                               maxColorValue = 255),
                           rgb(red = 180, green = 199, blue = 231,
                               maxColorValue = 255),
                           rgb(red = 116, green = 116, blue = 116,
                               maxColorValue = 255),
                           rgb(red = 191, green = 191, blue = 191,
                               maxColorValue = 255),
                           rgb(red = 82,  green = 135, blue = 136,
                               maxColorValue = 255))
  zeta_color_palette_02 <- c("#f89404",
                             "#f5426c",
                             "#000000")

  f_col  <- ("#404040")
  f_size <- 12

  t1 <- ggplot2::theme(plot.title = ggplot2::element_text(colour = f_col,
                                                          size = f_size + 2,
                                                          hjust = 0.5),
                       plot.subtitle = ggplot2::element_text(colour = f_col,
                                                             vjust = 1,
                                                             size = f_size),
                       plot.caption = ggplot2::element_text(colour = f_col,
                                                            vjust = 1,
                                                            size = f_size - 6),
                       axis.ticks = ggplot2::element_line(colour = NA),
                       axis.text = ggplot2::element_text(colour = f_col,
                                                         size = f_size),
                       axis.text.x = ggplot2::element_text(colour = f_col,
                                                           size = f_size,
                                                           angle = 90,
                                                           vjust = 0.1,
                                                           hjust = 1),
                       axis.title = ggplot2::element_text(colour = f_col,
                                                          size = f_size),
                       panel.background =
                         ggplot2::element_rect(fill = NA))
  t2 <- ggplot2::theme(plot.title = ggplot2::element_text(colour = f_col,
                                                          size = f_size + 2,
                                                          hjust = 0.5),
                       plot.subtitle = ggplot2::element_text(colour = f_col,
                                                             vjust = 1,
                                                             size = f_size),
                       plot.caption = ggplot2::element_text(colour = f_col,
                                                            vjust = 1,
                                                            size = f_size - 6),
                       # axis.ticks.x = ggplot2::element_blank(),
                       # axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(colour = f_col,
                                                           size = f_size),
                       axis.title = ggplot2::element_text(colour = f_col,
                                                          size = f_size),
                       panel.background = ggplot2::element_rect(fill = NA))

  # assign("zeta_color_palette_01", zeta_color_palette_01, envir = .GlobalEnv)
  assign("zeta_color_palette_02", zeta_color_palette_02, envir = .GlobalEnv)
  # assign("zeta_theme_01", t1, envir = .GlobalEnv)
  assign("zeta_theme_02", t2, envir = .GlobalEnv)

  invisible(1)
}
