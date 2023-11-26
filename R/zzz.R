#' @import dplyr
#' @import logger

.onLoad <- function(libname, pkgname) {
  layout <- logger::layout_glue_generator(format = '{time} {level} [{namespace}::{fn}] {msg}')
  logger::log_layout(layout)
}
