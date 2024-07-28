#' Formats a number as string with a % suffix
#'
#' @param x A integer or double.
#' @param format A formatc function passthrough
#' @param digits A formatc function passthrough.  enables rounding
#' @param ... Standard Ellipse
#' @return The number as a character folled by a percent sign
#' @export
#'

percent <- function(x, digits = 2, format = "f", ...) {
  ifelse(is.na(x),
         NA,
         paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  )
}
