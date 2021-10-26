#' @title Calculate weighted mean
#'
#' @description TODO: add this later
#' @param x one parameter.
#' @param w another parameter.
#' @return weighted mean values.
#' @export
wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
