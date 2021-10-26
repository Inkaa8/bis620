#' @title Calculate weighted mean
#'
#' @description This function
#' @param x one parameter.
#' @param w Weighted parameter.
#' @return weighted mean values.
#' @export
wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}

