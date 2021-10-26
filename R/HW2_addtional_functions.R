#' @title Prints the number of missing values in a data frame.
#'
#' @description TODO: add this later
#' @param x an AE data set for a single patient with AESEV.
#' @return The worst ae for the patient.
#' @export
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")

  invisible(df)
}
