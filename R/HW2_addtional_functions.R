#' @title Calculate weighted mean
#'
#' @description This function tells weighted mean values.
#' @param x one parameter.
#' @param w Weighted parameter.
#' @return weighted mean values.
#' @export
wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}


#' @title Calculate missing values
#'
#' @description This function tells the number of missing values.
#' @param x Data to calculate.
#' @return The number of missing values.
#' @export
num_na <- function(x) {
  if(sum(is.na(x)) == 0){
    return("There is no missing value.")
  } else{
    paste("The number of missing values is", sum(is.na(x)), ".", sep = " ")
  }
}
