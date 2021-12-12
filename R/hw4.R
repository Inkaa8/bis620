#' @title Set new class
#' @description This function will set new S4 class bis620_numpy_matrix.
#' @export
setClass(Class = "bis620_numpy_matrix",
         slots = list(pymatrix = "numpy.matrix")
)



#' @title Add
#' @description This function tells the sum of two bis620_numpy_matrices.
#' @param e1  bis620_numpy_matrix1
#' @param e2  bis620_numpy_matrix2
#' @return The sum of two bis620_numpy_matrices.
#' @export
setMethod(
  f = "+",
  signature = c(e1 = "bis620_numpy_matrix", e2 = "bis620_numpy_matrix"),
  definition = function(e1, e2){

    a <- py_to_r(e1@pymatrix)
    b <- py_to_r(e2@pymatrix)

    if (ncol(a) != ncol(b) || nrow(a) != nrow(b)){
      stop("Two matrices have different dimentions.")
    }

    return(a + b)

  }
)


#' @title Add
#' @description This function tells the sum of bis620_numpy_matrix and R base matrix.
#' @param e1  bis620_numpy_matrix
#' @param e2  R base matrix
#' @return The sum of bis620_numpy_matrix and R base matrix.
#' @export
setMethod(
  f = "+",
  signature = c(e1 = "bis620_numpy_matrix", e2 = "matrix"),
  definition = function(e1, e2){
    a <- py_to_r(e1@pymatrix)

    if (ncol(a) != ncol(e2) || nrow(a) != nrow(e2)){
      stop("Two matrices have different dimentions.")
    }

    return(a + e2)

  }
)


#' @title Add
#' @description This function tells the sum of bis620_numpy_matrix and R base Matrix.
#' @param e1  R base Matrix
#' @param e2  bis620_numpy_matrix
#' @return The sum of bis620_numpy_matrix and R base Matrix.
#' @export
setMethod(
  f = "+",
  signature = c(e1 = "Matrix", e2 = "bis620_numpy_matrix"),
  definition = function(e1, e2){
    b <- py_to_r(e2@pymatrix)

    if (ncol(e1) != ncol(b) || nrow(e1) != nrow(b)){
      stop("Two matrices have different dimentions.")
    }

    return(e1 + b)

  }
)



#' @title Add
#' @description This function tells the sum of bis620_numpy_matrix and R basic Matrix.
#' @param e1  bis620_numpy_matrix
#' @param e2  R base Matrix
#' @return The sum of bis620_numpy_matrix and R basic Matrix.
#' @export
setMethod(
  f = "+",
  signature = c(e1 = "bis620_numpy_matrix", e2 = "Matrix"),
  definition = function(e1, e2){
    a <- py_to_r(e1@pymatrix)

    if (ncol(a) != ncol(e2) || nrow(a) != nrow(e2)){
      stop("Two matrices have different dimentions.")
    }

    return(a + e2)

  }
)



#' @title Transpose
#' @description This function will give us the transpose of a bis620_numpy_matrix.
#' @param x  bis620_numpy_matrix
#' @return The transpose of bis620_numpy_matrix.
#' @export
setMethod(
  f = "t",
  signature = c(x = "bis620_numpy_matrix"),
  definition = function(x){
    return(t(py_to_r(x@pymatrix)))
  }
)



#' @title Matrix multiply
#' @description This function tells the multiply of two bis620_numpy_matrices.
#' @param x  bis620_numpy_matrix
#' @param y  bis620_numpy_matrix
#' @return The multiply of two bis620_numpy_matrices.
#' @export
setMethod(
  f = "%*%",
  signature = c(x = "bis620_numpy_matrix", y = "bis620_numpy_matrix"),
  definition = function(x, y){

    a <- py_to_r(x@pymatrix)
    b <- py_to_r(y@pymatrix)

    if (ncol(a) != nrow(b) || nrow(a) != ncol(b)){
      stop("The dimensions of two matrices don't match.")
    }

    return(a %*% b)

  }
)



#' @title Matrix multiply
#' @description This function tells the multiply of R basic matrix and bis620_numpy_matrix.
#' @param x  R base matrix
#' @param y  bis620_numpy_matrix
#' @return The multiply of two bis620_numpy_matrices.
#' @export
setMethod(
  f = "%*%",
  signature = c(x = "matrix", y = "bis620_numpy_matrix"),
  definition = function(x, y){
    b <- py_to_r(y@pymatrix)

    if (ncol(x) != nrow(b) || nrow(x) != ncol(b)){
      stop("The dimensions of two matrices don't match.")
    }

    return(x %*% b)

  }
)



#' @title Matrix multiply
#' @description This function tells the multiply of R basic matrix and bis620_numpy_matrix.
#' @param x  bis620_numpy_matrix
#' @param y  R base matrix
#' @return The multiply of R base matrix and bis620_numpy_matrix.
#' @export
setMethod(
  f = "%*%",
  signature = c(x = "bis620_numpy_matrix", y = "matrix"),
  definition = function(x, y){
    a <- py_to_r(x@pymatrix)

    if (ncol(a) != nrow(y) || nrow(a) != ncol(y)){
      stop("The dimensions of two matrices don't match.")
    }

    return(a %*% y)

  }
)



#' @title Matrix multiply
#' @description This function tells the multiply of R basic matrix and bis620_numpy_matrix.
#' @param x  bis620_numpy_matrix
#' @param y  R base Matrix
#' @return The multiply of R base Matrix and bis620_numpy_matrix.
#' @export
setMethod(
  f = "%*%",
  signature = c(x = "bis620_numpy_matrix", y = "Matrix"),
  definition = function(x, y){
    a <- py_to_r(x@pymatrix)

    if (ncol(a) != nrow(y) || nrow(a) != ncol(y)){
      stop("The dimensions of two matrices don't match.")
    }

    return(a %*% y)

  }
)

