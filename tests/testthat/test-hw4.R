library(reticulate)
use_condaenv("bis620", required = TRUE)
np <- import("numpy", convert = FALSE)

x1 <- new("bis620_numpy_matrix",
          pymatrix = np$matrix(list(c(0, 4.3, 0, 0, 0, 0),
                                    c(0, 5.6, 0, 0, 0, 0),
                                    c(0, 0, 0, 0, 0, 0),
                                    c(0, 0, 0, 0, 0, 0),
                                    c(0, 0, 0, 0, 0, 7),
                                    c(10, 0, 0, 0, 0, 0))
          ))

set.seed(1)
x2 <- matrix(rnorm(36), ncol = 6)
library(Matrix)
set.seed(1)
x3 <- Matrix(rnorm(36), ncol = 6)
x4 <- sparseMatrix(
  i = c(1, 1, 3, 6),
  j = c(2, 3, 5, 1),
  x = c(4.3, 5.6, 7, 10),
  dims = c(6, 6)
)

test_that("bis620_numpy_matrix is compatiable with itself", {
  expect_true(inherits(x1 + x1, "matrix"))
})

test_that("bis620_numpy_matrix is compatiable with itself", {
  expect_true(inherits(t(x1) %*% x1, "matrix"))
})

test_that("bis620_numpy_matrix is compatiable with itself", {
  expect_true(inherits(x1 %*% x1, "matrix"))
})

test_that("bis620_numpy_matrix is compatiable with base R matrix", {
  expect_true(inherits(x1 + t(x2), "matrix"))
})

test_that("bis620_numpy_matrix is compatiable with base R matrix", {
  expect_true(inherits(x1 %*% x2, "matrix"))
})

test_that("bis620_numpy_matrix is compatiable with base R matrix", {
  expect_true(inherits(x3 + x1, "Matrix"))
})

test_that("base R matrix is compatiable with base R matrix", {
  expect_true(inherits(x3 + x3, "Matrix"))
})

test_that("bis620_numpy_matrix is compatiable with base R matrix", {
  expect_true(inherits(x1 + x4, "Matrix"))
})

test_that("bis620_numpy_matrix is compatiable with base R matrix", {
  expect_true(inherits(x1 %*% x4, "Matrix"))
})

