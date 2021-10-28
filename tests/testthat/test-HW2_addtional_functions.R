test_that("The wt_mean function works",{
  expect_equal(wt_mean(2, 3), 2)
})

test_that("The wt_mean function works",{
  expect_equal(wt_mean(1:2, 2:3), 1.6)
})

test_that("The num_na function tells no missing value",{
  expect_equal(num_na(c(1, 2, 3)), "There is no missing value.")
})

test_that("The num_na function tells missing value number",{
  expect_equal(num_na(c(1, 2, NA, NA)), "The number of missing values is 2 .")
})

