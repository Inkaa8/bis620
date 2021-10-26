test_that("The wt_mean function works",{
  expect_equal(wt_mean(2, 3), 2)
})

test_that("The wt_mean function works",{
  expect_equal(wt_mean(1:2, 2:3), 1.6)
})
