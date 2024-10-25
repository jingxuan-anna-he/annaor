test_that("OR_95CI returns correct output format", {
  coef <- c(0.5, -0.3)
  se <- c(0.1, 0.15)
  result <- OR_95CI(coef, se, siglevel = 0.05, roundto = 2)
  expect_true(is.character(result))
  expect_length(result, length(coef))
})
