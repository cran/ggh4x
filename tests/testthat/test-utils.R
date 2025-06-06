test_that("center_limits centers limits", {
  f <- center_limits()
  expect_equal(f(c(-1, 3)), c(-3, 3))
  f <- center_limits(1)
  expect_equal(f(c(-1, 3)), c(-1, 3))
})

test_that("weave_factors combines factors", {
  f1 <- c("banana", "apple", "apple", "kiwi", NA)
  f2 <- factor(c(1, NA, 1:3), labels = c("house", "cat", "dog"))

  a <- levels(weave_factors(f1, f2))
  expect_identical(a, c("banana.house", "apple.house", "apple.", "kiwi.cat", ".dog"))

  a <- levels(weave_factors(as.factor(f1), f2))
  expect_identical(a, c("apple.house", "apple.", "banana.house", "kiwi.cat", ".dog"))

  a <- weave_factors(f1, f2, dopr = TRUE)
  b <- weave_factors(f1, f2, drop = FALSE)

  expect_length(levels(a), 5)
  expect_length(levels(b), 4*4) # f2 NA becomes empty string level

  a <- levels(weave_factors(f1, f2, replaceNA = FALSE))
  expect_identical(a, c("banana.house", "apple.house", "kiwi.cat", "NA.dog"))

  a <- substitute(weave_factors(f1, f2[1:3]))
  expect_error(eval(a), "same length")

})
