context("range")

test_that("continuous ranges expand as expected", {
  r <- continuous_range()

  r$train(1)
  expect_equal(r$range, c(1, 1))

  r$train(10)
  expect_equal(r$range, c(1, 10))
})

test_that("discrete ranges expand as expected", {
  r <- discrete_range()

  r$train("a")
  expect_equal(r$range, "a")

  r$train("b")
  expect_equal(r$range, c("a", "b"))

  r$train(letters)
  expect_equal(r$range, letters)
})

test_that("immutable ranges cannot be trained or reset", {
  r <- immutable_range(c(0, 0))
  expect_error(r$train(), "Cannot train")
  expect_error(r$reset(), "Cannot reset")
  expect_identical(r$range, c(0, 0))
})
