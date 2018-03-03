context("integrateIt")
test_that("squares add correctly", {
  expect_that(addSquares(2, 3),
              equals(new("Squares", square = (13), x = 2, y = 3)))
  expect_that(addSquares(2, 2),
              equals(new("Squares", square = (8), x = 2, y = 2)))
  expect_error(addSquares("p"))
  expect_is(addSquares(1, 2), "Squares")
})
