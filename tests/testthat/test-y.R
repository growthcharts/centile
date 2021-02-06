z <- c(-2, -2, 2, 2, -1, 1, -1, 1)
x <- c(rep(0.1, 4), rep(0.1, 4))
refcode <- c(rep(c("who_2011_hgt_male_", "who_2011_hgt_female_"), 2),
             rep(c("who_2011_wgt_male_", "who_2011_wgt_female_"), 2))
result <- c(51.620, 50.482, 59.453, 58.371, 4.13, 5.03, 4.13, 5.03)

test_that("y() returns numeric values", {
  expect_equal(y(z, x, refcode), result)
})

test_that("z(y(z)) returns z", {
  expect_equal(z(y(z, x, refcode), x, refcode), z, tolerance = 0.005)
})

