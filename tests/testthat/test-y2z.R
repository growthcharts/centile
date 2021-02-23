y <- c(50, 50, 60, 60, 4, 4, 4, 4)
x <- c(rep(0.1, 4), rep(0.1, 4))
refcode <- c(rep(c("who_2011_hgt_male_", "who_2011_hgt_female_"), 2),
             rep(c("who_2011_wgt_male_", "who_2011_wgt_female_"), 2))
result <- c(-2.827, -2.245,  2.279,  2.826, -1.234, -0.698, -1.234, -0.698)

test_that("z() returns numeric values", {
  expect_equal(y2z(y, x, refcode), result)
})

test_that("z2y(y2z(y)) returns y", {
  expect_equal(z2y(y2z(y, x, refcode), x, refcode), y, tolerance = 0.001)
})

