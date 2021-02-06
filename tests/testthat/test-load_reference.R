
test_that("finds internal reference", {
  expect_s3_class(load_reference("who_2011_hgt_male_"), "tbl")
})

test_that("finds internal reference: table", {
  expect_s3_class(load_reference("who_2011_hgt_male_", "table"), "tbl")
})

test_that("finds internal reference: study", {
  expect_type(load_reference("who_2011_hgt_male_", "study"), "character")
})

test_that("finds internal reference: index", {
  expect_type(load_reference("who_2011_hgt_male_", "index"), "double")
})

test_that("cannot find invalid refcode", {
  expect_warning(load_reference("junk", verbose = TRUE))
})

test_that("cannot find invalid pkg", {
  expect_warning(load_reference("junk", pkg = "notloaded", verbose = TRUE))
})
