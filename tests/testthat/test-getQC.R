context("get_QC_cols")

data <- image_data

test_that("returns error when expected",{
	  expect_error(get_QC_cols("string"))
	  expect_error(get_QC_cols(c("string", "Image_string")))
})

test_that("returns expeced values",{
	  out <- get_QC_cols(data)
	  expect_true(length(out) == 80)
	  expect_true(is.character(out))
})
