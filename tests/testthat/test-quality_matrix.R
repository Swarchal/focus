context("quality matrix")

data <- image_data
out <- quality_matrix(data)

test_that("quality matrix returns errors when expected",{
    expect_error(quality_matrix("string"))
    expect_error(quality_matrix(10))
    expect_error(quality_matrix(c(1,2,3,4)))
    expect_error(quality_matrix(iris))
})

test_that("quality matrix returns expected values",{
    expect_true(is.matrix(out))
    expect_equal(length(get_QC_cols(data)), ncol(out))
})

