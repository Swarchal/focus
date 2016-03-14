context("total outliers")

data <- image_data
qm_out <- quality_matrix(data)

to_out <- total_outliers(qm_out)

test_that("total outliers produces vector",{
    expect_true(is.vector(to_out))
})

test_that("total outliers produces numeric vector",{
    expect_true(is.numeric(to_out))
})

test_that("total outliers is the expected length",{
    expect_equal(length(to_out), nrow(data))
})
