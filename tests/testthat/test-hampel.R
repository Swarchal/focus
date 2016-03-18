context("Hampel outlier test")

set.seed(123321)

x <- rnorm(100)
x <- c(x, 100, 200, 300, 400, 500, NA)
x <- sample(x)

test_that("hampel handles missing values as expected",{
    expect_error(hampel(c(1, 2, 3, 4, "string")))
    expect_equal(hampel(c(1,2,3,4,NA), na.rm = FALSE), NA)

    facs <- factor(c(1,2,3,4,5,6))
    expect_error(hampel(facs))
})

test_that("hampel returns expected values",{
    out <- hampel(x, sigma = 1e10)
    expect_equal(sum(out), 0)

    expect_equal(sum(hampel(x, sigma = 6)), 5)
})
