test_that("lm standard errors", {
    x <- lm(mpg ~ disp, data = mtcars)
    Vbeta <- vcov(x)
    expect_equal(sqrt(diag(Vbeta)), se(x))
})
