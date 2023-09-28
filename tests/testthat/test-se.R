test_that("OLS standard errors", {
    x <- lm(mpg ~ disp, data = mtcars)
    V <- vcov(x)
    expect_equal(se(x), sqrt(diag(V)))
})

test_that("aliased OLS standard errors", {
    x <- lm(mpg ~ disp + I(disp + 1), data = mtcars)
    V <- vcov(x, complete = TRUE)
    expect_equal(se(x), sqrt(diag(V)))
})
