test_that("OLS standard errors", {
    x <- lm(mpg ~ disp, data = mtcars)
    V <- stats::vcov(x)
    expect_equal(se(x), sqrt(diag(V)))
})

test_that("aliased OLS standard errors", {
    x <- lm(mpg ~ disp + I(disp + 1), data = mtcars)
    V <- stats::vcov(x, complete = TRUE)
    expect_equal(se(x), sqrt(diag(V)))
})

test_that("HC0 standard errors", {
    x <- lm(mpg ~ disp, data = mtcars)
    V <- sandwich::vcovHC(x, type = "HC0")
    expect_equal(se(x, vcov = "HC0"), sqrt(diag(V)))
})

test_that("aliased HC0 standard errors", {
    x <- lm(mpg ~ disp + I(disp + 1), data = mtcars)
    V <- stats::.vcov.aliased(is.na(coef(x)),
                              sandwich::vcovHC(x, type = "HC0"))
    expect_equal(se(x, vcov = "HC0"), sqrt(diag(V)))
})

test_that("HC standard errors", {
    x <- lm(mpg ~ disp, data = mtcars)
    V <- sandwich::vcovHC(x, type = "HC3")
    expect_equal(se(x, vcov = "HC"), sqrt(diag(V)))
})
