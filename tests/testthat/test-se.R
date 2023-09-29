library(sandwich)

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

test_that("HC0 standard errors", {
    x <- lm(mpg ~ disp, data = mtcars)
    V <- vcovHC(x, type = "HC0")
    expect_equal(se(x, vcov = "HC0"), sqrt(diag(V)))
})

test_that("aliased HC0 standard errors", {
    x <- lm(mpg ~ disp + I(disp + 1), data = mtcars)
    V <- .vcov.aliased(is.na(coef(x)), vcovHC(x, type = "HC0"))
    expect_equal(se(x, vcov = "HC0"), sqrt(diag(V)))
})

test_that("HC standard errors", {
    x <- lm(mpg ~ disp, data = mtcars)
    V <- vcovHC(x, type = "HC3")
    expect_equal(se(x, vcov = "HC"), sqrt(diag(V)))
})
