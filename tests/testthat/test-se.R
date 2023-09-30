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

test_that("incorrect type of vcov parameter", {
    x <- lm(mpg ~ disp, data = mtcars)
    expect_error(se(x, vcov = NULL))
    expect_error(se(x, vcov = NA))
    expect_error(se(x, vcov = c("iid", "HC")))
    expect_error(se(x, vcov = sandwich::vcovHC))
})

test_that("HAC standard errors", {
    x <- lm(DriversKilled ~ law, data = Seatbelts)
    V <- sandwich::NeweyWest(x, prewhite = FALSE)
    expect_equal(se(x, vcov = "NW"), sqrt(diag(V)))
    expect_equal(se(x, vcov = "HAC"), sqrt(diag(V)))
})
