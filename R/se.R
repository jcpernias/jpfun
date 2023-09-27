##' Extract Standard Errors
##'
##' `se()` returns the standard errors of coefficients from objects
##' returning by modeling functions.
##'
##' @title se
##' @param x an object from which standard errors are extracted
##' @return a vector
##' @examples
##' x <- lm(mpg ~ disp, data = mtcars)
##' se(x)
##'
##' @export
se <- function(x) {
    return(sqrt(diag(stats::vcov(x))))
}
