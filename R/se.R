##' Extract Standard Errors
##'
##' `se()` returns the standard errors of parameter estimates from
##' objects returned by modeling functions.
##'
##' @title se
##' @param x an object from which standard errors are extracted
##' @return a vector
##' @examples
##' x <- lm(mpg ~ disp, data = mtcars)
##' se(x)
##'
##' @export
se <- function(x, vcov = NULL) {
    V <- get_vcov(x, how = vcov)
    return(sqrt(diag(V)))
}
