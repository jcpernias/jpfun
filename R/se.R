##' Extract Standard Errors
##'
##' `se()` returns the standard errors of parameter estimates from
##' objects returned by `lm` and other  modeling functions.
##'
##' The parameter `vcov` is a string that controls how the covariance
##' matrix is computed. If `vcov` is "iid" (the default), the
##' covariance matrix is computed using [`vcov`][stats::vcov]:
##' `vcov(x)`. Heteroskedasticity robust covariance matrices are
##' computed with `vcov = "HC"`.  Other variants valid under
##' heteroskedasticity are "HC0", "HC1", "HC2" and "HC3" (which is
##' equivalent to "HC"). Setting `vcov` equal to "NW" or "HAC"
##' computes the Newey and West covariance matrix estimator that is
##' valid under autocorrelation and heteroskedasticity.
##'
##' The `x` object should support the [`coef`][stats::coef] and
##' [`vcov`][stats::vcov] methods.
##'
##' @title se
##' @param x an object from which standard errors are extracted
##' @param vcov an object indicating how to get the covariance matrix.
##' @return a vector
##' @seealso [sandwich::vcovHC], [sandwich::NeweyWest].
##' @examples
##' x <- lm(mpg ~ disp, data = mtcars)
##' se(x)
##'
##' se(x, vcov = "HC")
##' @export
se <- function(x, vcov = "iid") {
    V <- get_vcov(x, type = vcov)
    return(sqrt(diag(V)))
}
