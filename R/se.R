##' Extract Standard Errors
##'
##' `se()` returns the standard errors of parameter estimates from
##' objects returned by modeling functions.
##'
##'  The parameter `vcov` controls how the covariance matrix is computed:
##' - If `vcov` is `NULL` (the default), the covariance matrix is computed using
##'   [`vcov`][stats::vcov]: `vcov(x)`.
##' - `vcov` can be a string that indicates which type of covariance matrix is used.
##'   Covariance matrices robust to heteroskedasticity are computed with `vcov = "HC"`.
##'   Other variants valid under heteroskedasticity are "HC0", "HC1", "HC2" and "HC3"
##'   (which is equivalent to "HC").
##'
##' The `x` object should support the [`coef`][stats::coef] and
##' [`vcov`][stats::vcov] methods.
##'
##' @title se
##' @param x an object from which standard errors are extracted
##' @param vcov an object indicating how to get the covariance matrix.
##' @return a vector
##' @seealso [sandwich::vcovHC]
##' @examples
##' x <- lm(mpg ~ disp, data = mtcars)
##' se(x)
##'
##' @export
se <- function(x, vcov = NULL) {
    V <- get_vcov(x, how = vcov)
    return(sqrt(diag(V)))
}
