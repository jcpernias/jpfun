## The `get_vcov` function provides support to functions of this
## package that allow the specification of a covariance matrix
## estimator.
##
## The returned covariance matrix include rows and columns of NA's for
## aliased coefficients.
get_vcov <- function(x) {
    Vbeta <- vcov(x)

    ## Handle aliased coefficients
    aliased <- is.na(stats::coef(x))
    Vbeta <- stats::.vcov.aliased(aliased, Vbeta, complete = TRUE)

    ## Return covariance matrix
    Vbeta
}
