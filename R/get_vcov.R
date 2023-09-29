## The `get_vcov` function provides support to functions of this
## package that allow the specification of a covariance matrix
## estimator.
##
## The returned covariance matrix include rows and columns of NA's for
## aliased coefficients.
get_vcov <- function(x, how = NULL) {
    Vbeta <- NULL

    if (is.null(how)) {
        Vbeta <- stats::vcov(x, complete = TRUE)
    } else if (is.character(how)) {
        if (length(how) != 1) {
            msg <- "covariance matrix estimator type must be a character vector of length 1"
            stop(msg, call. = FALSE)
        } else {
            if (how %in% c("HC", "HC0", "HC1", "HC2", "HC3")) {
                Vbeta <-
                    sandwich::vcovHC(x, type = if (how == "HC") "HC3" else how)
            } else {
                msg <- paste0("unknown covariance matrix estimator type '",
                              how, "'")
                stop(msg, call. = FALSE)
            }
        }
    }

    ## Handle aliased coefficients
    aliased <- is.na(stats::coef(x))
    if (any(aliased) && length(aliased) != nrow(Vbeta)) {
        Vbeta <- stats::.vcov.aliased(aliased, Vbeta, complete = TRUE)
    }

    ## Return covariance matrix
    Vbeta
}
