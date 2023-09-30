## The `get_vcov` function provides support to functions of this
## package that allow the specification of a covariance matrix
## estimator.
##
## The returned covariance matrix include rows and columns of NA's for
## aliased coefficients.
get_vcov <- function(x, type) {
    if (!is.character(type) && length(type) != 1) {
        msg <- "vcov type must be a character vector of length 1"
        stop(msg, call. = FALSE)
    }
    if (type == "iid") {
        Vbeta <- stats::vcov(x, complete = TRUE)
    } else if (type %in% c("HC", "HC0", "HC1", "HC2", "HC3")) {
        if (type == "HC") {
            type <- "HC3"
        }
        Vbeta <- sandwich::vcovHC(x, type)
    } else if (type %in% c("NW", "HAC")){
        Vbeta <- sandwich::NeweyWest(x, prewhite = FALSE)
    } else {
        msg <- paste0("unknown covariance matrix estimator type '", type, "'")
        stop(msg, call. = FALSE)
    }

    ## Handle aliased coefficients
    aliased <- is.na(stats::coef(x))
    if (any(aliased) && length(aliased) != nrow(Vbeta)) {
        Vbeta <- stats::.vcov.aliased(aliased, Vbeta, complete = TRUE)
    }

    ## Return covariance matrix
    Vbeta
}
