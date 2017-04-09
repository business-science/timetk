# Utility functions for working with forecast package -----

#' Print the ARIMA model parameters
#'
#' Refer to forecast:::arima.string.
#' [`forecast` arima.R](https://github.com/robjhyndman/forecast/blob/master/R/arima.R)
#'
#' @param object An object of class Arima
#' @param padding Add padding to the name returned
arima_string <- function(object, padding = FALSE) {

    order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
    result <- paste("ARIMA(", order[1], ",", order[2], ",", order[3],
                    ")", sep = "")
    if (order[7] > 1 & sum(order[4:6]) > 0)
        result <- paste(result, "(", order[4], ",", order[5],
                        ",", order[6], ")[", order[7], "]", sep = "")
    if (!is.null(object$xreg)) {
        if (NCOL(object$xreg) == 1 & is.element("drift", names(object$coef)))
            result <- paste(result, "with drift        ")
        else result <- paste("Regression with", result, "errors")
    }
    else {
        if (is.element("constant", names(object$coef)) | is.element("intercept",
                                                                    names(object$coef)))
            result <- paste(result, "with non-zero mean")
        else if (order[2] == 0 & order[5] == 0)
            result <- paste(result, "with zero mean    ")
        else result <- paste(result, "                  ")
    }
    if (!padding)
        result <- gsub("[ ]*$", "", result)
    return(result)

}

#' Print the BATS model parameters
#'
#' Refer to forecast:::makeText.
#' [`forecast` bats.R](https://github.com/robjhyndman/forecast/blob/master/R/bats.R)
#'
#' @param object An object of class bats
bats_string <- function(object) {

    name <- "BATS("
    if (!is.null(object$lambda)) {
        name <- paste(name, round(object$lambda, digits = 3),
                      sep = "")
    }
    else {
        name <- paste(name, "1", sep = "")
    }
    name <- paste(name, ", {", sep = "")
    if (!is.null(object$ar.coefficients)) {
        name <- paste(name, length(object$ar.coefficients), sep = "")
    }
    else {
        name <- paste(name, "0", sep = "")
    }
    name <- paste(name, ",", sep = "")
    if (!is.null(object$ma.coefficients)) {
        name <- paste(name, length(object$ma.coefficients), sep = "")
    }
    else {
        name <- paste(name, "0", sep = "")
    }
    name <- paste(name, "}, ", sep = "")
    if (!is.null(object$damping.parameter)) {
        name <- paste(name, round(object$damping.parameter, digits = 3),
                      sep = "")
    }
    else {
        name <- paste(name, "-", sep = "")
    }
    name <- paste(name, ", ", sep = "")
    if (!is.null(object$seasonal.periods)) {
        name <- paste(name, "{", sep = "")
        for (i in object$seasonal.periods) {
            name <- paste(name, i, sep = "")
            if (i != object$seasonal.periods[length(object$seasonal.periods)]) {
                name <- paste(name, ",", sep = "")
            }
            else {
                name <- paste(name, "})", sep = "")
            }
        }
    }
    else {
        name <- paste(name, "-)", sep = "")
    }
    return(name)

}

#' Print the TBATS model parameters
#'
#' Refer to forecast:::makeTextTBATS.
#' [`forecast` bats.R](https://github.com/robjhyndman/forecast/blob/master/R/bats.R)
#'
#' @param object An object of class bats or tbats
tbats_string <- function(object) {

    name <- "TBATS("
    if (!is.null(object$lambda)) {
        name <- paste(name, round(object$lambda, digits = 3),
                      sep = "")
    }
    else {
        name <- paste(name, "1", sep = "")
    }
    name <- paste(name, ", {", sep = "")
    if (!is.null(object$ar.coefficients)) {
        name <- paste(name, length(object$ar.coefficients), sep = "")
    }
    else {
        name <- paste(name, "0", sep = "")
    }
    name <- paste(name, ",", sep = "")
    if (!is.null(object$ma.coefficients)) {
        name <- paste(name, length(object$ma.coefficients), sep = "")
    }
    else {
        name <- paste(name, "0", sep = "")
    }
    name <- paste(name, "}, ", sep = "")
    if (!is.null(object$damping.parameter)) {
        name <- paste(name, round(object$damping.parameter, digits = 3),
                      ",", sep = "")
    }
    else {
        name <- paste(name, "-,", sep = "")
    }
    if (!is.null(object$seasonal.periods)) {
        name <- paste(name, " {", sep = "")
        M <- length(object$seasonal.periods)
        for (i in 1:M) {
            name <- paste(name, "<", object$seasonal.periods[i],
                          ",", object$k.vector[i], ">", sep = "")
            if (i < M) {
                name <- paste(name, ", ", sep = "")
            }
            else {
                name <- paste(name, "})", sep = "")
            }
        }
    }
    else {
        name <- paste(name, "{-})", sep = "")
    }
    return(name)

}
