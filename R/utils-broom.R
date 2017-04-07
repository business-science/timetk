# Utility functions for sw_tidy, sw_glance, sw_augment

#' Print the ARIMA model parameters (refer to forecast:::arima.string)
#' https://github.com/robjhyndman/forecast/blob/master/R/arima.R
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


