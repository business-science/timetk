# UTILILITY FUNCTIONS ----------------------------------------------------------

get_date_variables <- function(df){
    if (!is.data.frame(df)) {
        stop('df should be a data.frame', call. = FALSE)
    }
    classes <- lapply(df, class)
    date_classes <- (sapply(classes, function(x) 'POSIXt' %in% x) |
                         sapply(classes, function(x) 'Date' %in% x))
    return(names(which(date_classes)))
}
