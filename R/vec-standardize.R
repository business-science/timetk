#' Standardize to Mean 0, Standard Deviation 1 (Center & Scale)
#'
#' Standardization is commonly used to center and scale numeric features to prevent one from
#' dominating in algorithms that require data to be on the same scale.
#'
#' @param x A numeric vector.
#' @param mean The mean used to invert the standardization
#' @param sd The standard deviation used to invert the standardization process.
#' @param silent Whether or not to report the automated `mean` and `sd` parameters as a message.
#'
#' @return
#' Returns a `numeric` vector with the standardization transformation applied.
#'
#' @details
#' __Standardization vs Normalization__
#'
#' - __Standardization__ refers to a transformation that reduces the range to
#' mean 0, standard deviation 1
#'
#' - __Normalization__ refers to a transformation that reduces the min-max range: (0, 1)
#'
#' @seealso
#'   - Normalization/Standardization: [standardize_vec()], [normalize_vec()]
#'   - Box Cox Transformation: [box_cox_vec()]
#'   - Lag Transformation: [lag_vec()]
#'   - Differencing Transformation: [diff_vec()]
#'   - Rolling Window Transformation: [slidify_vec()]
#'   - Loess Smoothing Transformation: [smooth_vec()]
#'   - Fourier Series: [fourier_vec()]
#'   - Missing Value Imputation for Time Series: [ts_impute_vec()], [ts_clean_vec()]
#'
#'
#'
#' @examples
#' library(dplyr)
#'
#' d10_daily <- m4_daily %>% dplyr::filter(id == "D10")
#'
#' # --- VECTOR ----
#'
#' value_std <- standardize_vec(d10_daily$value)
#' value     <- standardize_inv_vec(value_std,
#'                                  mean = 2261.60682492582,
#'                                  sd   = 175.603721730477)
#'
#' # --- MUTATE ----
#'
#' m4_daily %>%
#'     group_by(id) %>%
#'     mutate(value_std = standardize_vec(value))
#'
#' @name standardize_vec
#' @export
NULL

#' @rdname standardize_vec
#' @export
standardize_vec <- function(x, mean = NULL, sd = NULL, silent = FALSE) {

    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")

    m <- mean
    s <- sd

    if (is.null(mean)) {
        m <- mean(x, na.rm = T)
    }
    if (is.null(sd)) {
        s <- stats::sd(x, na.rm = T)
    }

    if (!silent) {
        if (is.null(mean)) {
            if (is.null(sd)) {
                message(stringr::str_glue("Standardization Parameters
                                  mean: {m}
                                  standard deviation: {s}"))
            }
        }
    }

    (x - m) / s

}

#' @rdname standardize_vec
#' @export
standardize_inv_vec <- function(x, mean, sd) {

    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")

    if (rlang::is_missing(mean)) {
        rlang::abort("`mean` is missing with no default.")
    }
    if (rlang::is_missing(sd)) {
        rlang::abort("`sd` is missing with no default.")
    }

    (x * sd) + mean
}



