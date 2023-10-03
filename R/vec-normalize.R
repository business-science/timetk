#' Normalize to Range (0, 1)
#'
#' Normalization is commonly used to center and scale numeric features to prevent one from
#' dominating in algorithms that require data to be on the same scale.
#'
#' @param x A numeric vector.
#' @param min The population min value in the normalization process.
#' @param max The population max value in the normalization process.
#' @param silent Whether or not to report the automated `min` and `max` parameters as a message.
#'
#' @returns A `numeric` vector with the transformation applied.
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
#' @examples
#' library(dplyr)
#' library(timetk)
#'
#' d10_daily <- m4_daily %>% dplyr::filter(id == "D10")
#'
#' # --- VECTOR ----
#'
#' value_norm <- normalize_vec(d10_daily$value)
#' value      <- normalize_inv_vec(value_norm,
#'                                 min = 1781.6,
#'                                 max = 2649.3)
#'
#' # --- MUTATE ----
#'
#' m4_daily %>%
#'     group_by(id) %>%
#'     mutate(value_norm = normalize_vec(value))
#'
#' @name normalize_vec
#' @export
NULL

#' @rdname normalize_vec
#' @export
normalize_vec <- function(x, min = NULL, max = NULL, silent = FALSE) {

    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")

    mn <- min
    mx <- max

    if (is.null(mn)) {
        mn <- base::min(x, na.rm = T)
    }
    if (is.null(mx)) {
        mx <- base::max(x, na.rm = T)
    }

    if (!silent) {
        if (is.null(max)) {
            if (is.null(min)) {
                message(stringr::str_glue("Normalization Parameters
                                          min: {mn}
                                          max: {mx}"))
            }
        }
    }

    (x - mn) / (mx - mn)

}

#' @rdname normalize_vec
#' @export
normalize_inv_vec <- function(x, min, max) {

    if (!is.numeric(x)) rlang::abort("Non-numeric data detected. 'x' must be numeric.")

    if (rlang::is_missing(min)) {
        rlang::abort("`min` is missing with no default.")
    }
    if (rlang::is_missing(max)) {
        rlang::abort("`max` is missing with no default.")
    }

    x * (max - min) + min
}



