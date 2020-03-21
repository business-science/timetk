





step_loess <-
    function(recipe,
             ...,
             x,
             span = 0.75,
             names = NULL,
             role = "predictor",
             trained = FALSE,
             columns = NULL,
             objects = NULL,
             skip = FALSE,
             id = rand_id("loess")) {

        if (rlang::quo_is_missing(enquo(x))) {
            message("The `x` parameter is missing. This is normally provided as a date or numeric index.")
        }

        # if (!is_tune(degree) & !is_varying(degree)) {
        #     degree <- as.integer(degree)
        # }

        # if (any(names(options) == "degree")) {
        #     degree <- options$degree
        #     message(
        #         paste(
        #             "The `degree` argument is now a main argument instead of being",
        #             "within `options`."
        #         )
        #     )
        # }


        x_name <- recipe$template %>% select(!! enquo(x)) %>% names()

        add_step(
            recipe,
            step_loess_new(
                terms = ellipse_check(...),
                x = x_name,
                span = span,
                names = names,
                trained = trained,
                role = role,
                columns = columns,
                objects = objects,
                skip = skip,
                id = id
            )
        )
    }

step_loess_new <-
    function(terms, role, trained, columns, objects, x, span, names, skip, id) {
        step(
            subclass = "loess",
            terms = terms,
            role = role,
            names = names,
            trained = trained,
            columns = columns,
            objects = objects,
            x = x,
            span = span,
            skip = skip,
            id = id
        )
    }

model_loess_1 <- function(data, y, span) {

    y_vec <- data %>% pull(!! enquo(y))

    model_loess <- stats::loess(
        y_vec ~ seq_along(y_vec),
        span = span
    )

    model_loess
}

model_loess_2 <- function(data, x, y, span) {

    x_vec <- data %>% pull(!! enquo(x))
    y_vec <- data %>% pull(!! enquo(y))

    # If x is a timestamp (date or date-time) convert to numeric sequence
    if (is_date_class(x_vec)) {
        x_vec <- as.POSIXct(x_vec) %>% as.numeric() %>% as.integer()
    }

    model_loess <- stats::loess(
        y_vec ~ x_vec,
        span = span
    )

    model_loess
}


#' @export
prep.step_loess <- function(x, training, info = NULL, ...) {

    col_names <- terms_select(x$terms, info = info)

    obj <- list()
    for (i in seq_along(col_names)) {
        obj[[i]] <- model_loess_2(training, x$x, y = col_names[i], span = x$span)
        attr(obj[[i]], "var") <- col_names[i]
    }

    step_loess_new(
        terms = x$terms,
        role = x$role,
        trained = TRUE,
        columns = col_names,
        objects = obj,
        x = x$x,
        span = x$span,
        names = x$names,
        skip = x$skip,
        id = x$id
    )
}

#' @export
bake.step_loess <- function(object, new_data, ...) {

    # If x is a timestamp (date or date-time) convert to numeric sequence
    x_vec <- new_data %>% pull(object$x)
    if (is_date_class(x_vec)) {
        x_vec <- as.POSIXct(x_vec) %>% as.numeric() %>% as.integer()
    }

    # Loop through and create variables
    col_names <- object$columns
    models    <- object$objects

    if (!is.null(object$names)) {
        for (i in seq_along(models)) {
            new_data[,object$names[i]] <- predict(models[[i]], x_vec)
        }
    } else {
        for (i in seq_along(models)) {
            new_data[,col_names[i]] <- predict(models[[i]], x_vec)
        }
    }

    new_data
}


print.step_loess <-
    function(x, width = max(20, options()$width - 35), ...) {
        cat("Local Polynomial Regression Fitting (Loess) on ")
        printer(x$columns, x$terms, x$trained, width = width)
        invisible(x)
    }

#' @rdname step_loess
#' @param x A `step_loess` object.
#' @export
tidy.step_loess <- function(x, ...) {
    if (is_trained(x)) {
        res <- tibble(terms = names(x$objects), degree = x$degree)
    } else {
        term_names <- sel2char(x$terms)
        res <- tibble(terms = term_names, degree = x$degree)
    }
    res$id <- x$id
    res
}


#' @rdname tunable.step
#' @export
tunable.step_loess <- function(x, ...) {
    tibble::tibble(
        name = c("degree"),
        call_info = list(
            list(pkg = "dials", fun = "degree_int")
        ),
        source = "recipe",
        component = "step_loess",
        component_id = x$id
    )
}
