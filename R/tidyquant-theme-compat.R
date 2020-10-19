# tidyquant functions copied to remove dependency on tidyquant

#' @importFrom ggplot2 %+replace%

theme_tq <- function(base_size = 11, base_family = "") {

    # Tidyquant colors
    blue  <- "#2c3e50"
    green <- "#18BC9C"
    white <- "#FFFFFF"
    grey  <- "grey80"

    # Starts with theme_grey and then modify some parts
    ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
        ggplot2::theme(

            # Base Inherited Elements
            line               =  ggplot2::element_line(colour = blue, size = 0.5, linetype = 1,
                                                        lineend = "butt"),
            rect               =  ggplot2::element_rect(fill = white, colour = blue,
                                                        size = 0.5, linetype = 1),
            text               =  ggplot2::element_text(family = base_family, face = "plain",
                                                        colour = blue, size = base_size,
                                                        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                                                        margin = ggplot2::margin(), debug = FALSE),

            # Axes
            axis.line          = ggplot2::element_blank(),
            axis.text          = ggplot2::element_text(size = ggplot2::rel(0.8)),
            axis.ticks         = ggplot2::element_line(color = grey, size = ggplot2::rel(1/3)),
            axis.title         = ggplot2::element_text(size = ggplot2::rel(1.0)),

            # Panel
            panel.background   = ggplot2::element_rect(fill = white, color = NA),
            panel.border       = ggplot2::element_rect(fill = NA, size = ggplot2::rel(1/2), color = blue),
            panel.grid.major   = ggplot2::element_line(color = grey, size = ggplot2::rel(1/3)),
            panel.grid.minor   = ggplot2::element_line(color = grey, size = ggplot2::rel(1/3)),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.spacing      = ggplot2::unit(.75, "cm"),

            # Legend
            legend.key         = ggplot2::element_rect(fill = white, color = NA),
            legend.position    = "bottom",

            # Strip (Used with multiple panels)
            strip.background   = ggplot2::element_rect(fill = blue, color = blue),
            strip.text         = ggplot2::element_text(color = white, size = ggplot2::rel(0.8), margin = ggplot2::margin(t = 5, b = 5)),

            # Plot
            plot.title         = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0,
                                                       margin = ggplot2::margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")),
            plot.subtitle      = ggplot2::element_text(size = ggplot2::rel(0.9), hjust = 0,
                                                       margin = ggplot2::margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),

            # Complete theme
            complete = TRUE
        )
}

theme_tq_dark <- function(base_size = 11, base_family = "") {

    # Tidyquant colors
    blue  <- "#2c3e50"
    green <- "#18BC9C"
    white <- "#FFFFFF"
    grey  <- "grey50"

    # Starts with theme_tq and then invert some colors
    theme_tq(base_size = base_size, base_family = base_family) %+replace%
        ggplot2::theme(

            # Axes
            axis.ticks         = ggplot2::element_line(color = blue, size = ggplot2::rel(1/3)),

            # Panel
            panel.background   = ggplot2::element_rect(fill = grey, color = NA),
            panel.grid.major   = ggplot2::element_line(color = white, size = ggplot2::rel(1/3)),
            panel.grid.minor   = ggplot2::element_line(color = white, size = ggplot2::rel(1/3)),

            # Complete theme
            complete = TRUE
        )
}

theme_tq_green <- function(base_size = 11, base_family = "") {

    # Tidyquant colors
    blue  <- "#2c3e50"
    green <- "#18BC9C"
    white <- "#FFFFFF"
    grey  <- "grey80"

    # Starts with theme_tq and then invert some colors
    theme_tq(base_size = base_size, base_family = base_family) %+replace%
        ggplot2::theme(

            # Axes
            axis.ticks         = ggplot2::element_line(color = blue, size = ggplot2::rel(1/3)),

            # Panel
            panel.background   = ggplot2::element_rect(fill = green, color = NA),
            panel.grid.major   = ggplot2::element_line(color = white, size = ggplot2::rel(1/3)),
            panel.grid.minor   = ggplot2::element_line(color = white, size = ggplot2::rel(1/3)),

            # Complete theme
            complete = TRUE
        )
}

scale_color_tq <- function(..., theme = "light") {

    pal <- switch(theme,
                  "light" = unname(palette_light()) %>% rep(100),
                  "dark"  = unname(palette_dark()) %>% rep(100),
                  "green" = unname(palette_green() %>% rep(100))
    )

    ggplot2::scale_color_manual(values = pal)
}

palette_light <- function() {
    c(
        blue         = "#2c3e50", # blue
        red          = "#e31a1c", # red
        green        = "#18BC9C", # green
        yellow       = "#CCBE93", # yellow
        steel_blue   = "#a6cee3", # steel_blue
        navy_blue    = "#1f78b4", # navy_blue
        light_green  = "#b2df8a", # light_green
        pink         = "#fb9a99", # pink
        light_orange = "#fdbf6f", # light_orange
        orange       = "#ff7f00", # orange
        light_purple = "#cab2d6", # light_purple
        purple       = "#6a3d9a"  # purple
    ) %>% toupper()
}

palette_dark <- function() {
    # Brighter version of palette_light
    c(
        blue         = "#0055AA", # blue
        red          = "#C40003", # red
        green        = "#00C19B", # green
        yellow       = "#EAC862", # yellow
        steel_blue   = "#7FD2FF", # steel_blue
        navy_blue    = "#007ED3", # navy_blue
        light_green  = "#b2df8a", # light_green
        pink         = "#FFACAA", # pink
        light_orange = "#FF9D1E", # light_orange
        lime_green   = "#C3EF00", # lime_green
        light_purple = "#cab2d6", # light_purple
        purple       = "#894FC6"  # purple
    ) %>% toupper()
}

palette_green <- function() {
    # Green compatible version of palette_light
    c(
        blue         = "#0055AA", # blue
        red          = "#C40003", # red
        yellow       = "#EAC862", # yellow
        steel_blue   = "#7FD2FF", # steel_blue
        navy_blue    = "#007ED3", # navy_blue
        creme        = "#F6F4F3", # creme
        pink         = "#FFACAA", # pink
        light_orange = "#FF9D1E", # light_orange
        lime_green   = "#C3EF00", # lime_green
        light_purple = "#cab2d6", # light_purple
        purple       = "#894FC6", # purple
        brown        = "#592E2E"  # brown
    ) %>% toupper()
}

palette_light <- function() {
    c(
        blue         = "#2c3e50", # blue
        red          = "#e31a1c", # red
        green        = "#18BC9C", # green
        yellow       = "#CCBE93", # yellow
        steel_blue   = "#a6cee3", # steel_blue
        navy_blue    = "#1f78b4", # navy_blue
        light_green  = "#b2df8a", # light_green
        pink         = "#fb9a99", # pink
        light_orange = "#fdbf6f", # light_orange
        orange       = "#ff7f00", # orange
        light_purple = "#cab2d6", # light_purple
        purple       = "#6a3d9a"  # purple
    ) %>% toupper()
}
