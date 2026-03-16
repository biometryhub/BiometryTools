#' Custom ggplot2 themes
#'
#' Minimal theme helpers used across the package.
#'
#' These functions return `ggplot2::theme()` objects that can be added to a plot
#' with `+`.
#'
#' @param base_size Base font size (in points).
#' @param base_family Base font family.
#'
#' @return A `ggplot2` theme object.
#'
#' @details
#' - `theme_design()` removes most grid/axes clutter and hides the legend.
#' - `theme_design_heat()` is intended for heatmaps (keeps legend, rotates x labels).
#' - `theme_scatter()` uses a light grid and a panel border; larger text defaults.
#' - `theme_barplot()` places the legend at the bottom and rotates x labels.
#'
#' @importFrom ggplot2 theme theme_grey %+replace% element_blank element_text element_line element_rect
#'
#' @name themes
NULL


#' Design theme (no legend)
#'
#' A clean theme for general plots with the legend removed.
#'
#' @rdname themes
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_design()
#' }
theme_design <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
        theme(
            legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_text(),
            axis.text.x = element_text(angle = 0),
            axis.title = element_text(),
            strip.text = element_text(size = 14)
        )
}


#' Design theme for heatmaps
#'
#' A design theme variant intended for heatmaps (legend kept; x labels rotated).
#'
#' @rdname themes
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' df <- expand.grid(x = letters[1:5], y = letters[1:5])
#' df$z <- seq_len(nrow(df))
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_tile() +
#'   theme_design_heat()
#' }
theme_design_heat <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
        theme(
            #        legend.position = "none",
            strip.text.x = element_text(size = 14), #margin = margin(0.15,0,0.15,0, "cm")),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle = 45),
            axis.title = element_text()
        )
}


#' Scatter plot theme
#'
#' Theme tuned for scatter plots (light grid and a panel border).
#'
#' @rdname themes
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_scatter()
#' }
theme_scatter <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
        theme(
            legend.position = "none",
            legend.text = element_text(size = 26),
            legend.title = element_text(size = 26),
            panel.grid.minor = element_line(colour = "grey80"),
            panel.grid.major = element_line(colour = "grey80"),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, size = 1.1, colour = "grey80"),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 20, angle = 90),
            axis.title.x = element_text(size = 20),
            #        axis.title = element_blank(),
            strip.text = element_text(size = 16)
        )
}


#' Bar plot theme
#'
#' Theme tuned for bar plots (legend at bottom; x labels rotated).
#'
#' @rdname themes
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
#'   geom_bar() +
#'   theme_barplot()
#' }
theme_barplot <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
        theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 16),
            panel.grid.minor = element_line(colour = "grey80"),
            panel.grid.major = element_line(colour = "grey80"),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA, size = 1.1, colour = "grey80"),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6),
            axis.text.y = element_text(size = 12, hjust = 1),
            axis.title = element_blank(),
            strip.text = element_text(size = 14)
        )
}
