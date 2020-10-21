#' Custom `ggplot2` themes
#'
#' @description Themes that provide some custom styling for `ggplot2` plots.
#'
#' @param base_size Plot font size, given in pts.
#' @param base_family Font family used for the plot.
#' @importFrom ggplot2 theme theme_grey %+replace% element_blank element_text element_line element_rect
#'
#' @name themes
NULL


#' @rdname themes
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' SAM COMPLETE
#' }
theme_design <- function(base_size = 11, base_family = "") {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(),
      axis.text.x = element_text(angle = 45),
      axis.title = element_text(),
      strip.text = element_text()
    )
}


#' @rdname themes
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' SAM COMPLETE
#' }
theme_design_heat <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #        legend.position = "none",
      strip.text.x = element_text(size = 14), # margin = margin(0.15,0,0.15,0, "cm")),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(),
      axis.title = element_text()
    )
}


#' @rdname themes
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' SAM COMPLETE
#' }
theme_scatter <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.position = "none",
      legend.text = element_text(size = 26),
      legend.title = element_text(size = 26),
      panel.grid.minor = element_line(colour = "grey80"),
      panel.grid.major = element_line(colour = "grey80"),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, size = 1.1, colour = "grey80"),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.y = element_text(size = 20, angle = 90),
      axis.title.x = element_text(size = 20),
      #        axis.title = element_blank(),
      strip.text = element_text(size = 16)
    )
}


#' @rdname themes
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' SAM COMPLETE
#' }
theme_barplot <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 16),
      # legend.position = "none",
      panel.grid.minor = element_line(colour = "grey80"),
      panel.grid.major = element_line(colour = "grey80"),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, size = 1.1, colour = "grey80"),
      axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_blank(),
      strip.text = element_text(size = 15, margin = margin(0.17, 0, 0.17, 0, "cm"))
    )
}
