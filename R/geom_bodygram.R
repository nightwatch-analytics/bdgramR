#' Bodygram polygons
#'
#' @description
#' Visualize bodygrams for reference or with athlete data.
#'
#' @param data A dataframe. Input data from the athlete. If `NULL`, then a reference bodygram is plotted.
#' @param mapping aesthetics, the x,y and grouping part of the aesthetics
#' @param position The position parameter. It defaults to "identity"
#' @param stat The stats parameter. Itdefaults to "identity"
#' @param na.rm Whether to remove NAs in the dataset. It deffaults to FALSE.
#' @param show.legend Whether to show leged in the plot. It defaults to NA
#' @param inherit.aes Whether to inherit the aesthetics. It defaults to TRUE
#' @param ... Other arguments passed on to [layer()]'s `params` argument. These
#'   arguments broadly fall into one of 4 categories below. Notably, further
#'   arguments to the `position` argument, or aesthetics that are required
#'   can *not* be passed through `...`. Unknown arguments that are not part
#'   of the 4 categories below are ignored.
#'   * Static aesthetics that are not mapped to a scale, but are at a fixed
#'     value and apply to the layer as a whole. For example, `colour = "red"`
#'     or `linewidth = 3`. The geom's documentation has an **Aesthetics**
#'     section that lists the available options. The 'required' aesthetics
#'     cannot be passed on to the `params`. Please note that while passing
#'     unmapped aesthetics as vectors is technically possible, the order and
#'     required length is not guaranteed to be parallel to the input data.
#'   * When constructing a layer using
#'     a `stat_*()` function, the `...` argument can be used to pass on
#'     parameters to the `geom` part of the layer. An example of this is
#'     `stat_density(geom = "area", outline.type = "both")`. The geom's
#'     documentation lists which parameters it can accept.
#'   * Inversely, when constructing a layer using a
#'   `geom_*()` function, the `...` argument can be used to pass on parameters
#'     to the `stat` part of the layer. An example of this is
#'     `geom_area(stat = "density", adjust = 0.5)`. The stat's documentation
#'     lists which parameters it can accept.
#'   * The `key_glyph` argument of [`layer()`] may also be passed on through
#'     `...`. This can be one of the functions described as
#'     [key glyphs][draw_key], to change the display of the layer in the legend.
#'
#' @import ggplot2
#'
#' @export
#' @examples
#' bodygram_data <- bodygram()
#' bodygram_plot <- ggplot(data = bodygram_data, aes(x, y, group = Id)) +
#'   geom_bodygram(lty = 3, color = "black", aes(fill = Group))
#' bodygram_plot
#'
geom_bodygram <- function(mapping = NULL, data = NULL, position = "identity",
                         stat = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  list(ggplot2::layer(
    geom = geomBodygram, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  ), ggplot2::scale_y_reverse(), ggplot2::theme_void())
}

geomBodygram <- ggproto("geomBodygram", Geom,
  required_aes = c("x", "y"),
  default_aes = aes(colour = "black", fill = NA , alpha = 1, linetype = 1),
  draw_key = draw_key_polygon,
  draw_group = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    grid::polygonGrob(
      coords$x, coords$y,
      gp = grid::gpar(
        col = coords$colour,
        group = coords$Id,
        fill = coords$fill,
        lty = coords$linetype
      )
    )
  }
)
