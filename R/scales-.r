
#' @include ggplot2.r
NULL

#' Position scales for continuous data (x & y)
#'
#' `scale_x_continuous` and `scale_y_continuous` are the default
#' scales for continuous x and y aesthetics. There are three variants
#' that set the `trans` argument for commonly used transformations:
#' `scale_*_log10`, `scale_*_sqrt` and `scale_*_reverse`.
#'
#' For simple manipulation of labels and limits, you may wish to use
#' [labs()] and [lims()] instead.
#'
#' @inheritParams continuous_scale
#' @family position scales
#' @param ... Other arguments passed on to `scale_(x|y)_continuous`
#' @example inst/examples/lemon-scales-ex.r
#'
#' @name scale_continuous
#' @aliases NULL
NULL

#' @rdname scale_continuous
#'
#' @param sec.axis specify a secondary axis
#'
#' @seealso [sec_axis()] for how to specify secondary axes
#' @export
lemon_x_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), labels = waiver(),
                               limits = NULL, expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity",
                               position = "bottom", sec.axis = waiver()) {
  sc <- continuous_scale(
    c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none", position = position, super = ScaleContinuousPosition
  )
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
    sc$secondary.axis <- sec.axis
  }
  sc
}

#' @rdname scale_continuous
#' @export
lemon_y_continuous <- function(name = waiver(), breaks = waiver(),
                               minor_breaks = waiver(), labels = waiver(),
                               limits = NULL, expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity",
                               position = "left", sec.axis = waiver()) {
  sc <- continuous_scale(
    c("ytick"),
    "position_c", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = "none", position = position, super = LemonContinuousPosition
  )
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
    sc$secondary.axis <- sec.axis
  }
  prependClass(sc, 'lemon_scale')
  sc
}


draw_ticks_y <- function(panel_params, axis, scale, position, theme, data) {
  
}

#' @rdname lemon-ggproto
#' @format NULL
#' @usage NULL
#' @export
LemonContinuousPosition <- ggproto("LemonContinuousPosition", ScaleContinuousPosition
)

#' @keywords internal
#' @format NULL
#' @usage NULL
#' @export
ggplot_add.lemon_scale <- function(object, plot, object_name) {
  plot$scales$add(object)
  plot <- prependClass(plot, 'lemon_plot')
  plot
}


#' @export
geom_ytick <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       show.legend = NA,
                       inherit.aes = FALSE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYtick,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...),
    check.aes = FALSE
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomYtick <- ggproto("GeomYtick", GeomBlank,
       required_aes = c('ytick') #,default_aes = list(label=NA, colour=waiver())
)
