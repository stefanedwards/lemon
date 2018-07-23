# lemon_plot routines for hijacking ggplot_build and ggplot_gtable
# Key:
# class    --> lemon class
# ggplot   --> lemon_plot
# ggplot_built --> built_lemon
# (scale) --> lemon_scale
# ScaleContinuousPosition --> LemonContinuousPosition


#' @include ggplot2.r
NULL


# Adds a classname to an object, if it does not already have it.
prependClass <- function(object, new_class) {
if (!inherits(plot, new_class)) 
  class(object) <- c(new_class, class(object))
object
}


#' Lemon plots; ggplots with extended functionality.
#' 
#' Lemon plot basically hijacks ggplot2's building process and allows us
#' to change some things.
#' 
#' @param plot ggplot object
#' @keywords internal
#' @export
#' @rdname lemon_plot
as.lemon_plot <- function(plot) {
  if (inherits(plot, 'lemon_plot')) 
    return(plot)
  if (!'axis_annotation' %in% names(plot))
    plot$axis_annotation <- axis_annotation_list()
  prependClass(plot, 'lemon_plot')
}

#' @keywords internal
#' @rdname lemon_plot
#' @export
ggplot_build.lemon_plot <- function(plot) {
  g_built <- NextMethod() 
  prependClass(g_built, 'built_lemon')
}

#' @keywords internal
#' @rdname lemon_plot
#' @export
ggplot_gtable.built_lemon <- function(data) {
  gtable <- NextMethod()
  if ('axis_annotation' %in% names(data$plot) && 
      data$plot$axis_annotation$n() > 0) {
    cat("I'm gonna draw some annotations!\n")
  }
  gtable
}

#' @keywords internal
#' @rdname lemon_plot
#' @export
ggplot_add.lemon_scale <- function(object, plot, object_name) {
  plot$scales$add(object)
  plot <- prependClass(plot, 'lemon_plot')
  plot
}