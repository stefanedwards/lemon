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
if (!inherits(object, new_class)) 
  class(object) <- c(new_class, class(object))
object
}


#' Lemon plots; ggplots with extended functionality.
#' 
#' Lemon plot basically hijacks ggplot2's building process and allows us
#' to change some things. Used by \code{\link{annotate_y_axis}}.
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
#' @import gtable
#' @export
ggplot_gtable.built_lemon <- function(data) {
  gtable <- NextMethod()
  
  if ('axis_annotation' %in% names(data$plot) && 
    data$plot$axis_annotation$n('y') > 0) {
    
    #right
    g <- data$plot$axis_annotation$draw(
        side='right', 
        is.primary=get_panel_params(data$layout, 1)$y.arrange[2] == 'primary', 
        range=get_panel_y_range(data$layout, 1), 
        theme=plot_theme(data$plot$theme)
    )
    if (!all(sapply(g$children, is.zero))) {
      i <- which(gtable$layout$name == 'axis-r')
      gtable <- gtable::gtable_add_grob(gtable, g, 
                                        t = gtable$layout$t[i],
                                        l = gtable$layout$l[i],
                                        r = gtable$layout$l[i],
                                        b  = gtable$layout$b[i],
                                        name='axis2-r',
                                        clip='off')
      widths <- lapply(g$children[[2]]$grobs, grid::grobWidth)
      gtable$widths[[gtable$layout$l[i]]] <- do.call(grid::unit.pmax, c(widths, gtable$widths[gtable$layout$l[i]]))
    }
    
    #left
    g <- data$plot$axis_annotation$draw(
      side='left', 
      is.primary=get_panel_params(data$layout, 1)$y.arrange[1] == 'primary', 
      range=get_panel_y_range(data$layout, 1), 
      theme=plot_theme(data$plot$theme)
    )
    if (!all(sapply(g$children, is.zero))) {
      i <- which(gtable$layout$name == 'axis-l')
      gtable <- gtable::gtable_add_grob(gtable, g, 
                                        t = gtable$layout$t[i],
                                        l = gtable$layout$l[i],
                                        r = gtable$layout$l[i],
                                        b  = gtable$layout$b[i],
                                        name='axis2-l',
                                        clip='off')
      widths <- lapply(g$children[[2]]$grobs, grid::grobWidth)
      gtable$widths[[gtable$layout$l[i]]] <- do.call(grid::unit.pmax, c(widths, gtable$widths[gtable$layout$l[i]]))
    }
    
  }
  
  if ('axis_annotation' %in% names(data$plot) &&
      data$plot$axis_annotation$n('x') > 0) {
    # top
    g <- data$plot$axis_annotation$draw(
      side='top', 
      is.primary=get_panel_params(data$layout, 1)$x.arrange[1] == 'primary', 
      range=get_panel_x_range(data$layout, 1), 
      theme=plot_theme(data$plot$theme)
    )
    if (!all(sapply(g$children, is.zero))) {
      i <- which(gtable$layout$name == 'axis-t')
      gtable <- gtable::gtable_add_grob(gtable, g, 
                                        t = gtable$layout$t[i],
                                        l = gtable$layout$l[i],
                                        r = gtable$layout$l[i],
                                        b  = gtable$layout$b[i],
                                        name='axis2-t',
                                        clip='off')
      heights <- lapply(g$children[[2]]$grobs, grid::grobHeight)
      gtable$heights[[gtable$layout$l[i]]] <- do.call(grid::unit.pmax, c(heights, gtable$heights[gtable$layout$l[i]]))
    }
    
    # bottom
    g <- data$plot$axis_annotation$draw(
      side='bottom', 
      is.primary=get_panel_params(data$layout, 1)$x.arrange[2] == 'primary', 
      range=get_panel_x_range(data$layout, 1), 
      theme=plot_theme(data$plot$theme)
    )
    if (!all(sapply(g$children, is.zero))) {
      i <- which(gtable$layout$name == 'axis-b')
      gtable <- gtable::gtable_add_grob(gtable, g, 
                                        t = gtable$layout$t[i],
                                        l = gtable$layout$l[i],
                                        r = gtable$layout$l[i],
                                        b  = gtable$layout$b[i],
                                        name='axis2-b',
                                        clip='off')
      heights <- lapply(g$children[[2]]$grobs, grid::grobHeight)
      gtable$heights[[gtable$layout$l[i]]] <- do.call(grid::unit.pmax, c(heights, gtable$heights[gtable$layout$l[i]]))
    }
    
  }
  gtable
}

#' #' @keywords internal
#' #' @rdname lemon_plot
#' #' @export
#' ggplot_add.lemon_scale <- function(object, plot, object_name) {
#'   plot$scales$add(object)
#'   plot <- prependClass(plot, 'lemon_plot')
#'   plot
#' }