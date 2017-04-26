#' @include ggplot2.r
NULL

# facet_rep_grid ------------------------
NULL

#' Lay out panels in a grid with axes replicated across all panels.
#' 
#' \strong{Documentation is straight from \code{\link[ggplot2]{facet_grid}}.}
#' Only addition is this function \emph{copies the axis across all panels}. 
#' \code{repeat.tick.labels=FALSE} removes the labels (numbering) on an axis.
#'
#' \code{facet_grid} forms a matrix of panels defined by row and column
#' facetting variables. It is most useful when you have two discrete
#' variables, and all combinations of the variables exist in the data.
#'
#' @param facets a formula with the rows (of the tabular display) on the LHS
#'   and the columns (of the tabular display) on the RHS; the dot in the
#'   formula is used to indicate there should be no faceting on this dimension
#'   (either row or column). The formula can also be provided as a string
#'   instead of a classical formula object
#' @param margins either a logical value or a character
#'   vector. Margins are additional facets which contain all the data
#'   for each of the possible values of the faceting variables. If
#'   \code{FALSE}, no additional facets are included (the
#'   default). If \code{TRUE}, margins are included for all faceting
#'   variables. If specified as a character vector, it is the names of
#'   variables for which margins are to be created.
#' @param scales Are scales shared across all facets (the default,
#'   \code{"fixed"}), or do they vary across rows (\code{"free_x"}),
#'   columns (\code{"free_y"}), or both rows and columns (\code{"free"})
#' @param space If \code{"fixed"}, the default, all panels have the same size.
#'   If \code{"free_y"} their height will be proportional to the length of the
#'   y scale; if \code{"free_x"} their width will be proportional to the
#'  length of the x scale; or if \code{"free"} both height and width will
#'  vary.  This setting has no effect unless the appropriate scales also vary.
#' @param labeller A function that takes one data frame of labels and
#'   returns a list or data frame of character vectors. Each input
#'   column corresponds to one factor. Thus there will be more than
#'   one with formulae of the type \code{~cyl + am}. Each output
#'   column gets displayed as one separate line in the strip
#'   label. This function should inherit from the "labeller" S3 class
#'   for compatibility with \code{\link{labeller}()}. See
#'   \code{\link{label_value}} for more details and pointers to other
#'   options.
#' @param as.table If \code{TRUE}, the default, the facets are laid out like
#'   a table with highest values at the bottom-right. If \code{FALSE}, the
#'   facets are laid out like a plot with the highest value at the top-right.
#' @param switch By default, the labels are displayed on the top and
#'   right of the plot. If \code{"x"}, the top labels will be
#'   displayed to the bottom. If \code{"y"}, the right-hand side
#'   labels will be displayed to the left. Can also be set to
#'   \code{"both"}.
#' @param shrink If \code{TRUE}, will shrink scales to fit output of
#'   statistics, not raw data. If \code{FALSE}, will be range of raw data
#'   before statistical summary.
#' @param drop If \code{TRUE}, the default, all factor levels not used in the
#'   data will automatically be dropped. If \code{FALSE}, all factor levels
#'   will be shown, regardless of whether or not they appear in the data.
#' @param repeat.tick.labels Logical, removes the labels (numbering) on an axis when \code{FALSE}.
#' @importFrom ggplot2 ggproto
#' @export
facet_rep_grid <- function(facets, shrink=TRUE, repeat.tick.labels=FALSE, .debug=FALSE, ...) {
  f <- facet_grid(facets, shrink=shrink, ...)
  params <- append(f$params, list(repeat.tick.labels=repeat.tick.labels, .debug=.debug))
  ggproto(NULL, FacetGridRepeatLabels, 
          shrink=shrink,
          params=params)
}

#' Removes labels from axis grobs.
#' 
#' Called from FacetGridRepeatLabels.
#' 
#' @param axisgrob Grob with an axis.
#' @keywords interal
remove_labels_from_axis <- function(axisgrob) {
  if (inherits(axisgrob, 'zeroGrob')) return(axisgrob)
  a <- which(sapply(axisgrob$children, `[[`, i='name') == 'axis')
  d <- grepl('titleGrob', sapply(axisgrob$children[[a]]$grobs, `[[`, i='name'))
  if (sum(d) > 0) {
    axisgrob$children[[a]] <- do.call(gList, axisgrob$children[[a]]$grobs[!d])
    if (length(axisgrob$width$arg1) == 2) axisgrob$width$arg1 <- axisgrob$width$arg1[attr(axisgrob$width$arg1, 'unit') != 'grobwidth']
    if (length(axisgrob$height$arg1) == 2) axisgrob$height$arg1 <- axisgrob$height$arg1[attr(axisgrob$height$arg1, 'unit') != 'grobheight']
    #if (length(axisgrob$children[[a]]$heights) == 2) axisgrob$children[[a]]$heights <- axisgrob$children[[a]]$heights[!d]
  }
  axisgrob
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 FacetGrid ggproto
#' @importFrom gtable gtable_add_cols gtable_add_grob gtable_add_rows
FacetGridRepeatLabels <- ggplot2::ggproto('FacetGridRepeatLabels', `_inherit`=ggplot2::FacetGrid,
  draw_panels  = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    table <- FacetGrid$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    if (params$.debug) { saveRDS(table, file='table.rds')}
    
    # Add axes across all panels
    panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
    #cord <- do.call(rbind, strsplit(panels$name, split='-', fixed = TRUE))
    panels$col <- as.integer(as.factor(panels$l))# as.integer(cord[,3])
    panels$row <- as.integer(as.factor(panels$t))#as.integer(cord[,2])
    
    axis_b <- table$grobs[grepl('axis-b-[[:digit:]]+', table$layout$name)]
    axis_l <- table$grobs[grepl('axis-l-[[:digit:]]+', table$layout$name)]
    axis_t <- table$grobs[grepl('axis-t-[[:digit:]]+', table$layout$name)]
    axis_r <- table$grobs[grepl('axis-r-[[:digit:]]+', table$layout$name)]
    
    if (params$repeat.tick.labels == FALSE) {
      axis_b <- lapply(axis_b, remove_labels_from_axis)
      axis_l <- lapply(axis_l, remove_labels_from_axis)
      axis_t <- lapply(axis_t, remove_labels_from_axis)
      axis_r <- lapply(axis_r, remove_labels_from_axis)
    }
    
    panel_range <- find_panel(table)
    panel_range[,c('col','row')] <- c(max(panels$col), max(panels$row))
    
    l_axis_column_added <- logical(panel_range$col)
    r_axis_column_added <- logical(panel_range$col)
    t_axis_row_added <- logical(panel_range$row)
    b_axis_row_added <- logical(panel_range$row)
    
    for (i in nrow(panels):1) {
      p <- panels[i,]
      # Bottom
      if (p$row != panel_range$row & !inherits(axis_b[[p$col]], 'zeroGrob')) { # panel not in bottom row, (add row), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (b_axis_row_added[p$row] == FALSE) {
          b_axis_row_added[p$row] <- TRUE
          table <- gtable_add_rows(table, max_height(axis_b), coord$b)
        }
        table <- gtable_add_grob(table, axis_b[[p$col]], t=coord$b+1, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-b-%d-%d', p$col, p$row))
      }
      # Left
      if (p$col > 1 & !inherits(axis_l[[p$row]], 'zeroGrob')) { # panel is not left-most column, (add column), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (l_axis_column_added[p$col] == FALSE) {
          l_axis_column_added[p$col] <- TRUE
          table <- gtable_add_cols(table, max_width(axis_l), coord$l-1)
          table <- gtable_add_grob(table, axis_l[[p$row]], t=coord$t, b=coord$b, l=coord$l, clip='off', name=sprintf('axis-l-%d-%d', p$row, p$col))
        } else {
          table <- gtable_add_grob(table, axis_l[[p$row]], t=coord$t, b=coord$b, l=coord$l-1, clip='off', name=sprintf('axis-l-%d-%d', p$row, p$col))
        }
      }
      # Top
      if (p$row > 1 & !inherits(axis_t[[p$col]], 'zeroGrob')) { # panel not in top row, (add row), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (t_axis_row_added[p$row] == FALSE) {
          t_axis_row_added[p$row] <- TRUE
          table <- gtable_add_rows(table, max_height(axis_t), coord$t-1)
          table <- gtable_add_grob(table, axis_t[[p$col]], t=coord$t, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-t-%d-%d', p$col, p$row))
        } else {
          table <- gtable_add_grob(table, axis_t[[p$col]], t=coord$t-1, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-t-%d-%d', p$col, p$row))        
        }
        
      }
      # Right
      if (p$col != panel_range$col & !inherits(axis_l[[p$row]], 'zeroGrob')) { # panel is not right-most, (add colun), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (r_axis_column_added[p$col] == FALSE) {
          r_axis_column_added[p$col] <- TRUE
          table <- gtable_add_cols(table, max_width(axis_r), coord$r)
        }
        table <- gtable_add_grob(table, axis_r[[p$row]], t=coord$t, b=coord$b, l=coord$r+1, clip='off', name=sprintf('axis-r-%d-%d', p$row, p$col))
      }
      
    }
    
    table
  }
)

# facet_wrap ---------------
NULL

#' Wrap a 1d ribbon of panels into 2d
#'
#' \code{facet_wrap} wraps a 1d sequence of panels into 2d. This is generally
#' a better use of screen space than \code{\link{facet_grid}} because most
#' displays are roughly rectangular.
#'
#' @param facets Either a formula or character vector. Use either a
#'   one sided formula, \code{~a + b}, or a character vector, \code{c("a", "b")}.
#' @param nrow,ncol Number of rows and columns.
#' @param scales should Scales be fixed (\code{"fixed"}, the default),
#'   free (\code{"free"}), or free in one dimension (\code{"free_x"},
#'   \code{"free_y"}).
#' @param strip.position By default, the labels are displayed on the top of
#'   the plot. Using \code{strip.position} it is possible to place the labels on
#'   either of the four sides by setting \code{strip.position = c("top",
#'   "bottom", "left", "right")}
#' @param dir Direction: either "h" for horizontal, the default, or "v", for
#'   vertical.
#' @inheritParams facet_rep_grid
#' @export
facet_rep_wrap <- function(facets, shrink=TRUE, repeat.tick.labels=FALSE, .debug=FALSE, ...) {
  f <- facet_wrap(facets, shrink=shrink, ...)
  params <- append(f$params, list(repeat.tick.labels=repeat.tick.labels, .debug=.debug))
  ggproto(NULL, FacetWrapRepeatLabels, 
          shrink=shrink,
          params=params)
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @importFrom ggplot2 FacetWrap ggproto
#' @importFrom gtable gtable_add_cols gtable_add_grob gtable_add_rows
FacetWrapRepeatLabels <- ggplot2::ggproto('FacetWrapRepeatLabels', `_inherit`=ggplot2::FacetWrap,
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    table <- FacetWrap$draw_panels(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params)
    if (params$.debug) { saveRDS(table, file='table.rds')}
    
    # Add axes across all panels
    panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
    #cord <- do.call(rbind, strsplit(panels$name, split='-', fixed = TRUE))
    panels$col <- as.integer(as.factor(panels$l))# as.integer(cord[,3])
    panels$row <- as.integer(as.factor(panels$t))#as.integer(cord[,2])
    
    # axes are placed on all four sides of **all** panels, but are in most cases zeroGrobs.
    # 
    
    axis_b <- table$grobs[grepl('axis-b-[[:digit:]]+', table$layout$name)]
    axis_l <- table$grobs[grepl('axis-l-[[:digit:]]+', table$layout$name)]
    axis_t <- table$grobs[grepl('axis-t-[[:digit:]]+', table$layout$name)]
    axis_r <- table$grobs[grepl('axis-r-[[:digit:]]+', table$layout$name)]
    
    if (params$repeat.tick.labels == FALSE) {
      axis_b <- lapply(axis_b, remove_labels_from_axis)
      axis_l <- lapply(axis_l, remove_labels_from_axis)
      axis_t <- lapply(axis_t, remove_labels_from_axis)
      axis_r <- lapply(axis_r, remove_labels_from_axis)
    }
    
    # panel_range <- ggplot2:::find_panel(table)
    # panel_range[,c('col','row')] <- c(max(panels$col), max(panels$row))
    # 
    # l_axis_column_added <- logical(panel_range$col)
    # r_axis_column_added <- logical(panel_range$col)
    # t_axis_row_added <- logical(panel_range$row)
    # b_axis_row_added <- logical(panel_range$row)
    
    
    
    for (i in nrow(panels):1) {
      p <- panels[i,]
      # Bottom
      if (p$row != panel_range$row & !inherits(axis_b[[p$col]], 'zeroGrob')) { # panel not in bottom row, (add row), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (b_axis_row_added[p$row] == FALSE) {
          b_axis_row_added[p$row] <- TRUE
          table <- gtable_add_rows(table, max_height(axis_b), coord$b)
        }
        table <- gtable_add_grob(table, axis_b[[p$col]], t=coord$b+1, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-b-%d-%d', p$col, p$row))
      }
      # Left
      if (p$col > 1 & !inherits(axis_l[[p$row]], 'zeroGrob')) { # panel is not left-most column, (add column), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (l_axis_column_added[p$col] == FALSE) {
          l_axis_column_added[p$col] <- TRUE
          table <- gtable_add_cols(table, max_width(axis_l), coord$l-1)
          table <- gtable_add_grob(table, axis_l[[p$row]], t=coord$t, b=coord$b, l=coord$l, clip='off', name=sprintf('axis-l-%d-%d', p$row, p$col))
        } else {
          table <- gtable_add_grob(table, axis_l[[p$row]], t=coord$t, b=coord$b, l=coord$l-1, clip='off', name=sprintf('axis-l-%d-%d', p$row, p$col))
        }
      }
      # Top
      if (p$row > 1 & !inherits(axis_t[[p$col]], 'zeroGrob')) { # panel not in top row, (add row), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (t_axis_row_added[p$row] == FALSE) {
          t_axis_row_added[p$row] <- TRUE
          table <- gtable_add_rows(table, max_height(axis_t), coord$t-1)
          table <- gtable_add_grob(table, axis_t[[p$col]], t=coord$t, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-t-%d-%d', p$col, p$row))
        } else {
          table <- gtable_add_grob(table, axis_t[[p$col]], t=coord$t-1, l=coord$l, r=coord$r, clip='off', name=sprintf('axis-t-%d-%d', p$col, p$row))        
        }
        
      }
      # Right
      if (p$col != panel_range$col & !inherits(axis_l[[p$row]], 'zeroGrob')) { # panel is not right-most, (add colun), add axis
        coord <- table$layout[table$layout$name == p$name, ]
        if (r_axis_column_added[p$col] == FALSE) {
          r_axis_column_added[p$col] <- TRUE
          table <- gtable_add_cols(table, max_width(axis_r), coord$r)
        }
        table <- gtable_add_grob(table, axis_r[[p$row]], t=coord$t, b=coord$b, l=coord$r+1, clip='off', name=sprintf('axis-r-%d-%d', p$row, p$col))
      }
      
    }
    
    table
  }
)                      