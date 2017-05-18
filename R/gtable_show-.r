#' Is a given unit 'small'?
#'
#' Uses a holistic approach to determine whether a unit is 'small',
#' i.e. less than 1 cm, 1 line, 10 pt, or 0.4 in.
#'
#' Based on arbitraily chosen definitions of 'small', this function can return
#' \code{TRUE} or \code{FALSE} if a unit is 'small'.
#'
#' So far, less than 1 cm, 1 line, 10 pt, or 0.4 inches is defined as being
#' 'small'.
#' Unresolved sizes, suchs as 'grobheight', 'grobwidth', or 'null' are not
#' small.
#' Units based on arithmetic, such as sum of multiple units,
#' are also \emph{not} small.
#' \code{NA}s are returned for undecided sizes.
#'
#' @param x A unit.
#' @return Logical or \code{NA}.
is.small <- function(x) {
  #if (is.list(x) & !inherits(x[[1]], 'unit.list') & length(x) == 1) x <- x[[1]]
  #if (inherits(x, 'unit.list')) return(FALSE)
  if (!is.unit(x)) stop('`h` is not a unit.')
  if (is.null(attr(x, 'unit'))) return(FALSE)
  if (as.numeric(x) == 1 & attr(x, 'unit') == 'null') return(FALSE)
  if (as.numeric(x) == 0) return(TRUE)
  n <- as.numeric(x)
  r <- switch(attr(x, 'unit'),
              'null'= FALSE,
              'line'= n < 1,
              'in' = n < 0.40,
              'pt'= n < 10,
              'cm' = n < 1,
              'grobheight' = FALSE,
              'grobwidth' = FALSE,
              NA) # i.e. not implemented

  return(r)
}

#' Visualise underlying gtable layout.
#'
#' Visualises the table structure or the names of the gtable's components.
#'
#' These functions are highly similar to
#' \code{\link[gtable]{gtable_show_layout}}.
#' \code{gtable_show_grill} draws the grid of the underlying table, and places
#' row and column indicies in the margin.
#' \code{gtable_show_names} replaces the grobs with a semi-transparent rectangle
#' and the component's name.
#'
#' @param x A gtable object. If given a ggplot object, it is converted to a
#'          gtable object with \code{\link[ggplot2]{ggplotGrob}}.
#' @param plot Logical. When \code{TRUE} (default), draws resulting gtable
#'             object on a new page.
#' @return Modified gtable object, invisibly.
#' @rdname gtable_show
#' @import ggplot2 gtable grid
#' @export
#' @examples
#' library(ggplot2)
#' library(gtable)
#' library(grid)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' gtable_show_grill(p)
gtable_show_grill <- function(x, plot=TRUE) {
  if (is.ggplot(x)) x <- ggplotGrob(x)

  gp.gutter <- gpar(colour='grey', lty='dashed')
  if (is.null(x$vp)) {
    x$vp <- viewport(clip = 'on')
  }

  x <- gtable_add_cols(x, unit(2, 'line'), 0)
  for (i in 2:nrow(x)) {
    x <- gtable_add_grob(x, t=i, l=1, clip='off', grobs=grobTree(
      textGrob(sprintf('[%d, ]', i-1)),
      linesGrob(x=unit(c(-100,100), 'npc'), y=1, gp=gp.gutter),
      linesGrob(x=unit(c(-100,100), 'npc'), y=0, gp=gp.gutter)
    ), name='lemon')
    if (is.small(x$heights[[i]])) x$heights[[i]] <- unit(1.2, 'line')
  }
  x <- gtable_add_rows(x, unit(1, 'line'), 0)
  for (i in 2:ncol(x)) {
    x <- gtable_add_grob(x, t=1, l=i, clip='off', grobs=grobTree(
      textGrob(sprintf('[ ,%d]', i-1)),
      linesGrob(x=1, unit(c(-100, 100), 'npc'), gp=gp.gutter),
      linesGrob(x=0, unit(c(-100, 100), 'npc'), gp=gp.gutter)
    ), name='lemon')
    if (is.small(x$widths[[i]])) x$widths[[i]] <- unit(2, 'line')
  }

  if (plot) {
    grid.newpage()
    grid.draw(x)
  }

  invisible(x)
}

#' @inheritParams gtable_show_grill
#' @param rect.gp Graphical parameters (\code{\link[grid]{gpar}}) for background drop.
#' @rdname gtable_show
#' @import ggplot2 gtable grid
#' @export
#' @examples
#' library(ggplot2)
#' library(gtable)
#' library(grid)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' gtable_show_names(p)
gtable_show_names <- function(x, plot=TRUE, rect.gp=gpar(col='black', fill='white', alpha=1/4)) {
  if (is.ggplot(x)) x <- ggplotGrob(x)

  for (i in 1:nrow(x$layout)) {
    if (x$layout$name[i] == 'lemon') next
    if (grepl('ylab', x$layout$name[i])) {
      rot <- 90
    } else if (grepl('-l', x$layout$name[i])) {
      rot <- 90
    } else if (grepl('-r', x$layout$name[i])) {
      rot <- 90
    } else {
      rot <- 0
    }

    r <- rectGrob(gp=rect.gp)
    t <- textGrob(x$layout$name[i], rot = rot)
    x$grobs[[i]] <- grobTree(r, t)
  }

  if (plot) {
    grid.newpage()
    grid.draw(x)
  }

  invisible(x)
}
