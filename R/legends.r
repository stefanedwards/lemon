#' @include ggplot2.r
NULL

#' Extract ggplot legends
#'
#' Extracts the legend ('guide-box') from a ggplot2 object.
#'
#' The extraction is applied \emph{after} the plot is trained and themes are
#' applied. Modifying the legend is easiest by applying themes etc.
#' to the ggplot2 object, before calling \code{g_legend}.
#'
#' An alternative method for extracting the legend is using
#' \code{gtable::\link[gtable]{gtable_filter}}:
#'
#' \preformatted{
#'   gtable_filter(ggplotGrob(a.ggplot.obj), 'guide-box')
#' }
#'
#' This method however returns a \code{gtable} object which encapsulates
#' the entire legend. The legend itself may be a collection of \code{gtable}.
#' We have only noticed a problem with this extra layer when using the returned
#' legend with \code{\link[gridExtra]{arrangeGrob}} (see examples).
#'
#' @param a.gplot ggplot2 or gtable object.
#' @return gtable (grob) object. Draw with \code{\link[grid]{grid.draw}}.
#' @export
#' @author \href{http://baptiste.github.io/}{Baptiste Auguié}
#' @import ggplot2
#' @import gtable
#' @seealso \code{\link{grid_arrange_shared_legend}}, \code{\link{reposition_legend}},
#'          \code{\link[gtable]{gtable_filter}}
#' @examples
#' library(ggplot2)
#' library(gtable)
#' library(grid)
#' library(gridExtra)
#' library(gtable)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) +
#'  geom_point(aes(colour = clarity)) +
#'  theme(legend.position='bottom'))
#'
#' legend <- g_legend(d)
#' grid.newpage()
#' grid.draw(legend)
#'
#' (d2 <- ggplot(dsamp, aes(x=carat, fill=clarity)) +
#'   geom_histogram(binwidth=0.1) +
#'  theme(legend.position='bottom'))
#'
#' grid.arrange(d  + theme(legend.position='hidden'),
#'              d2 + theme(legend.position='hidden'),
#'              bottom=legend$grobs[[1]])
#' # Above fails with more than one guide
#'
#' legend2 <- gtable_filter(ggplotGrob(d), 'guide-box')
#' grid.arrange(d  + theme(legend.position='hidden'),
#'              d2 + theme(legend.position='hidden'),
#'              bottom=legend2$grobs[[1]]$grobs[[1]])
#' # Above fails with more than one guide
#'
#'
g_legend<-function(a.gplot){
  if (!gtable::is.gtable(a.gplot))
    a.gplot <- ggplotGrob(a.gplot)
  #gtable_filter(a.gplot, 'guide-box', fixed=TRUE)
  leg <- which(sapply(a.gplot$grobs, function(x) x$name) == "guide-box")
  a.gplot$grobs[[leg]]
}

#' Guidebox as a column
#'
#' Takes a plot or legend and returns a single guide-box in a single column,
#' for embedding in e.g. tables.
#'
#' @param legend A ggplot2 plot or the legend extracted with \code{\link{g_legend}}.
#'   \emph{Do not} provide a \code{\link[ggplot2]{ggplotGrob}} as it is indistinguisble
#'   from a legend.
#' @param which.legend Integer, a legend can contain multiple guide-boxes (or vice versa?).
#'   Use this argument to select which to use.
#' @param add.title Does nothing yet.
#' @return A \code{\link[gtable]{gtable}} with keys and labels reordered into
#'   a single column and each pair of keys and labels in the same cell.
#' @export
#' @seealso \code{\link{g_legend}}
#' @import ggplot2
#' @import gtable
#' @import gridExtra
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(diamonds, aes(x=x, y=y, colour=cut)) + geom_point()
#' guidebox_as_column(p)
#' p <- p + guides(colour=guide_legend(ncol=2, byrow=TRUE))
#' guidebox_as_column(p)
guidebox_as_column <- function(legend, which.legend=1, add.title=FALSE) {
  if (ggplot2::is_ggplot(legend)) {
    legend <- g_legend(legend)
  }
  if (gtable::is.gtable(legend) & legend$name == 'guide-box') {
    legend <- legend$grobs[[which.legend]]
  }

  # deduct number of keys from legend
  # assumes background, labels, and keys, are all in same order (albeit intermixed)
  bgs <- which(grepl('-bg', legend$layout$name))
  keys <- which(grepl('key-[0-9]+-[0-9]+', legend$layout$name))
  labels <- which(grepl('label-[0-9]+-[0-9]+', legend$layout$name))

  all.keys <- list()
  label.width <- max(legend$widths[legend$layout$l[labels]])
  for (i in 1:length(bgs)) {
    t = legend$layout$t[bgs[i]]
    lr = range(legend$layout[c(bgs[i], keys[i], labels[i]), c('l','r')])
    b = legend$layout$b[bgs[i]]

    n <- gtable::gtable(heights = legend$heights[t:b],
                        widths = unit.c(legend$widths[lr[1]:(lr[2]-1)], label.width),
                        name = paste0(legend$layout$name[keys[i]], '-all')
    )
    n <- gtable::gtable_add_grob(n, legend$grobs[bgs[i]], t=1, l=1, name=legend$layout$name[bgs[i]])
	if (bgs[i] != keys[i]) {
		n <- gtable::gtable_add_grob(n, legend$grobs[keys[i]], t=1, l=1, name=legend$layout$name[keys[i]])
	}
    n <- gtable::gtable_add_grob(n, legend$grobs[labels[i]], t=1, l=2, name=legend$layout$name[labels[i]])

    all.keys[[i]] <- n
  }
  #all.keys$size <- 'first'
  all.keys$ncol <- 1
  do.call(gridExtra::arrangeGrob, all.keys)

  # if (add.title) { ## argument
  #   i <- which(legend$layout$name == 'title')
  #   if (length(i) > 0) {
  #     all.keys <- rbind(gtable::gtable_row(
  #       name = 'title', grobs=legend$grobs[i]), all.keys, size='last')
  #   }
  # }
  # all.keys
}


#' Share a legend between multiple plots
#'
#' Extract legend, combines plots using \code{\link[gridExtra]{arrangeGrob}} /
#' \code{grid.arrange},
#' and places legend in a margin.
#'
#'
#'
#' @param ... Objects to plot. First argument should be a ggplot2 object,
#'            as the legend is extracted from this.
#'            Other arguments are passed on to
#'            \code{\link[gridExtra]{arrangeGrob}},
#'            including named arguments that are not defined for \code{grid_arrange_shared_legend}.
#'            ggplot2 objects have their legends hidden.
#' @param ncol Integer, number of columns to arrange plots in.
#' @param nrow Integer, number of rows to arrange plots in.
#' @param position 'bottom' or 'right' for positioning legend.
#' @param plot Logical, when \code{TRUE} (default), draws combined plot on a
#'             new page.
#' @return gtable of combined plot, invisibly.
#'   Draw  gtable object using \code{\link[grid]{grid.draw}}.
#' @author
#'   Originally brought to you by
#'   \href{http://baptiste.github.io/}{Baptiste Auguié}
#'   (\url{https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs})
#'   and
#'   \href{http://rpubs.com/sjackman}{Shaun Jackman}
#'   (\href{http://rpubs.com/sjackman/grid_arrange_shared_legend}{original}).
#'   Stefan McKinnon Edwards added left and top margins.
#' @import ggplot2 gridExtra grid
#' @export
#' @seealso \code{\link{g_legend}}, \code{\link{reposition_legend}}
#' @examples
#' library(ggplot2)
#' dsamp <- diamonds[sample(nrow(diamonds), 300), ]
#' p1 <- qplot(carat, price, data = dsamp, colour = clarity)
#' p2 <- qplot(cut, price, data = dsamp, colour = clarity)
#' p3 <- qplot(color, price, data = dsamp, colour = clarity)
#' p4 <- qplot(depth, price, data = dsamp, colour = clarity)
#' grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 4, nrow = 1)
#' grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2)
#'
#' # Passing on plots in a grob are not touched
#' grid_arrange_shared_legend(p1, gridExtra::arrangeGrob(p2, p3, p4, ncol=3), ncol=1, nrow=2)
#'
#' # We can also pass on named arguments to arrangeGrob:
#' title <- grid::textGrob('This is grob', gp=grid::gpar(fontsize=14, fontface='bold'))
#' nt <- theme(legend.position='none')
#' grid_arrange_shared_legend(p1,
#'    gridExtra::arrangeGrob(p2+nt, p3+nt, p4+nt, ncol=3), ncol=1, nrow=2,
#'    top=title)
grid_arrange_shared_legend <- function(...,
                                       ncol = length(list(...)),
                                       nrow = 1,
                                       position = c("bottom", "right","top","left"),
                                       plot=TRUE)
  {

  plots <- list(...)
  position <- match.arg(position)
  #g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  #legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  legend <- g_legend(plots[[1]] + theme(legend.position = position))
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) {
    if (is.ggplot(x)) { x + theme(legend.position="none") } else { x }})
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
   "top" = arrangeGrob(
      legend,
      do.call(arrangeGrob, gl),
      ncol = 1,
      heights = unit.c(lheight, unit(1, "npc") - lheight)),
   "bottom" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)),
   "left" = arrangeGrob(
      legend,
      do.call(arrangeGrob, gl),
      ncol = 2,
      widths = unit.c(lwidth, unit(1, "npc") - lwidth)),
   "right" = arrangeGrob(
      do.call(arrangeGrob, gl),
     legend,
     ncol = 2,
     widths = unit.c(unit(1, "npc") - lwidth, lwidth))
   )

  if (plot) {
    grid.newpage()
    grid.draw(combined)
  }

  # return gtable invisibly
  invisible(combined)
}
arrangeGrob <- gridExtra::arrangeGrob


#' Reposition a legend onto a panel
#'
#' Repositions a legend onto a panel, by either taking it from the same ggplot,
#' or by using another. Works on both ggplot2 and gtable objects, and can accept
#' any grob as legend.
#'
#' To modify the look of the legend, use themes and the natural ggplot functions
#' found in \code{\link[ggplot2]{guide_legend}}.
#'
#' \emph{Positioning} is done by argument \code{position} which places the panel
#' relative in \code{panel} (see below).
#' \code{position} resolves to three variables, \code{x}, \code{y}, and \code{just}.
#' \code{x} and \code{y} is the coordinate in \code{panel}, where the anchorpoint of
#' the legend (set via \code{just}) is placed.
#' In other words, \code{just='bottom right'} places the bottom right corner of
#' the legend at coordinates \code{(x,y)}.
#'
#' The positioning can be set by argument \code{position} alone, which can be
#' further nudged by setting \code{position}, \code{x}, and \code{y}.
#' Alternatively, manually positioning can be obtained by setting arguments.
#' \code{x}, \code{y}, and \code{just}.
#'
#' \emph{Panel} name is by default \code{panel}, but when using facets it typically
#' takes the form \code{panel-{col}-{row}}, but not for wrapped facets.
#' Either print result from \code{\link[ggplot2]{ggplotGrob}} or use
#' \code{\link{gtable_show_names}} to display all the names of the gtable
#' object.
#'
#' \code{panel} takes multiple names, and will then use these components'
#' extremes for placing the legend.
#'
#' If \code{panel} is an integer vector of length 2 or 4, these elements are
#' used directly for top-left and bottom-right coordinates.
#'
#' @param aplot a ggplot2 or gtable object.
#' @param position Where to place the legend in the panel.
#'                 Overrules  \code{just} argument.
#' @param legend The legend to place, if \code{NULL} (default),
#'               it is extracted from \code{aplot} if
#'               this is a ggplot2 object.
#' @param panel Name of panel in gtable. See description.
#' @param x horisontal coordinate of legend, with 0 at left.
#' @param y vertical coordiante of legend, with 0 at bottom.
#' @param just 'Anchor point' of legend; it is this point of the legend that is
#'             placed at the \code{x} and \code{y} coordinates.
#' @param offset Numeric vector, sets distance from edge of panel.
#'               First element for horisontal distance, second for vertical.
#'               Not used by arguments \code{x} and \code{y}.
#' @param name,clip,z Parameters forwarded to
#'             \code{\link[gtable]{gtable_add_grob}}.
#' @param plot Logical, when \code{TRUE} (default), draws plot with legend
#'             repositioned on a new page.
#' @return
#'   gtable object, invisibly, with legend repositioned.
#'   Can be drawn with \code{\link[grid]{grid.draw}}.
#' @import ggplot2 grid gridExtra gtable
#' @author Stefan McKinnon Edwards <sme@@iysik.com>
#' @seealso \code{\link{g_legend}}, \code{\link{grid_arrange_shared_legend}}
#'   and \code{\link{gtable_show_names}} for displaying names of facet's panels.
#' @export
#' @examples
#' library(ggplot2)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) +
#'  geom_point(aes(colour = clarity)))
#'
#' reposition_legend(d + theme(legend.position='bottom'), 'bottom right')
#'
#' # To change the orientation of the legend, use theme's descriptors.
#' reposition_legend(d + theme(legend.position='bottom'), 'top left')
#'
#' # Use odd specifications, here offset the legend with half its height from the bottom.
#' reposition_legend(d + theme(legend.position='bottom'), x=0.3, y=0, just=c(0, -0.5))
#'
#' # For using with facets (panel name may vary depending on ggplot2 version):
#' try(reposition_legend(d + facet_grid(.~cut), 'top left', panel = 'panel-5-1'))
#' try(reposition_legend(d + facet_grid(.~cut), 'top left', panel = 'panel-1-5'))
reposition_legend <- function(aplot,
                             position=NULL,
                             legend=NULL,
                             panel='panel',
                             x=NULL,
                             y=NULL,
                             just=NULL,
                             name='guide-box',
                             clip='on',
                             offset=c(0,0),
                             z=Inf,
                             plot=TRUE) {

  # Work out positioning
  if (!is.null(position)) {
    if (length(offset) == 1) offset <- offset[c(1,1)]
    if (!grid::is.unit(offset)) offset <- grid::unit(offset, 'npc')

    position <- match.arg(position, c('bottom right','right bottom','bottom','bottom left','left bottom','left','top left','top','top right','left top','right top','right','center'))
    just <- switch(position,
      'bottom right' = c(x=1, y=0),
      'right bottom' = c(x=1, y=0),
      'bottom' = c(x=0.5, y=0),
      'bottom left' = c(x=0, y=0),
      'left bottom' = c(x=0, y=0),
      'left' = c(x=0, y=0.5),
      'top left' = c(x=0, y=1),
      'left top' = c(x=0, y=1),
      'top' = c(x=0.5, y=1),
      'top right' = c(x=1, y=1),
      'right top' = c(x=1, y=1),
      'right' = c(x=1, y=0.5),
      'center' = c(x=0.5, y=0.5)
    )
    if (is.null(x)) x = unit(just[1], 'npc') + offset[1] *
      ifelse(grepl('right', position), -1, ifelse(grepl('left', position), 1, 0))
    if (is.null(y)) y = unit(just[2], 'npc') + offset[2] *
      ifelse(grepl('top', position), -1, ifelse(grepl('bottom', position), 1, 0))
  }
  if (!is.null(x) && !grid::is.unit(x)) x <- unit(x, 'npc')
  if (!is.null(y) && !grid::is.unit(y)) y <- unit(y, 'npc')

  if (is.null(x) | is.null(y) | is.null(just)) {
    stop('Please supply either `position`, or `x`, `y`, and `just` arguments.')
  }

  # Extract legends and gtable
  if (is.null(legend) & inherits(aplot, 'ggplot'))
    legend <- g_legend(aplot)

  if (is.null(legend) || inherits(legend, 'zeroGrob'))
    stop('No legend given in arguments, or could not extract legend from plot.')

  if (!inherits(aplot, 'gtable')) {
    aplot$theme$legend.position <- 'hidden'
    aplot <- ggplotGrob(aplot)
    #aplot <- ggplot_gtable(ggplot_build(aplot + theme(legend.position='hidden')))
  }

  # Update name if already found. No idea why this is necessary.
  if (any(grepl(name, aplot$layout$name))) {
    add <- gregexpr(paste0('(?!', name, '-?)([[:digit:]]+)'), aplot$layout$name, perl = TRUE)
    add <- regmatches(aplot$layout$name, add)
    add <- as.integer(unlist(add))
    if (length(add) > 0) {
      add <- max(add) + 1
    } else {
      add <- 1
    }
    name <- paste(name, add, sep='-')
  }

  legend$vp <- viewport(x=x, y=y, just=just,  width=sum(legend$widths), height=sum(legend$heights))
  legend$name <- name

  # Place legend *under* the lowest axis-line, if z is Inf
  if (is.infinite(z)) {
    axes <- grepl('axis', aplot$layout$name)
    .z <- min(aplot$layout$z[axes]) - 1
  } else {
    .z <- z
  }
  .clip <- clip

  if (is.character(panel)) {
    pn <- which(aplot$layout$name %in% panel)
    if (length(pn) == 0) stop('Could not find panel named `',panel,'`.')

    aplot <- with(aplot$layout[pn,],
                  gtable_add_grob(aplot,
                                  legend,
                                  t = min(t),
                                  l = min(l),
                                  r = max(r),
                                  b = max(b),
                                  z = .z,
                                  clip = .clip,
                                  name = legend$name
                                  ))
  } else if ((is.numeric(panel) | is.integer(panel)) & length(panel) %in% c(2,4)) {
    panel <- rep(as.integer(panel), length.out=4)
    aplot <- gtable_add_grob(aplot,
                             legend,
                             t = panel[1],
                             l = panel[2],
                             b = panel[3],
                             r = panel[4],
                             z = .z,
                             clip = .clip,
                             name = legend$name
                             )
  }

  if (plot) {
    grid.newpage()
    grid.draw(aplot)
  }

  invisible(aplot)
}
