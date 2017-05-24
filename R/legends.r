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
#' legend with \code{\link[gridExtra]{grid.arrange}} (see examples).
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


#' Share a legend between multiple plots
#'
#' Extract legend, combines plots using \code{\link[gridExtra]{arrangeGrob}},
#' and places legend in a margin.
#'
#' 
#'
#' @param ... ggplot2 objects. Their legends are automatically hidden.
#'            The legend is taken from the first argument.
#' @param ncol Integer, number of columns to arrange plots in.
#' @param nrow Integer, number of rows to arrange plots in.
#' @param position 'bottom' or 'right' for positioning legend.
#' @param plot Logical, when \code{TRUE} (default), draws combined plot on a
#'             new page.
#' @return gtable of combined plot, invisibly. 
#'   Draw  gtable object using \code{\link[grid]{grid.draw}}.
#' @author
#'   Originally brought to you by \href{http://rpubs.com/sjackman}{Shaun Jackman}
#'   (\href{http://rpubs.com/sjackman/grid_arrange_shared_legend}{original}),
#'   and further improved by \href{http://baptiste.github.io/}{Baptiste Auguié}  at
#'   \url{https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs}.
#'   Stefan McKinnon Edwards added left and top margins.
#' @import ggplot2 gridExtra grid
#' @export
#' @seealso \code{\link{g_legend}}, \code{\link{reposition_legend}}
#' @examples
#' library(ggplot2)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' p1 <- qplot(carat, price, data = dsamp, colour = clarity)
#' p2 <- qplot(cut, price, data = dsamp, colour = clarity)
#' p3 <- qplot(color, price, data = dsamp, colour = clarity)
#' p4 <- qplot(depth, price, data = dsamp, colour = clarity)
#' grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 4, nrow = 1)
#' grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2)
grid_arrange_shared_legend <- function(...,
                                       ncol = length(list(...)),
                                       nrow = 1,
                                       position = c("bottom", "right","top","left"),
                                       plot=TRUE)
  {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
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
#' Panel name is by default \code{panel}, but when using facets it typically 
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
#'                 Overrules \code{x}, \code{y}, and \code{just} arguments.
#' @param legend The legend to place, if \code{NULL} (default),
#'               it is extracted from \code{aplot} if
#'               this is a ggplot2 object.
#' @param panel Name of panel in gtable. See description.
#' @param x horisontal coordinate of legend, with 0 at left.
#' @param y vertical coordiante of legend, with 0 at bottom.
#' @param just 'Anchor point' of legend; it is this point of the legend that is
#'             placed at the \code{x} and \code{y} coordinates.
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
#' # For using with facets:
#' reposition_legend(d + facet_grid(.~cut), 'top left', panel = 'panel-3-1')
reposition_legend <- function(aplot,
                             position=NULL,
                             legend=NULL,
                             panel='panel',
                             x=NULL,
                             y=NULL,
                             just=NULL,
                             name='guide-box',
                             clip='on',
                             z=Inf,
                             plot=TRUE) {

  # Work out positioning
  if (!is.null(position)) {
    position <- match.arg(position, c('bottom right','bottom','bottom left','left','top left','top','top right','right','center'))
    just <- switch(position,
      'bottom right' = c(x=1, y=0),
      'bottom' = c(x=0.5, y=0),
      'bottom left' = c(x=0, y=0),
      'left' = c(x=0, y=0.5),
      'top left' = c(x=0, y=1),
      'top' = c(x=0.5, y=1),
      'top right' = c(x=1, y=1),
      'right' = c(x=1, y=0.5),
      'center' = c(x=0.5, y=0.5)
    )
    x = unit(just[1], 'npc')
    y = unit(just[2], 'npc')
  }
  if (is.null(x) | is.null(y) | is.null(just)) {
    stop('Please supply either `position`, or `x`, `y`, and `just` arguments.')
  }

  # Extract legends and gtable
  if (is.null(legend) & inherits(aplot, 'ggplot'))
    legend <- g_legend(aplot)

  if (is.null(legend) || inherits(legend, 'zeroGrob'))
    stop('No legend given in arguments, or could not extract legend from plot.')

  if (!inherits(aplot, 'gtable'))
    aplot <- ggplot_gtable(ggplot_build(aplot + theme(legend.position='hidden')))

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
  .z <- z
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
