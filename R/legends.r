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
#' @param a.gplot ggplot2 or gtable object.
#' @return gtable (grob) object. Draw with \code{\link[grid]{grid.draw}}.
#' @export
#' @author Stack Overflow
#' @import ggplot2
#' @seealso \code{\link{grid_arrange_shared_legend}}, \code{\link{reposition_legend}}
#' @examples
#' library(ggplot2)
#' library(grid)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) +
#'  geom_point(aes(colour = clarity)))
#'
#' legend <- g_legend(d)
#' grid.newpage()
#' grid.draw(legend)
g_legend<-function(a.gplot){
  if (!is.gtable(a.gplot))
    a.gplot <- ggplotGrob(a.gplot)
  gtable_filter(a.gplot, 'guide-box', fixed=TRUE)
}


#' Share a legend between multiple plots
#'
#' Extract legend, combines plots using \code{\link[gridExtra]{arrangeGrob}},
#' and places legend underneath.
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
#' @source
#'   Originally brought to you by \href{http://rpubs.com/sjackman}{Shaun Jackman}
#'   (\href{http://rpubs.com/sjackman/grid_arrange_shared_legend}{original}),
#'   and further improved by \code{baptiste} at
#'   \url{https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs}
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
   "top" = arrangeGrob(do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(lheight, unit(1, "npc") - lheight)),
   "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)),
   "left" = arrangeGrob(do.call(arrangeGrob, gl),
      legend,
      ncol = 2,
      widths = unit.c(lwidth, unit(1, "npc") - lwidth)),
   "right" = arrangeGrob(do.call(arrangeGrob, gl),
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



#' Reposition a legend onto a panel
#'
#' Repositions a legend onto a panel, by either taking it from the same ggplot,
#' or by using another. Works on both ggplot2 and gtable objects, and can accept
#' any grob as legend.
#'
#' To modify the look of the legend, use themes and the natural ggplot functions
#' found in \code{\link[ggplot2]{guide_legend}}.
#'
#' Panel name is \code{panel}, but when using facets, it typically takes the
#' form \code{panel-{col}-{row}}. Use
#' \preformatted{
#' ggplot_gtable(ggplot_build(aplot))
#' }
#' to build a \code{\link[gtable]{gtable}} object, and print it to look at the
#' names.
#'
#' @param aplot a ggplot2 or gtable object.
#' @param position Where to place the legend in the panel.
#'                 Overrules \code{x}, \code{y}, and \code{just} arguments.
#' @param legend The legend to place, if \code{NULL} (default),
#'               it is extracted from \code{aplot} if
#'               this is a ggplot2 object.
#' @param panel Name of panel in gtable.
#' @param x horisontal coordinate of legend, with 0 at left.
#' @param y vertical coordiante of legend, with 0 at bottom.
#' @param just 'Anchor point' of legend; it is this point of the legend that is
#'             placed at the \code{x} and \code{y} coordinates.
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
  if (is.null(x) & is.null(y) & is.null(just)) {
    stop('Either supply `position` or `x`, `y`, and `just` arguments.')
  }

  # Extract legends and gtable
  if (is.null(legend) & inherits(aplot, 'ggplot'))
    legend <- g_legend(aplot)

  if (is.null(legend) || inherits(legend, 'zeroGrob'))
    stop('No legend given in arguments, or could not extract legend from plot.')

  if (!inherits(aplot, 'gtable'))
    aplot <- ggplot_gtable(ggplot_build(aplot + theme(legend.position='hidden')))

  legend$vp <- viewport(x=x, y=y, just=just,  width=sum(legend$widths), height=sum(legend$heights))

  pn <- which(aplot$layout$name == panel)
  if (length(pn) == 0) stop('Could not find panel named `',panel,'`.')
  if (inherits(aplot$grobs[[pn]], 'zeroGrob')) {
    aplot$grobs[[pn]] <- legend
  } else {
    aplot$grobs[[pn]]$vp <- viewport(x=unit(0, 'npc'), y=unit(0, 'npc'), just=c(0,0), name=panel)
    aplot$grobs[[pn]]$children <- gList(aplot$grobs[[pn]]$children, `guide-box`=legend)
    aplot$grobs[[pn]]$childrenOrder <- append(aplot$grobs[[pn]]$childrenOrder, 'guide-box')
  }

  if (plot) {
    grid.newpage()
    grid.draw(aplot)
  }

  invisible(aplot)
}
