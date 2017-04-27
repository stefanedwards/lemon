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
#' @param a.gplot ggplot2 object
#' @return gtable (grob) object. Draw with \code{\link[grid]{grid.draw}}.
#' @export
#' @author Stack Overflow
#' @import ggplot2
#' @examples
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) +
#'  geom_point(aes(colour = clarity)))
#'  
#' legend <- g_legend(d)
#' grid.newpage()
#' grid.draw(legend)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
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
#' @examples
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
                                       position = c("bottom", "right"),
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
   "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)),
   "right" = arrangeGrob(do.call(arrangeGrob, gl),
     legend,
     ncol = 2,
     widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  if (plot) {
    grid.newpage()
    grid.draw(combined)
  }
  
  # return gtable invisibly
  invisible(combined)
}


