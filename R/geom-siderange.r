#' @include ggplot2.r
NULL

#' Display range of data in side of plot
#' 
#' @section Aesthetics:
#' These geoms are drawn using with \code{\link{geom_line}} so support the
#' same aesthetics: \code{alpha}, \code{colour}, \code{linetype} and
#' \code{size}. They also each have aesthetics that control the position of
#' the line:
#'
#' @inheritParams geom_pointpath
#' @param xintercept,yintercept,slope,intercept Parameters that control the
#'   position of the line. If these are set, \code{data}, \code{mapping} and
#'   \code{show.legend} are overridden
#' @param distance Distance between edge of panel and lines, and distance 
#'   between lines, in multiples of line widths.
#' @export
#' 
#' @example inst/examples/geom-siderange-ex.r
#' @export
#' TODO: Put points at the end to indicate open or closed ranges?
#' Consider `arrow`, but this will only provide arrows, not use of arbitrary (?) points.
#' For end-points, use start for smallest (bottomest/leftest) values, and end for top-most.
geom_siderange <- function(mapping = NULL, data = NULL, 
                           stat = "identity", position = "identity",
                           ...,
                           distance = 3, 
                           arrow = NULL,
                           lineend = "butt",
                           sides = 'bl',
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSideRange,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      distance = distance,
      lineend = lineend,
      sides = sides,
      na.rm = na.rm,
      ...
    )
  )  
}

#' @rdname lemon-ggproto
#' @keywords internal
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import gtable
#' @import grid
#' @import plyr
GeomSideRange <- ggplot2::ggproto('GeomSideRange',
                                  `_inherit`=ggplot2::Geom,
                                  required_aes = c("x", "y"),
                                  non_missing_aes = c("size", "linetype", "colour"),
                                  default_aes = aes(
                                    shape = 19, colour = "black", size = 1.5, fill=NA,
                                    alpha = NA, stroke = 0.5,
                                    linetype = 1
                                  ),
                                  
  draw_panel = function(data, panel_scales, coord, arrow = NULL, distance = 1,
                        lineend = "butt", sides='bl', na.rm = FALSE) {

    data <- coord$transform(data, panel_scales)
    save(data, coord, file='tmp.Rdata')
    
    #  from ggplot2/R/geom-rug.r
    sideranges <- list()
    if (!is.null(data$x) & grepl("[tb]", sides)) {
      df <- plyr::ddply(data, .(PANEL, colour, group, size, linetype, alpha),
                        function(x) {c(max=max(x$x), min=min(x$x))})
      gp <- grid::gpar(col=alpha(df$colour, df$alpha), lty=df$linetype, lwd=df$size * .pt)
      if (grepl("t", sides)) {
        df <- plyr::ddply(df, .(PANEL), function(x) {
          x$y <- grid::unit(1, 'npc') -  unit(df$size, 'pt') * order(x$group) * distance; 
          x
        })
        sideranges$x_t <- grid::segmentsGrob(x0=df$min, x1=df$max, y0=df$y, y1=df$y, gp=gp)
      }
      if (grepl("b", sides)) {
        df <- plyr::ddply(df, .(PANEL), function(x) {
          x$y <- unit(df$size, 'pt') * order(x$group) * distance; 
          x
          })
        sideranges$x_b <- grid::segmentsGrob(x0=df$min, x1=df$max, y0=df$y, y1=df$y, gp=gp)
      }
    }
    if (!is.null(data$y) & grepl("[lr]", sides)) {
      df <- plyr::ddply(data, .(PANEL, colour, group, size, linetype, alpha),
                       function(x) c(max=max(x$y), min=min(x$y)))
      gp <- grid::gpar(col=alpha(df$colour, df$alpha), lty=df$linetype, lwd=df$size * .pt)
      if (grepl("l", sides)) {
        df <- plyr::ddply(df, .(PANEL), function(x) {
          x$x <- unit(df$size, 'pt') * order(x$group) * distance;
          x
        })
        sideranges$y_l <- grid::segmentsGrob(x0=df$x, x1=df$x, y0=df$min, y1=df$max,  gp=gp)
      }
      if (grepl("r", sides)) {
        df <- plyr::ddply(df, .(PANEL), function(x) {
          x$x <- unit(1, 'npc') - unit(df$size, 'pt') * order(x$group) * distance;
          x
        })
        sideranges$y_r <- grid::segmentsGrob(x0=df$x, x1=df$x, y0=df$min, y1=df$max, gp=gp)
      }
    }
    grid::gTree(children = do.call(grid::gList, sideranges))
  },
  
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  
  draw_key = draw_key_path
)