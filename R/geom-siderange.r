#' @include ggplot2.r
#' @include lemon_print.r
NULL

#' Display range of data in side of plot
#' 
#' Projects data onto horizontal or vertical edge of panels.
#' 
#' The \code{geom_siderange} projects the data displayed in the panel onto the
#' sides, using the same aesthetics. It has the added capability of potting a
#' symbol  at either end
#' of the line, and lines are offset from the edge and each other.
#' 
#' To display a symbol, specify an integer for either \code{start} or \code{end}.
#' See the list for \code{pch} in \code{\link[graphics]{points}} for values to use.
#' The argumetns \code{start} and \code{end} also accepts a list object with
#' named entries \code{pch}, \code{alpha}, \code{stroke}, and \code{fill}, which 
#' correspond to the usual aesthetics, as well as a special named entry, 
#' \code{sizer} (note the extra 'r').
#' This last entry is a multiplier for enlarging the symbol relative to the 
#' linewidth, as the aesthetic \code{size} affects both linewidth and symbol size.
#' 
#' The distance between the panel's edge and sideranges are specified by 
#' the argument \code{distance}. If a symbol is specified, the linewidth is
#' further expanded to cover the width of the symbol (including \code{sizer}).
#' 
#' 
#' @inheritParams geom_pointpath
#' @param sides Character including \strong{t}op, \strong{r}ight, \strong{b}ottom, and/or \strong{l}eft,
#'   indicating which side to project data onto.
#' @param distance Distance between edge of panel and lines, and distance 
#'   between lines, in multiples of line widths, see description.
#' @param start,end Adds a symbol to either end of the siderange. 
#'   \code{start} corresponds to minimal value, \code{end} to maximal value.
#' @section Aesthetics:
#' The geom understands the following aesthetics (required are in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{y}
#'   \item alpha
#'   \item colour
#'   \item fill (if a symbol is applied with \code{start} or \code{end}
#'   \item group
#'   \item linetype
#'   \item size
#'   \item stroke
#' }
#'  
#'
#'   
#' @seealso \code{\link[ggplot2]{geom_rug}}
#' @export
#' 
#' @example inst/examples/geom-siderange-ex.r
geom_siderange <- function(mapping = NULL, data = NULL, 
                           stat = "identity", position = "identity",
                           ...,
                           distance = 3, 
                           arrow = NULL,
                           lineend = "butt",
                           sides = 'bl',
                           start = NA,
                           end = NA,
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
      start = start,
      end = end,
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
                                  
  draw_panel = function(data, panel_params, coord, arrow = NULL, distance = 1,
                        start = NA, end = NA,
                        lineend = "butt", sides='bl', na.rm = FALSE) {

    data <- coord$transform(data, panel_params)
    save(data, coord, file='tmp.Rdata')

    default_point = list(pch = 19, sizer = 3, alpha = NA, stroke = 0.5, fill = NA)
    if (!is.list(start) && !is.na(start)) {
      start <- merge.list(list(pch=start), default_point)
    } else if (is.list(start)) {
      start <- merge.list(start, default_point)
    
    }
    if (!is.list(end) && !is.na(end)) {
      end <- merge.list(list(pch=end), default_point)
    } else if (is.list(end)) {
      end <- merge.list(end, default_point)
    }
    
    sizer <- c(NA, NA)
    if (is.list(start)) sizer[1] <- start$sizer
    if (is.list(end)) sizer[2] <- start$sizer
    if (!all(is.na(sizer))) distance <- distance * max(sizer, na.rm=TRUE)
        
    #  from ggplot2/R/geom-rug.r
    sideranges <- list()
    if (!is.null(data$x) & grepl("[tb]", sides)) {
      df <- plyr::ddply(data, plyr::.(PANEL, colour, group, size, linetype, alpha, stroke, fill),
                        function(x) {c(max=max(x$x), min=min(x$x))})
      gp <- grid::gpar(col=alpha(df$colour, df$alpha), lty=df$linetype, lwd=df$size * .pt)
      if (grepl("t", sides)) {
        df <- plyr::ddply(df, plyr::.(PANEL), function(x) {
          x$y <- grid::unit(1, 'npc') -  unit(df$size, 'pt') * order(x$group) * distance; 
          x
        })
        sideranges$x_t <- list(grid::segmentsGrob(x0=df$min, x1=df$max, y0=df$y, y1=df$y, gp=gp, arrow=arrow), NULL, NULL)
        if (is.list(start)) {
          df_s <- merge.list(df, start)
          df_s$min <- with(df_s, unit(min, 'native') - unit(sizer * size * .pt / 2, 'pt'))
          sideranges$x_t[[2]] <- grid::pointsGrob(x=df_s$min, y=df_s$y, pch=df_s$pch, 
                                                  gp = grid::gpar(
                                                    col = alpha(df_s$colour, df_s$alpha),
                                                    fill = alpha(df_s$fill, df_s$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = df_s$sizer * df_s$size * .pt + df_s$stroke * .stroke / 2,
                                                    lwd = df_s$stroke * .stroke / 2
                                                  )
          )
        }
        if (is.list(end)) {
          df_e <- merge.list(df, end)
          df_e$max <- with(df_e, unit(max, 'native') + unit(sizer * size * .pt / 2, 'pt'))
          sideranges$x_t[[3]] <- grid::pointsGrob(x=df_e$max, y=df_e$y, pch=df_e$pch, 
                                                  gp = grid::gpar(
                                                    col = alpha(df_e$colour, df_e$alpha),
                                                    fill = alpha(df_e$fill, df_e$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = df_e$sizer * df_e$size * .pt + df_e$stroke * .stroke / 2,
                                                    lwd = df_e$stroke * .stroke / 2
                                                  )
          )
        }
        sideranges$x_t <- grid::gTree(children=do.call(grid::gList, sideranges$x_t))
      }
      if (grepl("b", sides)) {
        df <- plyr::ddply(df, plyr::.(PANEL), function(x) {
          x$y <- unit(df$size, 'pt') * order(x$group) * distance; 
          x
          })
        sideranges$x_b <- list(grid::segmentsGrob(x0=df$min, x1=df$max, y0=df$y, y1=df$y, gp=gp, arrow=arrow), NULL, NULL)
        if (is.list(start)) {
          df_s <- merge.list(df, start)
          df_s$min <- with(df_s, unit(min, 'native') - unit(sizer * size * .pt / 2, 'pt'))
          sideranges$x_b[[2]] <- grid::pointsGrob(x=df_s$min, y=df_s$y, pch=df_s$pch, 
                                                  gp = grid::gpar(
                                                    col = alpha(df_s$colour, df_s$alpha),
                                                    fill = alpha(df_s$fill, df_s$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = df_s$sizer * df_s$size * .pt + df_s$stroke * .stroke / 2,
                                                    lwd = df_s$stroke * .stroke / 2
                                                  )
          )
        }
        if (is.list(end)) {
          df_e <- merge.list(df, end)
          df_e$max <- with(df_e, unit(max, 'native') + unit(sizer * size * .pt / 2, 'pt'))
          sideranges$x_b[[3]] <- grid::pointsGrob(x=df_e$max, y=df_e$y, pch=df_e$pch, 
                                                  gp = grid::gpar(
                                                    col = alpha(df_e$colour, df_e$alpha),
                                                    fill = alpha(df_e$fill, df_e$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = df_e$sizer * df_e$size * .pt + df_e$stroke * .stroke / 2,
                                                    lwd = df_e$stroke * .stroke / 2
                                                  )
          )
        }
        sideranges$x_b <- grid::gTree(children=do.call(grid::gList, sideranges$x_b))
      }
    }
    if (!is.null(data$y) & grepl("[lr]", sides)) {
      df <- plyr::ddply(data, plyr::.(PANEL, colour, group, size, linetype, alpha),
                       function(x) c(max=max(x$y), min=min(x$y)))
      gp <- grid::gpar(col=alpha(df$colour, df$alpha), lty=df$linetype, lwd=df$size * .pt)
      if (grepl("l", sides)) {
        df <- plyr::ddply(df, plyr::.(PANEL), function(x) {
          x$x <- unit(df$size, 'pt') * order(x$group) * distance;
          x
        })
        sideranges$y_l <- list(grid::segmentsGrob(x0=df$x, x1=df$x, y0=df$min, y1=df$max,  gp=gp, arrow=arrow), NULL, NULL)
        if (is.list(start)) {
          df_s <- merge.list(df, start)
          df_s$min <- with(df_s, unit(min, 'native') - unit(sizer * size * .pt / 2, 'pt'))
          sideranges$y_l[[2]] <- grid::pointsGrob(x=df_s$x, y=df_s$min, pch=df_s$pch, 
                                                  gp = grid::gpar(
                                                    col = alpha(df_s$colour, df_s$alpha),
                                                    fill = alpha(df_s$fill, df_s$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = df_s$sizer * df_s$size * .pt + df_s$stroke * .stroke / 2,
                                                    lwd = df_s$stroke * .stroke / 2
                                                  )
          )
        }
        if (is.list(end)) {
          df_e <- merge.list(df, end)
          df_e$max <- with(df_e, unit(max, 'native') + unit(sizer * size * .pt / 2, 'pt'))
          sideranges$y_l[[3]] <- grid::pointsGrob(x=df_e$x, y=df_e$max, pch=df_e$pch, 
                                                  gp = grid::gpar(
                                                    col = alpha(df_e$colour, df_e$alpha),
                                                    fill = alpha(df_e$fill, df_e$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = df_e$sizer * df_e$size * .pt + df_e$stroke * .stroke / 2,
                                                    lwd = df_e$stroke * .stroke / 2
                                                  )
          )
        }
        sideranges$y_l <- grid::gTree(children=do.call(grid::gList, sideranges$y_l))
      }
      if (grepl("r", sides)) {
        df <- plyr::ddply(df, plyr::.(PANEL), function(x) {
          x$x <- unit(1, 'npc') - unit(df$size, 'pt') * order(x$group) * distance;
          x
        })
        sideranges$y_r <- list(grid::segmentsGrob(x0=df$x, x1=df$x, y0=df$min, y1=df$max, gp=gp, arrow=arrow), NULL, NULL)
        if (is.list(start)) {
          df_s <- merge.list(df, start)
          df_s$min <- with(df_s, unit(min, 'native') - unit(sizer * size * .pt / 2, 'pt'))
          print(df_s)
          sideranges$y_r[[2]] <- grid::pointsGrob(x=df_s$x, y=df_s$min, pch=df_s$pch, 
                                                  gp = grid::gpar(
                                                    col = alpha(df_s$colour, df_s$alpha),
                                                    fill = alpha(df_s$fill, df_s$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = df_s$sizer * df_s$size * .pt + df_s$stroke * .stroke / 2,
                                                    lwd = df_s$stroke * .stroke / 2
                                                  )
          )
        }
        if (is.list(end)) {
          df_e <- merge.list(df, end)
          df_e$max <- with(df_e, unit(max, 'native') + unit(sizer * size * .pt / 2, 'pt'))
          sideranges$y_r[[3]] <- grid::pointsGrob(x=df_e$x, y=df_e$max, pch=df_e$pch, 
                                                  gp = grid::gpar(
                                                    col = alpha(df_e$colour, df_e$alpha),
                                                    fill = alpha(df_e$fill, df_e$alpha),
                                                    # Stroke is added around the outside of the point
                                                    fontsize = df_e$sizer * df_e$size * .pt + df_e$stroke * .stroke / 2,
                                                    lwd = df_e$stroke * .stroke / 2
                                                  )
          )
        }
        sideranges$y_r <- grid::gTree(children=do.call(grid::gList, sideranges$y_r))
      }
    }
    save(data, coord, sideranges, file='tmp.Rdata')
    
    grid::gTree(children = do.call(grid::gList, sideranges))
  },
  
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA, stroke=0.5, fill=NA),
  default_point = list(size = 1.5, alpha = NA, stroke = 0.5, stroke=0.5, fill=NA),
    
  draw_key = draw_key_path
)