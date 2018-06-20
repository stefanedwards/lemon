#' @include ggplot2.r
NULL

#' Connected points
#' 
#' \code{geom_pointpath} combines \code{\link[ggplot2]{geom_point}} and 
#' \code{\link[ggplot2]{geom_path}}, such that a) when jittering is used,
#' both lines and points stay connected, and b) provides a visual effect
#' by adding a small gap between the point and the end of line.
#' \code{geom_pointline} combines \code{\link[ggplot2]{geom_point}} and
#' \code{\link[ggplot2]{geom_line}}.
#' 
#' \code{geom_pointpath} connects the observations in the same order in which
#' they appear in the data.
#' \code{geom_pointline} connects them in order of the variable on the x-axis.
#' 
#' Both \code{geom_pointpath} and \code{geom_pointline} will only 
#' connect observations within the same group! However,
#' if \code{linecolour} is \emph{not} \code{waiver()}, connections
#' will be made between groups, but possible in an incorrect order.
#' \strong{Note!} If the x-variable is a factor, the `group' aesthetic will
#' \emph{also} use this, thus lines between x-values are not grouped.
#' Use \code{aes(group=...)} to enforce a group across x-values.
#' 
#' @section Aesthetics:
#' \code{geom_pointline} and \code{geom_pointpath} understands the following 
#' aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{y}
#'   \item alpha
#'   \item colour -- sets colour of point. Only affects line if \code{linecolour=waiver()}. 
#'   \item stroke
#'   \item shape
#'   \item stroke
#'   \item group
#'   \item linetype
#'   \item size -- only affects point size. Width of line is set with 
#'         \code{linesize} and cannot be linked to an aesthetic.
#' }
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#'   or \code{\link[ggplot2]{aes_}}.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer, 
#'   as a string.
#' @param position Position adjustment, either as a string, or the result of a 
#'   call to a position adjustment function
#'   (e.g. \code{\link[ggplot2]{position_jitter}}).
#'   Both lines and points gets the same adjustment 
#'   (\emph{this} is where the function excels over \code{geom_point() + geom_line()}).
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mintre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param arrow Arrow specification, as created by \code{\link[grid]{arrow}}.
#' @param na.rm If \code{FALSE} (default), missing values are removed with a warning.
#'   If \code{TRUE}, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#'   \code{NA} (default), includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetic, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the 
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param linesize Width of of line.
#' @param distance Gap size between point and end of lines when the distance
#'   between points exceeds \code{threshold};
#'   use \code{\link[grid]{unit}}. Is converted to 'pt' if given as simple numeric.
#' @param threshold,shorten Use \code{threshold} to adjust when the line between
#'   two points should be drawn with a gap (per argument above) or shortened by
#'   the proportion given by \code{shorten}.  
#' @param linecolour,linecolor When not \code{waiver()}, the line is drawn with 
#'   this colour instead of that set by aesthetic \code{colour}.
#' 
#' @example inst/examples/geom-pointline-ex.r
#' @export
geom_pointpath <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE, 
                      distance = unit(3, 'pt'), 
                      shorten = 0.5,
                      threshold = 0.1,
                      lineend = "butt",
                      linejoin = "round",
                      linemitre = 1,
                      linesize = 0.5,
                      linecolour = waiver(),
                      linecolor = waiver(),
                      arrow = NULL,
                      ...) {
  if (is.waive(linecolour) && !is.waive(linecolor)) linecolour <- linecolor
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      distance = distance,
      shorten = shorten,
      threshold = threshold,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      linesize = linesize,
      linecolour = linecolour,
      arrow = arrow,
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
GeomPointPath <- ggplot2::ggproto('GeomPointPath',
                                  `_inherit`=ggplot2::GeomPoint,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5,
    linetype = 1
  ),
  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE,
                        distance = grid::unit(3, 'pt'), 
                        shorten = 0.5,
                        threshold = 0.1,
                        linesize = 0.5,
                        linecolour = waiver(),
                        arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 1
                        ) {
    # Test input parameters
    if (is.null(distance) || is.na(distance)) 
      distance=grid::unit(0, 'pt')
    if (!grid::is.unit(distance) && is.numeric(distance)) 
      distance <- grid::unit(distance, 'pt')
    
    # Contents of GeomPoint$draw_panel in geom-point.r
    coords <- coord$transform(data, panel_params)
    saveRDS(coords, 'tmp.rds')
    gr_points <- ggplot2:::ggname("geom_point",
           grid::pointsGrob(
             coords$x, coords$y,
             pch = coords$shape,
             gp = grid::gpar(
               col = alpha(coords$colour, coords$alpha),
               fill = alpha(coords$fill, coords$alpha),
               # Stroke is added around the outside of the point
               fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
               lwd = coords$stroke * .stroke / 2
             )
           )
    )
    
    # Contents of GeomPath$draw_panel in geom-path
    if (!anyDuplicated(data$group)) {
      message_wrap("geom_path: Each group consists of only one observation. ",
                   "Do you need to adjust the group aesthetic?")
    }

    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- ggplot2::coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())

    # Work out whether we should use lines or segments
    attr <- plyr::ddply(munched, "group", function(df) {
      linetype <- unique(df$linetype)
      data.frame(
        solid = identical(linetype, 1) || identical(linetype, "solid"),
        constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      stop("geom_path: If you are using dotted or dashed lines",
           ", colour, size and linetype must be constant over the line",
           call. = FALSE)
    }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    munched$end <-   c(group_diff, TRUE)

    munched <- within(munched, {
      x1 = c(x[-1], NA);
      y1 = c(y[-1], NA);
      deltax = x1 - x;
      deltay = y1 - y;
      length <- sqrt(deltax**2 + deltay**2);
    })
    
    if (any(munched$length > threshold)) {
      # Calculate angle between each pair of points and move endpoints:
      if (as.numeric(distance) != 0) {
        df <- within(munched[munched$length > threshold,], {
          theta = atan2(deltay, deltax);
          size = grid::unit(size, 'pt');
          x = grid::unit(x, 'native') + size*cos(theta) + distance*cos(theta);
          x1 = grid::unit(x1, 'native') - size*cos(theta) - distance*cos(theta);
          y = grid::unit(y, 'native') + size*sin(theta) + distance*sin(theta);
          y1 = grid::unit(y1, 'native') - size*sin(theta) - distance*sin(theta);
        })
      }
      
      gr_distant <- with(df, grid::segmentsGrob(
        x0=x[!end], y0=y[!end], x1=x1[!end], y1=y1[!end],
        arrow = arrow,
        gp = grid::gpar(
          col = 'green', #ggplot2::alpha(colour, alpha)[!end],
          fill = ggplot2::alpha(colour, alpha)[!end],
          lwd = linesize * .pt,
          lty = linetype[!end],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      ))
      if (!is.waive(linecolour)) 
        gr_distant$gp$col <- linecolour
    }
    
    if (any(munched$length <= threshold)) {
      df <- within(munched[munched$length <= threshold,], {
        x  = x + shorten/2 * deltax; x = grid::unit(x, 'native');
        y  = y + shorten/2 * deltay; y = grid::unit(y, 'native');
        x1 = x1 - shorten/2 * deltax; x1 = grid::unit(x1, 'native');
        y1 = y1 - shorten/2 * deltay; y1 = grid::unit(y1, 'native');
      })
      
      gr_short <- with(df, grid::segmentsGrob(
        x0=x[!end], y0=y[!end], x1=x1[!end], y1=y1[!end],
        arrow = arrow,
        gp = grid::gpar(
          col = 'red', #ggplot2::alpha(colour, alpha)[!end],
          fill = ggplot2::alpha(colour, alpha)[!end],
          lwd = linesize * .pt,
          lty = linetype[!end],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      ))
      if (!is.waive(linecolour)) 
        gr_short$gp$col <- linecolour
    }
    
    grid::gList(gr_points, gr_distant, gr_short)
  },
  
  draw_key = ggplot2::draw_key_point
)


#' @export
#' @inheritParams geom_pointpath
#' @rdname geom_pointpath
geom_pointline <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, 
                           distance = unit(3, 'pt'), 
                           lineend = "butt",
                           linejoin = "round",
                           linemitre = 1,
                           linesize = 0.5,
                           linecolour = waiver(),
                           linecolor = waiver(),
                           arrow = NULL,
                           ...) {
  
  if (is.waive(linecolour) && !is.waive(linecolor)) linecolour <- linecolor
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      distance = distance,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      linesize = linesize,
      linecolour = linecolour,
      arrow = arrow,    
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
GeomPointLine <- ggproto("GeomPointLine", GeomPointPath,
    setup_data = function(data, params) {
      data[order(data$PANEL, data$group, data$x), ]
    }
)



#' @inheritParams geom_pointpath
#' @rdname geom_pointpath
geom_pointrangeline <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, 
                                distance = unit(3, 'pt'), 
                                lineend = "butt",
                                linejoin = "round",
                                linemitre = 1,
                                linesize = 0.5,
                                linecolour = waiver(),
                                linecolor = waiver(),
                                arrow = NULL,
                                ...) {
  stop('geom_pointrangeline has not been implemented. Sorry')
  if (is.waive(linecolour) && !is.waive(linecolor)) linecolour <- linecolor
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointRangeLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      distance = distance,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      linesize = 0.5,
      linecolour = linecolour,
      arrow = arrow,      
      ...
    )
  )
}

#' @rdname lemon-ggproto
#' @keywords internal
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @import gtable
GeomPointRangeLine <- ggproto("GeomPointRangeLine", GeomPointLine,
  required_aes = c("x", "y", "ymin", "ymax"),
  
  setup_data = function(data, params) {
    data[order(data$PANEL, data$group, data$x), ]
  },
  draw_panel = function(data, panel_params, coord, na.rm = FALSE,
                        distance = grid::unit(3, 'pt'), linesize = 0.5,
                        linecolour = waiver(),
                        arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 1
  ) {
    data <- transform(data, xend = x, y = ymin, yend = ymax)
    grid::gList(ggname("geom_linerange", GeomSegment$draw_panel(data, panel_params, coord)),
                GeomPointLine$draw_panel(data, panel_params, coord,
                                         na.rm=na.rm, distance=distance,
                                         linesize=linesize, linecolour=linecolour,
                                         arrow=arrow, lineend=lineend,
                                         linejoin=linejoin, linemitre=linemitre)
    )
  }
)
