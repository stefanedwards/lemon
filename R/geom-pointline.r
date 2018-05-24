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
#' 
#' The \strong{tweak} parameter can be used to fine adjust the slope of a line.
#' If two points are very close horizontally or vertically, something happens
#' and the line may appear to be pointing in the wrong direction 
#' (e.g. upwards when the point indicates it should be downwards). I have no
#' explanation for this, other than it could be a rounding error.
#' The size of this weaking parameter depends on the physical size of the output.
#' Good values could be \code{c(0.02, 0.02)} for very small graphs,
#' \code{c(0.02, 0.06)} for a low, but wide graph, or \code{c(0.06, 0.06)} for
#' a larger graph.
#' 
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
#'   or \code{\link[ggpot2]{aes_}}.
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
#' @param arraow Arrow specification, as created by \code{\link[grid]{arrow}}.
#' @param na.rm If \code{FALSE} (default), missing values are removed with a warning.
#'   If \code{TRUE}, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#'   \code{NA} (default), includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetic, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the 
#'   default plot specification, e.g. \code{\link[ggplot2]{border}}.
#' @param linesize Width of of line.
#' @param distance Gap size between point and end of lines;
#'   use \code{\link[grid]{unit}}. Is converted to 'pt' if given as simple numeric.
#' @param linecolour,linecolor When not \code{waiver()}, the line is drawn with 
#'   this colour instead of that set by aesthetic \code{colour}.
#' @param tweak Numeric vector for x or y tweaking parameter.
#'   When \code{NULL} and \code{NA}, \code{tweak = c(0,0).}
#' 
#' @example inst/examples/geom-pointline-ex.r
#' @export
geom_pointpath <- function(mapping = NULL, data = NULL, stat = "identity",
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
                      tweak = c(0.02, 0.03), 
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
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      linesize = linesize,
      linecolour = linecolour,
      arrow = arrow,
      tweak = tweak,
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
                        distance = grid::unit(3, 'pt'), linesize = 0.5,
                        linecolour = waiver(),
                        arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 1,
                        tweak = c(0.02, 0.03) 
                        ) {
    # Test input parameters
    if (is.null(distance) || is.na(distance)) 
      distance=grid::unit(0, 'pt')
    if (!is.unit(distance) && is.numeric(distance)) 
      distance <- grid::unit(distance, 'pt')
    
    if (is.null(tweak))
      tweak = c(0.0,0.0)
    if (any(is.na(tweak)))
      tweak[is.na(tweak)] = 0
    if (length(tweak) == 1)
      tweak = c(tweak,tweak)

    # Contents of GeomPoint$draw_panel in geom-point.r
    coords <- coord$transform(data, panel_params)
    coords_p <- coords
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
    # gr_points <- GeomPoint$draw_panel(data, panel_params, coord, na.rm)
    
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
    #start <- c(TRUE, group_diff)
    group_diff <- munched$group[-1] != munched$group[-n]
    end <-   c(group_diff, TRUE)

    
    # Make df with x0,y0,x1,y1
    munched$x1 <- c(munched$x[-1], NA)
    munched$y1 <- c(munched$y[-1], NA)
    #munched$start <- start
    munched$end <- end
    
    gr_debug <- grid::segmentsGrob(
      x0=munched$x[!end], y0=munched$y[!end], x1=munched$x1[!end], y1=munched$y1[!end],
      arrow = arrow,
      gp = grid::gpar(col='grey', lty='solid')
    )
    
    # Calculate angle between each pair of points:
    tooshort = sqrt(sum(tweak**2))
    cat('Short:', tooshort, '\n')
    cat('Tweak:', tweak, '\n')
    if (TRUE && as.numeric(distance) != 0) {
      munched <- within(munched, {
        end = ifelse(sqrt((x-x1)**2 + (y-y1)**2) < tooshort, TRUE, end);
        length = sqrt((x-x1)**2 + (y-y1)**2);
        theta = atan2(y1-y, x1-x);
        
        #theta = ifelse(abs(y1 - y) < tweak[2], round(theta / pi * 4) * pi / 4, theta);
        # -0.2 corresponds to 12 degrees, about 1/16th of a radian
        # 1.371 = pi/2 - 0.2; 1.771 = pi/2 + 0.2
        deltay = y1 - y;
        adjust = abs(y1 - y) < tweak[2];
        if (any(adjust, na.rm=TRUE)) {
          theta = ifelse(adjust & -pi/4 <= theta & theta <= pi/4, pmax(pmin(theta, 0.2), -0.2), theta);
          theta = ifelse(adjust & pi/4 <= theta & theta <= pi*3/4, pmax(pmin(theta, 1.771), 1.371), theta);
          theta = ifelse(adjust & -pi/3*4 <= theta & theta <= -pi/4, pmax(pmin(theta, -1.771), -1.371), theta);
          theta = ifelse(adjust & theta < -pi*3/4, pmin(-3.351, theta), theta);
          theta = ifelse(adjust & theta > pi*3/4, pmax(3.351, theta), theta);
          
          colour = ifelse(adjust & -pi/4 <= theta & theta <= pi/4, 'red', colour);
          colour = ifelse(adjust & pi/4 <= theta & theta <= pi*3/4, 'blue', colour);
          colour = ifelse(adjust & -pi/3*4 <= theta & theta <= -pi/4, 'green', colour);
          colour = ifelse(adjust & theta < -pi*3/4, 'orange', colour);
          colour = ifelse(adjust & theta > pi*3/4, 'purple', colour);
          
        }

        size = grid::unit(size, 'pt');
        x = grid::unit(x, 'native') + size*cos(theta) + distance*cos(theta);
        x1 = grid::unit(x1, 'native') - size*cos(theta) - distance*cos(theta);
        y = grid::unit(y, 'native') + size*sin(theta) + distance*sin(theta);
        y1 = grid::unit(y1, 'native') - size*sin(theta) - distance*sin(theta);
      })
    }
    write.csv(munched, '/home/stefan/projects/lemon/lemon/munched.csv')
    
    gr_lines <- grid::segmentsGrob(
      x0=munched$x[!end], y0=munched$y[!end], x1=munched$x1[!end], y1=munched$y1[!end],
      arrow = arrow,
      gp = grid::gpar(
        col = ggplot2::alpha(munched$colour, munched$alpha)[!end],
        fill = ggplot2::alpha(munched$colour, munched$alpha)[!end],
        lwd = linesize * .pt,
        lty = munched$linetype[!end],
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    )
    if (!is.waive(linecolour)) gr_lines$gp$col <- linecolour
    

    
    #save(data, panel_params, coord, coords, coords_p, gr_lines, gr_points, munched,  file='tmp.Rdata')
     
    #gr_points
    grid::gList(gr_points, gr_lines , gr_debug)
  },
  
  draw_key = ggplot2::draw_key_point
  # function(data, params, size) {
  #   grid::grobTree(ggplot2::draw_key_path(data, params, size),
  #               ggplot2::draw_key_point(data, params, size)
  #   )
  # }
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
                           tweak = c(0.02, 0.03), 
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
      linesize = 0.5,
      linecolour = linecolour,
      arrow = arrow,    
      tweak = tweak,
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
#' @export
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

