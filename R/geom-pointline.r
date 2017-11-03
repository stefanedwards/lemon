#' @include ggplot2.r
NULL

#' @export
geom_pointline <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE, 
                      distance = unit(6, 'pt'), 
                      lineend = "butt",
                      linejoin = "round",
                      linemitre = 1,
                      arrow = NULL,
                      ...) {
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
GeomPointLine <- ggplot2::ggproto('GeomPointLine',
                                  `_inherit`=ggplot2::GeomPoint,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5,
    linetype = 1
  ),
                                  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE,
                        distance = unit(6, 'pt'),
                        arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 1
                        ) {
    # Contents of GeomPoint$draw_panel in geom-point.r
    coords <- coord$transform(data, panel_params)
    coords_p <- coords
    gr_points <- ggname("geom_point",
           grid::pointsGrob(
             coords$x, coords$y,
             pch = coords$shape,
             gp = gpar(
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
    munched <- coord_munch(coord, data, panel_params)

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
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)

    if (!constant) {
      gr_lines <- segmentsGrob(
        munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
        default.units = "native", arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[!end],
          fill = alpha(munched$colour, munched$alpha)[!end],
          lwd = munched$size[!end] * .pt,
          lty = munched$linetype[!end],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
    } else {
      id <- match(munched$group, unique(munched$group))
      gr_lines <- polylineGrob(
        munched$x, munched$y, id = id,
        default.units = "native", arrow = arrow,
        gp = gpar(
          col = alpha(munched$colour, munched$alpha)[start],
          fill = alpha(munched$colour, munched$alpha)[start],
          lwd = munched$size[start] * .pt,
          lty = munched$linetype[start],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
    }
    #  gr_lines <- GeomPath$draw_panel(data, panel_params, coord0, arrow, lineend, linejoin, linemitre, na.rm)
    # 
    # 
    # gr_lines$gp$col <- 'blue'
    
    gr_lines$gp$lwd <- 2.0
    
    
    # Make df with x0,y0,x1,y1
    munched$x1 <- c(munched$x[-1], NA)
    munched$y1 <- c(munched$y[-1], NA)
    munched$start <- start
    munched$end <- end
    
    # Calculate angle between each pair of points:
    munched <- within(munched, {
      xn = x;
      yn = y;
      theta = atan2(y1-y, x1-x);
      x = unit(x, 'native') + distance*cos(theta);
      x1 = unit(x1, 'native') - distance*cos(theta);
      y = unit(y, 'native') + distance*sin(theta);
      y1 = unit(y1, 'native') - distance*sin(theta)
    })
    
    gr_tmp <- segmentsGrob(
      x0=munched$x[!end], y0=munched$y[!end], x1=munched$x1[!end], y1=munched$y1[!end],
      arrow = arrow,
      gp = gpar(
        col = alpha(munched$colour, munched$alpha)[!end],
        fill = alpha(munched$colour, munched$alpha)[!end],
        lwd = munched$size[!end] * .pt,
        lty = munched$linetype[!end],
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    )
    
    save(data, panel_params, coord, coords, coords_p, gr_lines, gr_points, gr_tmp, munched,  file='tmp.Rdata')
     
    #gr_points
    grid::gList(gr_points, gr_tmp)
  }
)