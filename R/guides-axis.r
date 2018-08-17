#' From R/guide-axes.r
#' @keywords internal
#' @rdname ggplot2-non-exports
# @param position of ticks
# @param labels at ticks
# @param position of axis (top, bottom, left or right)
# @param range of data values
#' @import grid
#' @import gtable
guide_axis <- function(at, labels, position = "right", theme, label_element, gp_df) {
  if (length(at) == 0)
    return(zeroGrob())
  
  at <- grid::unit(at, "native")
  position <- match.arg(position, c("top", "bottom", "right", "left"))

  zero <- grid::unit(0, "npc")
  one <- grid::unit(1, "npc")

  label_x <- switch(position,
    top = ,
    bottom = at,
    right = theme$axis.ticks.length,
    left = one - theme$axis.ticks.length
  )
  label_y <- switch(position,
    top = theme$axis.ticks.length,
    bottom = one - theme$axis.ticks.length,
    right = ,
    left = at
  )

  if (is.list(labels)) {
    if (any(sapply(labels, is.language))) {
      labels <- do.call(expression, labels)
    } else {
      labels <- unlist(labels)
    }
  }

  if (missing(gp_df)) {
    label_render <- switch(position,
       top = "axis.text.x.top", bottom = "axis.text.x",
       left = "axis.text.y", right = "axis.text.y.right"
    )
    
    labels <- switch(position, 
      top = ,
      bottom = element_render(theme, label_render, labels, x = label_x, expand_y = TRUE),
      right = ,
      left =  element_render(theme, label_render, labels, y = label_y, expand_x = TRUE)
    )
    gp_df <- list(tickcolour = NULL)
  } else {
    labels <- switch(position,
      top = ,
      bottom = element_grob(label_element, label = labels, x = label_x, margin_y = TRUE,
                            family=gp_df$family, face=gp_df$face, colour=gp_df$colour, size=gp_df$size, 
                            hjust=gp_df$hjust, vjust=gp_df$vjust, angle=gp_df$angle),
      right = ,
      left = element_grob(label_element, label = labels, y = label_y, margin_x = TRUE,
                          family=gp_df$family, face=gp_df$face, colour=gp_df$colour, size=gp_df$size, 
                          hjust=gp_df$hjust, vjust=gp_df$vjust, angle=gp_df$angle)
    )
  }
  
  line <- switch(position,
    top =    element_render(theme, "axis.line.x", c(0, 1), c(0, 0), id.lengths = 2),
    bottom = element_render(theme, "axis.line.x", c(0, 1), c(1, 1), id.lengths = 2),
    right =  element_render(theme, "axis.line.y", c(0, 0), c(0, 1), id.lengths = 2),
    left =   element_render(theme, "axis.line.y", c(1, 1), c(0, 1), id.lengths = 2)
  )

  nticks <- length(at)

  ticks <- switch(position,
    top = element_render(theme, "axis.ticks.x",
      x          = rep(at, each = 2),
      y          = rep(unit.c(zero, theme$axis.ticks.length), nticks),
      id.lengths = rep(2, nticks),
      colour = gp_df$tickcolour),
    bottom = element_render(theme, "axis.ticks.x",
      x          = rep(at, each = 2),
      y          = rep(unit.c(one - theme$axis.ticks.length, one), nticks),
      id.lengths = rep(2, nticks),
      colour = gp_df$tickcolour),
    right = element_render(theme, "axis.ticks.y",
      x          = rep(unit.c(zero, theme$axis.ticks.length), nticks),
      y          = rep(at, each = 2),
      id.lengths = rep(2, nticks),
      colour = gp_df$tickcolour),
    left = element_render(theme, "axis.ticks.y",
      x          = rep(unit.c(one - theme$axis.ticks.length, one), nticks),
      y          = rep(at, each = 2),
      id.lengths = rep(2, nticks),
      colour = gp_df$tickcolour)
  )

  # Create the gtable for the ticks + labels
  gt <- switch(position,
    top    = gtable::gtable_col("axis",
      grobs   = list(labels, ticks),
      width   = one,
      heights = grid::unit.c(grobHeight(labels), theme$axis.ticks.length)
    ),
    bottom = gtable::gtable_col("axis",
      grobs   = list(ticks, labels),
      width   = one,
      heights = grid::unit.c(theme$axis.ticks.length, grobHeight(labels))
    ),
    right  = gtable::gtable_row("axis",
      grobs   = list(ticks, labels),
      widths  = grid::unit.c(theme$axis.ticks.length, grobWidth(labels)),
      height  = one
    ),
    left   = gtable::gtable_row("axis",
      grobs   = list(labels, ticks),
      widths  = grid::unit.c(grobWidth(labels), theme$axis.ticks.length),
      height  = one
    )
  )

  # Viewport for justifying the axis grob
  justvp <- switch(position,
    top    = grid::viewport(y = 0, just = "bottom", height = gtable::gtable_height(gt)),
    bottom = grid::viewport(y = 1, just = "top",    height = gtable::gtable_height(gt)),
    right  = grid::viewport(x = 0, just = "left",   width  = gtable::gtable_width(gt)),
    left   = grid::viewport(x = 1, just = "right",  width  = gtable::gtable_width(gt))
  )
  
  absoluteGrob(
    grid::gList(line, gt),
    width = gtable::gtable_width(gt),
    height = gtable::gtable_height(gt),
    vp = justvp
  )
}
