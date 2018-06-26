library(ggplot2)

p <- ggplot(mpg, aes(reorder(manufacturer, displ), cty)) +
  geom_point()


#' @export
# From R/plot-construction.r
ggplot_add.Scale <- function(object, plot, object_name) {
  plot$scales$add(object)
  plot
}


# R/scale-discrete-.r
scale_x_discreate2 <- function(..., expand = waiver(), position = "bottom") {
  sc <- ggplot2:::discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", identity, ...,
                       expand = expand, guide = "none", position = position, super = ScaleDiscretePosition2)
  sc$range_c <- ggplot2:::continuous_range()
  sc
}

ScaleDiscretePosition2 <- ggproto("ScaleDiscretePosition2", ScaleDiscretePosition)

ggplot_add.ScaleDiscretePosition2 <- function(object, plot, object_name) {
  plot$scales$add(object)
  cat('I AM THE HOLY KTITEN!\n')
  print(class(plot))
  class(plot) <- c('tablex',class(plot))
  plot
}

# R/plot-build.r 
# ggplot_gtable <- function(data, ...) {
#   UseMethod('ggplot_gtable')
# }

# ggplot_gtable.tablex <- function(data) {
#   class(data) <- setdiff(class(data), 'tablex')
#   cat('I am plotting tablex')
#   plot_table <- ggplot_gtable(data)
#   plot_table
#   
# }
  
ggplotGrob <- function(x) {
  UseMethod('ggplotGrob')
}

ggplotGrob.tablex <- function(x) {
  gtable <- ggplot_gtable(ggplot_build(x))
  cat('I am plotting!\n')
  gtable
}

ggplotGrob.ggplot <- function(x) {
  ggplot2::ggplotGrob(x)
}

ggplotGrob(p + scale_x_discreate2('XX'))

