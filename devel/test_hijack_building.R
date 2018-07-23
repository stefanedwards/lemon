# test if adding a ... geom? scale?? anything ggplot2-like can change the class
# of the ggplot2 object

library(ggplot2)
library(lemon)

stranger_pointline <- function(...) {
  a <- geom_pointline(...)
  cat(class(a), '\n')
  class(a) <- c('mja', class(a))  # really no other options, but can be made safer to avoid re-adding the same extended class
  a
}




ggplot_add.mja <- function(object, plot, object_name) {
  cat('I\'m gonna screw this stuff up!\n')
  class(plot) <- c('yes',class(plot))
  ggplot2:::ggplot_add.Layer(object, plot, object_name)
}


# ggplot2/R/plot-build.r
# ggplot_build.ggplot returns `ggplot_built` object
# param plot ggplot2 object
ggplot_build.yes <- function(plot) {
  cat('Yes yes yes!!\n')
  res <- NextMethod()
  #str(res)
  cat(class(res), '\n')
  class(res) <- c('yes', class(res))
  res
}

# ggplot2/R/plot-build.r
# ggplot_gtable.ggplot_built returns a `gtable` object
# param data ggplot_built object
ggplot_gtable.yes <- function(data) {
  cat('Nah nah nah\n')
  NextMethod()
}


p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p <- ggplot(mtcars, aes(wt, mpg)) + stranger_pointline()

p
# ggplotGrob <- function(x) {
#   ggplot_gtable(ggplot_build(x))
# }
# ggplot2/R/plot.r
# print.ggplot:
#   grid.newpage()
#   data <- ggplot_build(x)
#   gtable <- ggplot_gtable(data)
#   grid.draw(gtable)


# 
nothing_ <- function(self) {
  cat('Ha ha ha ha ha ha \n')
  str(self)
}


yes <- ggproto('yes', 
  nothing = nothing_
)

# coerce into yes

# ggproto(NULL, yes, p)
# 
# 
# setAs(from="ggplot", to="yes", function(from) {
#   assign('nothing', nothing_, envir=from)
#   from
# })




# And now for something completly different -------


prependClass <- function(object, new_class) {
  if (!inherits(plot, new_class)) 
    class(object) <- c(new_class, class(object))
  object
}


as.ggplot2twist <- function(plot) {
  if (inherits(plot, 'ggplot2twist')) 
    return(plot)
  plot <- prependClass(plot, 'ggplot2twist')
  #if (!'twist' %in% names(plot))
  #  plot$twist <- list()
  plot
}

TwistedContinuousScale = ggproto('TwistedContinuousScale', ScaleContinuousPosition,
  draw = function(...) { # something that returns a grob
    grid::gList(grid::linesGrob())
  }
)

censor <- scales::censor
twisted_y_scale <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), 
                             labels = waiver(), limits = NULL, expand = waiver(), oob = censor, 
                             na.value = NA_real_, trans = "identity", position = "right", 
                             sec.axis = waiver()) 
{
  sc <- continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept", 
                           "ymin_final", "ymax_final", "lower", "middle", "upper"), 
                         "position_c", identity, name = name, breaks = breaks, 
                         minor_breaks = minor_breaks, labels = labels, limits = limits, 
                         expand = expand, oob = oob, na.value = na.value, trans = trans, 
                         guide = "none", position = position, super = TwistedContinuousScale)
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) 
      sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) 
      stop("Secondary axes must be specified using 'sec_axis()'")
    sc$secondary.axis <- sec.axis
  }
  prependClass(sc, 'twistedscale')
  sc
}

ggplot_add.twistedscale <- function(object, plot, object_name) {
  plot$scales$add(object)
  plot <- prependClass(plot, 'ggplot2twist')
  plot
}

ggplot_build.ggplot2twist <- function(plot) {
  g_built <- NextMethod() #ggplot2:::ggplot_build.ggplot(plot)
  #cat('lalal\n')
  prependClass(g_built, 'g_built_twist')
}

ggplot_gtable.g_built_twist <- function(data) {
  # data: list(data = data, layout = layout, plot = plot)
  save(data, file='gbuild.Rdata')
  gtable <- NextMethod()
  if (inherits(data$plot$scales$get_scales('y'), 'TwistedContinuousScale')) {
    cat('Inheriting...\n')
    twistedy <- data$plot$scales$get_scales('y')  ## returns a scale object (atleast without facets)
    # data$layout$panel_scales_y ## returns list, is more for facets (and free scales)
    if (nrow(data$layout$layout) == 1) {
      name <- switch(twistedy$position, left='axis-l', right='axis-r')
      cat(name, '\n')
      i <- which(gtable$layout$name == name)
      gtable$grobs[i] <- twistedy$draw()
    }
  }
  gtable
}


p <- ggplot(mtcars, aes(wt, y=mpg)) + geom_point()
(pt <- p + twisted_y_scale(name=NULL, sec.axis=dup_axis(name='mpg')))


p2 <- ggplot(mtcars, aes(wt, mpg, colour=drat)) + geom_point() + facet_grid(cyl~., scales='free_y')
(pt2 <- p2 + twisted_y_scale(name=NULL, sec.axis=dup_axis(name='mpg')))
