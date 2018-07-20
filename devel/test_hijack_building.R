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
