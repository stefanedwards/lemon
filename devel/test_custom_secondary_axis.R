library(ggplot2)
library(lemon)

save_y_axis <- function(scale_details, axis, scale, position, theme) {
  save(scale_details, axis, scale, position, theme, file='yaxis.Rdata')
  #str(scale_details)
  ggplot2:::render_axis(scale_details, axis, scale, position, theme)
  
}



LabelAxis <- ggproto("LabelAxis", ggplot2::AxisSecondary,
  
  init = function(self, scale) {
    cat('init!\n')
    #str(scale) # an actual scale object
    ggproto_parent(AxisSecondary, self)$init(scale)
  },
  transform_range = function(self, range) {
    cat('transform_range!!\n')
    #str(range) # numeric vector of 1000 elements from lower to upper range (?)
    ggproto_parent(AxisSecondary, self)$transform_range(range)
  },
  break_info = function(self, range, scale) {
    cat('break_info! Yeah! \n')
    str(range)
    cat('and scale?\n')
    str(scale)
    ggproto_parent(AxisSecondary, self)$break_info(range, scale)
  },
  # Stuff from Scale super-class that doesn't get called on secondary axis.
  train_df = function(self, df) {
    cat('train_df called!\n')
    ggproto_parent(AxisSecondary, self)$train_df(df)
  },
  train = function(self, x) {
    cat('train called!\n')
    ggproto_parent(AxisSecondary, self)$train(x)
  },
  transform_df = function(self, df) {
    cat('transform_df called!\n')
    ggproto_parent(AxisSecondary, self)$transform_df(df)
  },
  transform = function(self, x) {
    cat('transform called!\n')
    ggproto_parent(AxisSecondary, self)$transform(x)
  },
  map_df = function(self, df, i = NULL) {
    cat('map_df called!\n')
    ggproto_parent(AxisSecondary, self)$map_df(df, i)
  }
)

label_axis <- function(breaks, labels, trans = ~., name = waiver()) {
  ggproto(NULL, LabelAxis,
          trans = ~.,
          name = name,
          breaks = breaks,
          labels = labels
  )
}

p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
p1 + coord_flex_cart(left=save_y_axis) + scale_y_continuous(sec.axis = label_axis(breaks=c(20,30), labels=c('Ja','Nej')))

#p1 + label_axis(breaks=c(20,30), labels=c('Ja','Nej'))  # doesn't work
