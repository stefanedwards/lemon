library(ggplot2)
library(lemon)

twisted_y <- function(y, label, ...) {
  
  layer(
    data = data.frame(twist=y, label=label),
    mapping = aes(x=NULL, yintercept=y, twist=y, label=label),
    stat = 'identity',
    geom = TwistedAes,
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      ...
    )
  )
}

TwistedAes = ggplot2::ggproto('TwistedAes',
  `_inherit`=ggplot2::GeomHline,
  required_aes = c("yintercept", "twist", "label"),
  non_missing_aes = c(),
  default_aes = aes(
    shape = 19, colour = "black", size = 0.5, fill = NA,
    alpha = NA, stroke = 0.5,
    linetype = 2
  )
)



TwistedScale = ggplot2::ggproto('TwistedScale', ScaleDiscretePosition,

  # Stuff from Scale super-class that doesn't get called on secondary axis.
  train_df = function(self, df) {
    cat('train_df called!\n')
    str(self)
    ggproto_parent(ScaleDiscretePosition, self)$train_df(df)
  },
  train = function(self, x) {
    cat('train called!\n')
    ggproto_parent(ScaleDiscretePosition, self)$train(x)
  },
  transform_df = function(self, df) {
    cat('transform_df called!\n')
    str(df)
    ggproto_parent(ScaleDiscretePosition, self)$transform_df(df)
  },
  transform = function(self, x) {
    cat('transform called!\n')
    str(x)
    ggproto_parent(ScaleDiscretePosition, self)$transform(x)
  },
  map_df = function(self, df, i = NULL) {
    cat('map_df called!\n')
    ggproto_parent(ScaleDiscretePosition, self)$map_df(df, i)
  }  
)

twisted_y_scale <- function(..., position = "right") {
  sc <- discrete_scale(c("twist"), "position_d", identity, ...,
                       expand = c(0,0), guide = "none", position = position, super = TwistedScale) #super = ScaleDiscretePosition)
  sc$range_c <- ggplot2:::continuous_range()
  sc
}

(p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + twisted_y(y=4, label='hello'))
(p2 <- p + twisted_y_scale())

ggplot_build(p + twisted_y(y=6, label='more'))$data
ggplot_build(p2 + twisted_y(y=6, label='more'))$layout$get_scales(1)

gb <- ggplot_build(p2 + twisted_y(y=6, label='more'))


p3 <- ggplot(data.frame(x='a', y=3), aes(x=x, y=y)) + geom_point()
p3b <- p3 + geom_point(data=data.frame(x='b', y=3.5))



## Again again, Monday mornings.... ----


TwistedScale = ggplot2::ggproto('TwistedScale', ScaleDiscretePosition,
                                
    # Stuff from Scale super-class that doesn't get called on secondary axis.
    train_df = function(self, df) {
      cat('train_df called!\n')
      str(self)
      ggproto_parent(ScaleDiscretePosition, self)$train_df(df)
    },
    train = function(self, x) {
      cat('train called!\n')
      ggproto_parent(ScaleDiscretePosition, self)$train(x)
    },
    transform_df = function(self, df) {
      cat('transform_df called!\n')
      str(df)
      ggproto_parent(ScaleDiscretePosition, self)$transform_df(df)
    },
    transform = function(self, x) {
      cat('transform called!\n')
      str(x)
      ggproto_parent(ScaleDiscretePosition, self)$transform(x)
    },
    map_df = function(self, df, i = NULL) {
      cat('map_df called!\n')
      ggproto_parent(ScaleDiscretePosition, self)$map_df(df, i)
    }  
)

twisted_y_scale <- function(..., position = "right") {
  sc <- discrete_scale(c("twist"), "position_d", identity, ...,
                       expand = c(0,0), guide = "none", position = position, super = TwistedScale) #super = ScaleDiscretePosition)
  sc$range_c <- ggplot2:::continuous_range()
  sc
}
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(col=cyl)) #+ twisted_y(y=4, label='hello'))
(p2 <- p + twisted_y_scale())



save_y_axis <- function(scale_details, axis, scale, position, theme) {
  #save(scale_details, axis, scale, position, theme, file='yaxis.Rdata')
  str(scale_details)
  ggplot2:::render_axis(scale_details, axis, scale, position, theme)
  
}
save_y_axis2 <- function(scale_details, axis, scale, position, theme, data) {
  #save(scale_details, axis, scale, position, theme, file='yaxis.Rdata')
  cat('Calling save_y_axis2 with data!\n')
  str(data)
  ggplot2:::render_axis(scale_details, axis, scale, position, theme)
  
}


p + coord_flex_cart2(left=save_y_axis)
