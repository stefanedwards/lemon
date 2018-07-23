library(ggplot2)
p <- ggplot(mtcars, aes(wt, y=mpg)) + geom_point()
p <- p + lemon_y_continuous() + geom_ytick(data=data.frame(ytick=20, label='blah'), aes(ytick=ytick, label=label))

class(p)

#(pt <- p + twisted_y_scale(name=NULL, sec.axis=dup_axis(name='mpg')))
