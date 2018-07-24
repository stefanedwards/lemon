library(ggplot2)

p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()

l <- p + annotated_y_axis('hello', y=200, tick=TRUE)
l <- l + annotated_y_axis('nah', y=300, colour='blue') +
  annotated_y_axis('leftsidey', y=300, side='left') +
  annotated_y_axis("x^2 == .(y)", y=250, parsed=TRUE, vjust=1, tick=TRUE) +
  annotated_y_axis("bar(x) ~ yadayda == .(y)", y=150, parsed=TRUE, col='green', hjust=0, x=1)
(l <- p +   annotated_y_axis("x^2 == .(y)", y=250, parsed=TRUE, vjust=0.3, tick=TRUE) )
(l <- p +   annotated_y_axis("X == .(y)", y=250, parsed=TRUE, tick=TRUE) )

l + annotated_x_axis(x=25, side='bottom', label='nada')

l
gt <- ggplot_gtable(ggplot_build(l))
data <- ggplot_build(l)
