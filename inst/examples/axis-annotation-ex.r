library(ggplot2)

p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()

l <- p + annotated_y_axis('hello', y=200)
l <- l + annotated_y_axis('nah', y=300, colour='blue') +
  annotated_y_axis('leftsidey', y=300, side='left') +
  annotated_y_axis("x^2 == .(y)", y=250, parsed=TRUE, vjust=1) +
  annotated_y_axis("bar(x) ~ yadayda == .(y)", y=150, parsed=TRUE, col='green', hjust=0, x=1)
l
gt <- ggplot_gtable(ggplot_build(l))
data <- ggplot_build(l)
