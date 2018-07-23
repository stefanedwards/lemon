library(ggplot2)

p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()

l <- p + annotated_y_axis('hello', y=200)
l <- l + annotated_y_axis('nah', y=300, colour='blue') +
  annotated_y_axis('leftsidey', y=300, side='left') +
  annotated_y_axis("x^2 == .(y)", y=300, parsed=TRUE) 
data <- ggplot_build(l)
