library(ggplot2)

p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()

l <- p + annotated_y_axis('hello', y=200)
l <- l + annotated_y_axis('nah', y=300, colour='blue')

