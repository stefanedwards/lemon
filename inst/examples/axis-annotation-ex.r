library(ggplot2)

p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()

l <- p + annotated_y_axis('mark at', y=200, tick=TRUE)
l

(l + annotated_x_axis('| good economy ->', x=25, print_value=FALSE, hjust=0, tick=TRUE))

l + annotated_y_axis("x^2 == .(y)", y=150, parsed=FALSE,  tick=FALSE) +
    annotated_y_axis("x^2 + bar(x) == .(y)", y=mean(mtcars$hp), parsed=TRUE, tick=TRUE)

l + annotated_y_axis("bar(x) == .(y)", y = mean(mtcars$hp),  parsed=TRUE, tick=FALSE) 
# use double equal signs, or the output becomes '=(...)' for some reason.

l + annotated_y_axis('this is midway', y=sum(range(mtcars$hp))/2, print_value = FALSE, side='left')

# work around if an axis only contains parsed expressions
p + annotated_y_axis("bar(x) == .(y)", y = mean(mtcars$hp),  parsed=TRUE, tick=FALSE) +
  annotated_y_axis("some long string", y=100, tick=FALSE, print_value=FALSE, colour=NA)
