library(ggplot2)

p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()

l <- p + annotated_y_axis('mark at', y=200, tick=TRUE)
l

(l + annotated_x_axis('| good economy ->', x=25, print_value=FALSE, hjust=0, tick=TRUE))

l + annotated_y_axis("x^2 == .(y)", y=150, parsed=FALSE,  tick=FALSE) +
    annotated_y_axis("x^2 + bar(x) == .(y)", y=mean(mtcars$hp), parsed=TRUE, tick=TRUE)

p + annotated_y_axis("bar(x) == .(y)", y = mean(mtcars$hp),  parsed=TRUE, tick=FALSE)
# use double equal signs, or the output becomes '=(...)' for some reason.

l + annotated_y_axis('this is midway', y=sum(range(mtcars$hp))/2, print_value = FALSE, side='left')


l <- l + annotated_y_axis('nah', y=300,  tick=TRUE) +
l + annotated_y_axis('leftsidey', y=300, side='left') 
  annotated_y_axis("x^2 == .(y)", y=250, parsed=TRUE, vjust=1, tick=TRUE) +
  annotated_y_axis("bar(x) ~ yadayda == .(y)", y=150, parsed=TRUE, col='green', hjust=0, x=1)
(l <- p +   annotated_y_axis("x^2 == .(y)", y=250, parsed=TRUE, vjust=0.3, tick=TRUE) )
(l <- p +   annotated_y_axis("X == .(y)", y=250, parsed=TRUE, tick=TRUE) )

l + annotated_x_axis(x=25, side='bottom', label='nada')

p + annotated_y_axis('leftsidey', y=300, side='left') 
