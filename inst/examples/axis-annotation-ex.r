library(ggplot2)

p <- ggplot(mtcars, aes(mpg, hp, colour=disp)) + geom_point()

l <- p + annotate_y_axis('mark at', y=200, tick=TRUE)
l

(l + annotate_x_axis('| good economy ->', x=25, print_value=FALSE, hjust=0, tick=TRUE))

l + annotate_y_axis("x^2 == .(y)", y=150, parsed=FALSE,  tick=FALSE) +
    annotate_y_axis("x^2 + bar(x) == .(y)", y=mean(mtcars$hp), parsed=TRUE, tick=TRUE)

l + annotate_y_axis("bar(x) == .(y)", y = mean(mtcars$hp),  parsed=TRUE, tick=FALSE) 
# use double equal signs, or the output becomes '=(...)' for some reason.

l + annotate_y_axis('this is midway', y=sum(range(mtcars$hp))/2, print_value = FALSE, side='left')

# work around if an axis only contains parsed expressions
p + annotate_y_axis("bar(x) == .(y)", y = mean(mtcars$hp),  parsed=TRUE, tick=FALSE) +
  annotate_y_axis("some long string", y=100, tick=FALSE, print_value=FALSE, colour=NA)


# Works together with other functions
p <- p + theme_light() + theme(panel.border=element_blank(),
                               axis.line = element_line(), 
                               axis.ticks = element_line(colour='black')) 
p + coord_capped_cart(bottom='right') + 
  annotate_y_axis('More than I\ncan afford', y=125, 
                  print_value=FALSE, tick=TRUE)
