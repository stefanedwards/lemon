library(ggplot2)

x <- rnorm(25)
df <- data.frame(x=x, y=x+rnorm(25, sd=0.2), 
                 a=sample(c('horse','goat'), 25, replace=TRUE), 
                 stringsAsFactors = FALSE)
df$y <- with(df, ifelse(y > 1 & a=='horse', 1, y))
(p <- ggplot(df, aes(x=x, y=y, colour=a)) + geom_point(shape=1))

p + geom_siderange(start=19)

# Capping the sideranges with different symbols:
p + geom_siderange(start=19, end=22, fill='black', sides='b') + geom_siderange(sides='tl')

# It also works with facets

p <- ggplot(mpg, aes(displ, hwy, colour=fl)) +
  geom_point() +
  facet_wrap(~class, nrow = 4)

p + geom_siderange()

