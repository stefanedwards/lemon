# geom_point examples
library(ggplot2)

p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point() + geom_line()
p + geom_pointline()

p + geom_pointline(linecolour='brown')

p + geom_pointpath()

# Add aesthetic mappings
p + geom_pointline(aes(colour = factor(cyl)))
# Using linecolour preserved groups.
p + geom_pointline(aes(colour = factor(cyl)), linecolour='brown') 

## If you want to combine the pretty lines of pointline that do *not* respect
## grouping (or order), combine several layers with geom_point on top:
p + geom_pointline() + geom_point(aes(colour=factor(cyl)))

# Change scales
p + geom_pointline(aes(colour = cyl)) + scale_colour_gradient(low = "blue")
p + geom_pointline(aes(colour = cyl), linecolour='black') + scale_colour_gradient(low = "blue")
p + geom_pointline(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)

# For shapes that have a border (like 21), you can colour the inside and
# outside separately. Use the stroke aesthetic to modify the width of the
# border
ggplot(mtcars, aes(wt, mpg)) +
  geom_pointline(shape = 21, colour = "black", fill = "white", 
                 size = 5, stroke = 5, distance = unit(10, 'pt'))

## Another example
df <- data.frame(x=rep(c('orange','apple','pear'), each=3), b=rep(c('red','green','purple'), times=3), y=runif(9))
ggplot(df, aes(x=x, y=y, colour=b, group=b)) + geom_pointline(linesize=1, size=2, distance=6) + theme_bw()

# geom_pointline() is suitable for time series
ggplot(economics, aes(date, unemploy)) + geom_pointline()
ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_pointline()
