# geom_point examples
library(ggplot2)

p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point() + geom_line()
p + geom_pointline()
p + geom_pointline(linecolour='brown')

# Add aesthetic mappings
p + geom_pointline(aes(colour = factor(cyl)))
# Using linecolour preserved groups.
p + geom_pointline(aes(colour = factor(cyl)), linecolour='brown') 

## If you want to combine the pretty lines of pointline that do *not* respect
## grouping (or order), combine several layers with geom_point on top:
p + geom_pointline() + geom_point(aes(colour=factor(cyl)))

p + geom_point(aes(shape = factor(cyl)))
p + geom_pointline(aes(shape = factor(cyl)))
p + geom_point(aes(size = qsec))
p + geom_pointline(aes(size=qsec))

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

# geom_line() is suitable for time series
ggplot(economics, aes(date, unemploy)) + geom_pointline()
ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_pointline()


# geom_step() is useful when you want to highlight exactly when
# the y value chanes
recent <- economics[economics$date > as.Date("2013-01-01"), ]
ggplot(recent, aes(date, unemploy)) + geom_pointline()
ggplot(recent, aes(date, unemploy)) + geom_step() 
# geom_pointstep is not implemented; no idea of what to expect.

# geom_path lets you explore how two variables are related over time,
# e.g. unemployment and personal savings rate
m <- ggplot(economics, aes(unemploy/pop, psavert))
m + geom_pointpath()
m + geom_pointpath(aes(colour = as.numeric(date)))

# NAs break the line. Use na.rm = T to suppress the warning message
df <- data.frame(
  x = 1:5,
  y1 = c(1, 2, 3, 4, NA),
  y2 = c(NA, 2, 3, 4, 5),
  y3 = c(1, 2, NA, 4, 5)
)
ggplot(df, aes(x, y1)) + geom_point() + geom_line()
ggplot(df, aes(x, y1)) + geom_pointline()

ggplot(df, aes(x, y3)) + geom_point() + geom_line()
#geom_pointline does not break where NA's are missing.
ggplot(df, aes(x, y3)) + geom_pointline() 


# Setting line type vs colour/size
# Line type needs to be applied to a line as a whole, so it can
# not be used with colour or size that vary across a line
x <- seq(0.01, .99, length.out = 100)
df <- data.frame(
  x = rep(x, 2),
  y = c(qlogis(x), 2 * qlogis(x)),
  group = rep(c("a","b"),
              each = 100)
)
p <- ggplot(df, aes(x=x, y=y, group=group))
# These work
p + geom_pointline(linetype=2)
p + geom_pointline(aes(colour = group), linetype = 2)
p + geom_line(aes(colour = x))
p + geom_pointline(aes(colour = x))
