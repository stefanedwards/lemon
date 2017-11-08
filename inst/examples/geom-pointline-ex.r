# geom_point examples

p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point() + geom_line()
p + geom_pointline()
p + geom_pointline(linecolour='brown')
p + geom_pointline(colour='green')

# Add aesthetic mappings
p + geom_point(aes(colour = factor(cyl)))
p + geom_pointline(aes(colour = factor(cyl)))
p + geom_pointline(aes(colour = factor(cyl)), linecolour='brown')  # wrong group and ordering...


p + geom_point(aes(shape = factor(cyl)))
p + geom_pointline(aes(shape = factor(cyl)))
p + geom_point(aes(size = qsec))
p + geom_pointline(aes(size=qsec))

# Change scales
p + geom_point(aes(colour = cyl)) + scale_colour_gradient(low = "blue")
p + geom_pointline(aes(colour = cyl)) + scale_colour_gradient(low = "blue")
p + geom_pointline(aes(colour = cyl), linecolour='black') + scale_colour_gradient(low = "blue")
p + geom_point(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)
p + geom_pointline(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)


# Set aesthetics to fixed value
ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = "red", size = 3)


# Varying alpha is useful for large datasets
d <- ggplot(diamonds, aes(carat, price))
d + geom_point(alpha = 1/10)
d + geom_point(alpha = 1/20)
d + geom_point(alpha = 1/100)


# For shapes that have a border (like 21), you can colour the inside and
# outside separately. Use the stroke aesthetic to modify the width of the
# border
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)


# You can create interesting shapes by layering multiple points of
# different sizes
p <- ggplot(mtcars, aes(mpg, wt, shape = factor(cyl)))
p + geom_point(aes(colour = factor(cyl)), size = 4) +
  geom_point(colour = "grey90", size = 1.5)
p + geom_point(colour = "black", size = 4.5) +
  geom_point(colour = "pink", size = 4) +
  geom_point(aes(shape = factor(cyl)))

# These extra layers don't usually appear in the legend, but we can
# force their inclusion
p + geom_point(colour = "black", size = 4.5, show.legend = TRUE) +
  geom_point(colour = "pink", size = 4, show.legend = TRUE) +
  geom_point(aes(shape = factor(cyl)))

# geom_point warns when missing values have been dropped from the data set
# and not plotted, you can turn this off by setting na.rm = TRUE
mtcars2 <- transform(mtcars, mpg = ifelse(runif(32) < 0.2, NA, mpg))
ggplot(mtcars2, aes(wt, mpg)) + geom_point()
ggplot(mtcars2, aes(wt, mpg)) + geom_point(na.rm = TRUE)
