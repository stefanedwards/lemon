require(testthat)
require(ggplot2)
require(lemon)

expect_lemon_plot <- function(object) expect_s3_class(object, 'lemon_plot')

test_that('ggplot2 does not break lemon_plot by altering class', {
  p <- ggplot(data.frame(a=1:2, b=runif(2), c=letters[1:2]), aes(a, b, colour=c)) + geom_blank()
  
  l <- as.lemon_plot(p)
  expect_lemon_plot(l)
  expect_error(l + 1, regexp = "Don't know how to add 1 to a plot", fixed=TRUE)
  expect_lemon_plot(l + NULL)
  expect_lemon_plot(l %+% data.frame(a=3, b=0, c='a'))
  expect_lemon_plot(l + theme_bw())
  expect_lemon_plot(l + scale_y_continuous())
  expect_lemon_plot(l + labs(title='Lemons!'))
  expect_lemon_plot(l + guides(colour='legend'))
  # ggplot_add.uneval ???
  expect_lemon_plot(l + coord_flip())
  expect_lemon_plot(l + facet_wrap(~c))
  expect_lemon_plot(l + list(data=data.frame(a=3, b=0, c='a'), scale_x_continuous()))
  expect_lemon_plot(l + geom_point())
  
})