library(testthat)
library(vdiffr)
#library(lemon)

expect_ggplot <- function(object, class='ggplot', ...) expect_s3_class(object, class=class, ...)

my.theme <- theme_light() +
    theme(panel.border=element_blank(),
          axis.line = element_line(),
          axis.ticks = element_line(colour='black'))

dat1 <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30),
  cl = sample.int(3, 30, replace=TRUE),
  cl2 = sample(c('a','b','c'), 30, replace=TRUE)
)

test_that('Brackets do not produce errors when theme(axis.ticks = element_blank()).', {

  p <- ggplot(dat1, aes(gp, y)) +
    geom_point(position=position_jitter(width=0.2, height=0)) +
    my.theme

  expect_ggplot(p)

  expect_ggplot(
    p + coord_capped_cart(bottom=brackets_horizontal()) +
      theme(axis.ticks = element_blank())
  )

  expect_ggplot(
    p + coord_capped_cart(bottom=brackets_horizontal()) +
      theme(axis.ticks.x = element_blank())
  )

  expect_ggplot(
    p + coord_capped_cart(bottom=brackets_horizontal()) +
      theme(axis.ticks.y = element_blank())
  )

  expect_ggplot(
    p + coord_capped_cart(bottom=brackets_horizontal(length=0))
  )

  expect_ggplot(
    p + coord_capped_cart(bottom=brackets_horizontal(length=0)) +
      theme(axis.ticks = element_blank())
  )

  expect_ggplot(
    p + coord_capped_cart(left=brackets_vertical()) +
      theme(axis.ticks = element_blank())
  )

  expect_ggplot(
    p + coord_capped_cart(left=brackets_vertical()) +
      theme(axis.ticks.x = element_blank())
  )

  expect_ggplot(
    p + coord_capped_cart(left=brackets_vertical()) +
      theme(axis.ticks.y = element_blank())
  )

  expect_ggplot(
    p + coord_capped_cart(left=brackets_vertical(length=0))
  )

  expect_ggplot(
    p + coord_capped_cart(left=brackets_vertical(length=0)) +
      theme(axis.ticks = element_blank())
  )
})


test_that('The visual object is still correct', {
  data(mpg, package='ggplot2')
  p <- ggplot(mpg, aes(cyl, hwy, colour=class)) +
    geom_point() +
    scale_x_continuous(breaks=c(4,5,6,8), sec.axis=dup_axis()) +
    scale_y_continuous(sec.axis=dup_axis()) +
    coord_flex_cart(bottom=brackets_horizontal(),
                    top=brackets_horizontal(direction='down'),
                    left=brackets_vertical(),
                    right=brackets_vertical(direction='right')) +
    my.theme
    expect_doppelganger("brackets on all four sides", p)
})
