library(testthat)
library(lemon)

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
    p + coord_capped_cart(bottom=brackets_horisontal()) +
      theme(axis.ticks = element_blank())
  )
  
  expect_ggplot(
    p + coord_capped_cart(bottom=brackets_horisontal()) +
      theme(axis.ticks.x = element_blank())
  )
  
  expect_ggplot(
    p + coord_capped_cart(bottom=brackets_horisontal()) +
      theme(axis.ticks.y = element_blank())
  )
  
  expect_ggplot(
    p + coord_capped_cart(bottom=brackets_horisontal(length=0))
  )
  
  expect_ggplot(
    p + coord_capped_cart(bottom=brackets_horisontal(length=0)) +
      theme(axis.ticks = element_blank())
  )
    #p + coord_capped_cart(bottom=brackets_vertical()) +
    #  theme(axis.ticks = element_blank())
    #Error in unit.c(grobHeight(axis_h$top), unit(aspect_ratio, "null"), grobHeight(axis_h$bottom)) : 
    #  it is invalid to combine 'unit' objects with other types
  
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
