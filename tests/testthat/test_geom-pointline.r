library(testthat)
library(ggplot2)
library(lemon)

expect_ggplot <- function(object, class='ggplot', ...) expect_s3_class(object, class=class, ...)

test_that('geom_pointline works', {
  p <- ggplot(mtcars[order(mtcars$wt), ], aes(wt, mpg))
  expect_ggplot(p + geom_point())
  
  #expect_ggplot(
    p + geom_pointline()
  #  )
  
  p + geom_pointline(position=position_jitter(width=0.7, height=0.7))
  
  p + geom_pointline(aes(colour=as.factor(cyl)))
  
  
  p + geom_pointline(aes(colour=as.factor(cyl)), position=position_jitter(width=0.7, height=0.7))
})