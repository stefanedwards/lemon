library(testthat)
library(ggplot2)
library(grid)
library(lemon)

expect_ggplot <- function(object, class='ggplot', ...) expect_s3_class(object, class=class, ...)

test_that('geom_pointpath works', {
  p <- ggplot(mtcars, aes(wt, mpg))
  expect_ggplot(p + geom_point())
  
  p + geom_point(position=position_jitter(0.3, 0.3)) + geom_line(position=position_jitter(0.3, 0.3))
  
  expect_ggplot(
    expect_warning(
      p + geom_pointpath(),
      "soft-deprecated", fixed=TRUE)
  )
  
  expect_warning(
    p + geom_pointpath(position=position_jitter(width=0.7, height=0.7)),
    "soft-deprecated", fixed=TRUE)
  expect_warning(
    p + geom_pointpath(aes(colour=as.factor(cyl))),
    "soft-deprecated", fixed=TRUE)
  expect_warning(
    p + geom_pointpath(aes(colour=cyl)),
    "soft-deprecated", fixed=TRUE)
  expect_warning(
    p + geom_pointpath(aes(alpha=cyl)),
    "soft-deprecated", fixed=TRUE)
  
  expect_warning(
    p + geom_pointpath(aes(colour=as.factor(cyl), size=drat), position=position_jitter(width=0.2, height=0.2)),
    "soft-deprecated", fixed=TRUE)
  expect_warning(
    p + geom_pointpath(aes(size=drat), distance=unit(0, 'pt')),
    "soft-deprecated", fixed=TRUE)
  
  expect_warning(
    p + geom_pointpath(arrow=arrow()),
    "soft-deprecated", fixed=TRUE)
})

test_that('geom_pointline works', {
  p <- ggplot(mtcars, aes(wt, mpg))
  expect_ggplot(p + geom_point() + geom_line())

  expect_warning(
  #expect_ggplot(
    p + geom_pointline(),
  #  )
    "soft-deprecated", fixed=TRUE)
  
  # Jitter
  p + geom_point(position=position_jitter(0.2, 0.2)) + geom_line(position=position_jitter(0.2, 0.2))
  expect_warning(
    p + geom_pointline(position=position_jitter(width=0.7, height=0.7)),
	"soft-deprecated", fixed=TRUE)
  
  expect_warning(
    p + geom_pointline(aes(colour=as.factor(cyl))),
	"soft-deprecated", fixed=TRUE)
	
  expect_warning(
    p + geom_pointline(aes(colour=as.factor(cyl), size=drat), position=position_jitter(width=0.2, height=0.2)),
	"soft-deprecated", fixed=TRUE)
  expect_warning(
    p + geom_pointline(aes(size=drat), distance=unit(-3, 'pt')),
	"soft-deprecated", fixed=TRUE)
})