library(testthat)
library(vdiffr)

context('Facets with repeated axes; auxillary functions')

all.sides <- c('top', 'right','bottom','left') # for quicker referencing

test_that('`reduce.ticks.labels.settings` returns correct value on single logicals', {
  r <- lemon:::reduce.ticks.labels.settings ### I'm lazy

  expect_equal(r(FALSE), character(0))
  expect_equal(r(TRUE), all.sides)
})

test_that('`reduce.ticks.labels.settings` returns correct value on scalars', {
  r <- lemon:::reduce.ticks.labels.settings ### I'm lazy

  for (s in all.sides) {
    expect_equal(r(s), s, info=paste0('side was ', s) )
  }
})

test_that('`reduce.ticks.labels.settings` returns correct values on single composites', {
  r <- lemon:::reduce.ticks.labels.settings ### I'm lazy

  expect_equal(r('none'), character(0))
  expect_equal(r('all'), all.sides)
  expect_equal(r('x'), all.sides[c(1,3)])
  expect_equal(r('y'), all.sides[c(2,4)])
})

test_that('`reduce.ticks.labels.settings` returns correct values on composites + side', {
  r <- lemon:::reduce.ticks.labels.settings ### I'm lazy

  for (s in all.sides) {
    expect_equal(r(c('none',s)), character(0), info=paste0('side was ', s) )
  }
  for (s in all.sides) {
    expect_equal(r(c('all',s)), all.sides, info=paste0('side was ', s) )
  }
  expect_equal(r(c('x','left')), all.sides[c(4,1,3)])
  expect_equal(r(c('x','top')), all.sides[c(1,1,3)])
  expect_equal(r(c('y','top')), all.sides[c(1,2,4)])
  expect_equal(r(c('y','top')), all.sides[c(1,2,4)])
  expect_equal(r(c('right','y')), all.sides[c(2,2,4)])
})

test_that('`reduce.ticks.labels.settings` fails on unrecognized input', {
  r <- lemon:::reduce.ticks.labels.settings ### I'm lazy

  expect_error(r('cnter'))
})


test_that('Still works with vdiffr', {
  data(mpg, package='ggplot2')
  p <- ggplot(mpg, aes(displ, cty)) + geom_point() +
    coord_capped_cart(bottom='both', left='both') +
    theme_bw() + theme(panel.border=element_blank(), axis.line=element_line())
  expect_doppelganger("facet_rep_grid", 
    expect_warning(
      p + facet_rep_grid(drv ~ cyl, repeat.tick.labels = TRUE),
    "soft-deprecated", fixed=TRUE))
  expect_doppelganger("facet_rep_wrap", 
    expect_warning(
      p + facet_rep_wrap(~ cyl, repeat.tick.labels = TRUE, ncol=3),
    "soft-deprecated", fixed=TRUE))
})
