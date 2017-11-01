library(testthat)
library(lemon)

expect_ggplot <- function(object, class='ggplot', ...) expect_s3_class(object, class=class, ...)

test_that('Capped axes returns functions and have attributes', {
  for (g in c('both','left','right','none')) {
    ch <- capped_horisontal(g)
    expect_is(ch, 'function', info=paste('capped is', g))
    expect_equal(attr(ch, 'orientation', exact=TRUE), 'horisontal')
  }
  
  for (g in c('both','top','bottom','none')) {
    ch <- capped_vertical(g)
    expect_is(ch, 'function', info=paste('capped is', g))
    expect_equal(attr(ch, 'orientation', exact=TRUE), 'vertical')
  }
})

test_that('Capped axes do not work with incorrect capping argument', {
  for (g in c('left','right')) {
    expect_error(capped_vertical(g), "'arg' should be one of", info=paste('capped is', g))
  }
  
  for (g in c('top','bottom')) {
    expect_error(capped_horisontal(g), "'arg' should be one of", info=paste('capped is', g))
  }
})

