library(testthat)
library(vdiffr)

expect_ggplot <- function(object, class='ggplot', ...) expect_s3_class(object, class=class, ...)

test_that('Capped axes returns functions and have attributes', {
  for (g in c('both','left','right','none')) {
    ch <- capped_horizontal(g)
    expect_is(ch, 'function', info=paste('capped is', g))
    expect_equal(attr(ch, 'orientation', exact=TRUE), 'horizontal')
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
    expect_error(capped_horizontal(g), "'arg' should be one of", info=paste('capped is', g))
  }
})

test_that('Plot works with vdiffr', {
  data(mtcars)
  p <- ggplot(mtcars, aes(x = mpg)) + geom_dotplot(binwidth = 1) +
    scale_y_continuous(sec.axis = sec_axis(~.*100)) +
    scale_x_continuous(sec.axis = sec_axis(~1/., name='Madness scale')) +
    coord_capped_cart(bottom='left', left='both', top='none', right='both') +
    theme_bw() +
    theme(panel.border=element_blank(), axis.line=element_line())
  expect_doppelganger("capped axes", p)

})
