library(vdiffr)

test_that('Different sides', {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(gear ~ carb)


  expect_doppelganger('four sides', p + theme(
    panel.border = element_box(
      left = element_line(color='green'),
      top = element_line(colour='blue', linetype = 2, linewidth=3),
      right = element_line(colour='red', linewidth = 2),
      bottom = element_line(colour='orange'),
    ))
  )

  expect_doppelganger('sides', p + theme(
    panel.border = element_box(side = 'L',
      color='blue', linetype=3, linewidth=2
    ))
  )

  expect_doppelganger('combo', p + theme(
    panel.border = element_box(
      left = element_line(color='green'),
      side = 'L',
      color='blue', linetype=3, linewidth=2
    ))
  )
})
