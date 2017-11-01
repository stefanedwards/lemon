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

p <- ggplot(dat1, aes(gp, y)) + 
  geom_point(position=position_jitter(width=0.2, height=0)) +
  my.theme

test_that('coord_flex_ and coord_capped_ does not issue errors on ordinary axes', {
  expect_ggplot(p + coord_capped_cart())
  expect_ggplot(p + coord_capped_flip())
  expect_ggplot(p + coord_flex_cart())
  expect_ggplot(p + coord_flex_flip())
})

test_that('coord_flex_ and coord_capped_ issues errors on wrong orientation', {
  verticals <- c('capped_vertical', 'brackets_vertical')
  horisontals <- c('capped_horisontal', 'brackets_horisontal')
  
  v_sides <- c('left','right')
  h_sides <- c('top','bottom')
  
  coords <- c('coord_capped_cart', 'coord_capped_flip', 'coord_flex_cart', 'coord_flex_flip')
  
  for (coo in coords) {
    for (ax in verticals) {
      for (s in v_sides) {
        f <- sprintf('%s(%s=%s())', coo, s, ax)
        expect_ggplot(p + eval(parse(text=f)))
      }
      for (s in h_sides) {
       f <- sprintf('%s(%s=%s())', coo, s, ax)
       expect_error(p + eval(parse(text=f)), 'this will not work', label=f)
      }
    }
    for (ax in horisontals) {
      for (s in h_sides) {
        f <- sprintf('%s(%s=%s())', coo, s, ax)
        expect_ggplot(p + eval(parse(text=f)))
      }
      for (s in v_sides) {
        f <- sprintf('%s(%s=%s())', coo, s, ax)
        expect_error(p + eval(parse(text=f)), 'this will not work', label=f)
      }
    }
  }

})


#Test how capped_axis works when no axes are drawn
