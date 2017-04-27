## ----setup,include=FALSE-------------------------------------------------
library(knitr)

knitr::opts_chunk$set(fig.height=4, fig.width=6)

## ----load_pkg_and_data,fig.cap='Default ggplot2 plotting.'---------------
#library(ggplot2)
library(splot)

dat1 <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30),
  cl = sample.int(3, 30, replace=TRUE),
  cl2 = sample(c('a','b','c'), 30, replace=TRUE)
)

my.theme <- theme_light()

(
  p <- ggplot(dat1, aes(gp, y)) + geom_point() + my.theme
)

## ----theme---------------------------------------------------------------
my.theme <- my.theme + theme(panel.border=element_blank(), axis.line = element_line())
p <- p + my.theme

## ----fig.cap='Using `coord_capped_cart` to cap the bottom axis from the right. The left axis is unaffected.'----
p + coord_capped_cart(bottom='right')

## ----fig.cap='As before, but left axis is now also capped to give a consistent look.'----
p + coord_capped_cart(bottom='right', left='none')

## ----fig.cap='Placing brackets brackets instead of ticks emphasises that the x-scale is categorical and not nominal.'----
ggplot(dat1, aes(gp, y)) + geom_point(position=position_jitter(width=0.2, height=0)) +
  coord_capped_cart(left='none', bottom=brackets_horisontal()) +
  my.theme + theme(panel.grid.major.x = element_blank())

## ----eval=FALSE----------------------------------------------------------
#  
#  # Facet grid -----------------
#  ggplot(dat1, aes(gp, y)) + geom_point() +
#    coord_capped_flip(bottom = 'left', left='none') +
#    theme(axis.title=element_blank(), plot.title=element_text(size=rel(1))) +
#    facet_rep_grid(~cl)
#  
#  dat2 <- rbind(dat1, data.frame(gp=letters[1:3], y=rnorm(3, 2), cl=3, cl2='b'))
#  ggplot(dat2, aes(gp, y)) + geom_point() +
#    coord_capped_flip(bottom = 'left', left='none') +
#    theme(axis.title=element_blank(), plot.title=element_text(size=rel(1))) +
#    facet_rep_grid(.~cl, scales='free_y')
#  
#  

