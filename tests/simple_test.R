library(ggplot2)
library(splot)


# Start ------------

dat1 <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30),
  cl = sample.int(3, 30, replace=TRUE),
  cl2 = sample(c('a','b','c'), 30, replace=TRUE)
)

Cmy.theme <- theme_light() + theme(panel.border=element_blank(), axis.line = element_line())
theme_set(my.theme)
                                  
p <- ggplot(dat1, aes(gp, y)) + geom_point()#
p + coord_capped_cart(bottom='right', left='none')
p + coord_flex_cart(bottom=capped_horisontal('none'), left=brackets_vertical())
p + coord_capped_cart(bottom=brackets_horisontal())

# Facet grid -----------------
ggplot(dat1, aes(gp, y)) + geom_point() + 
  coord_capped_flip(bottom = 'left', left='none') + 
  theme(axis.title=element_blank(), plot.title=element_text(size=rel(1))) +
  facet_rep_grid(~cl)

dat2 <- rbind(dat1, data.frame(gp=letters[1:3], y=rnorm(3, 2), cl=3, cl2='b'))
ggplot(dat2, aes(gp, y)) + geom_point() + 
  coord_capped_flip(bottom = 'left', left='none') + 
  theme(axis.title=element_blank(), plot.title=element_text(size=rel(1))) +
  facet_rep_grid(.~cl, scales='free_y')

