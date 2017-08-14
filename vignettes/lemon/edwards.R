## ----setup,include=FALSE-------------------------------------------------
library(knitr)
library(ggplot2)
library(lemon)

knitr::opts_knit$set(out.format='latex')
knitr::opts_chunk$set(fig.width=5.5, fig.height=5.5/4*2.5, fig.path='edwards2017-',
                      cache=TRUE, autodep = TRUE, cache.path = 'cache/')

tex_ordinal <- function(x) {
  x <- scales::ordinal(x)
  gsub('([0-9]+)(st|nd|rd|th)', '\\1\\\\textsuperscript{\\2}', x)
}

## ------------------------------------------------------------------------
library(ggplot2)
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(title='A scatter plot')
g <- ggplotGrob(p)

## ----p_object------------------------------------------------------------
print(p)

## ----g_object------------------------------------------------------------
print(g)

## ----gtable_show_names,echo=FALSE----------------------------------------
library(lemon)
library(gtable)
library(grid)
s <- gtable_show_grill(p)
grid.draw(gtable_show_names(s, plot=FALSE, rect.gp=gpar(fill='white', col='white', alpha=1/4)))

## ------------------------------------------------------------------------
g$widths

## ----replace_grob--------------------------------------------------------
g <- ggplot2::ggplotGrob(p)
i <- which(g$layout$name == 'xlab-b')
g$grobs[[i]] <- grid::roundrectGrob(r=unit(0.5, 'snpc'), 
                                    gp=gpar(fill='pink', col='black'))
grid::grid.draw(g)

## ----change_width--------------------------------------------------------
g <- ggplot2::ggplotGrob(p)
layout <- subset(g$layout, name == 'panel')
g$widths[[layout$l]] <- unit(5, 'cm')
grid::grid.draw(g)

## ----reposition_legend---------------------------------------------------
reposition_legend(p %+% aes(color=cyl), position = 'top right')

## ----multiple_legends----------------------------------------------------
x <- p %+% aes(color=cyl, size=qsec)
g <- g_legend(x)
x <- reposition_legend(x, position='bottom left', legend=g$grobs[[2]], plot=FALSE)
x <- reposition_legend(x, position='top right', legend=g$grobs[[1]], plot=FALSE)
grid.draw(x)

## ----brackets,eval=FALSE-------------------------------------------------
## p <- ggplot(mpg, aes(as.factor(cyl), hwy, colour=class)) +
##   geom_point(position=position_jitter(width=0.3)) +
##   theme_bw()
## 
## # Demonstrate brackets, left plot
## p1 <- p + coord_flex_cart(bottom=brackets_horisontal(length=unit(0.08, 'npc'))) +
##   theme(panel.border = element_blank(), axis.line = element_line(),
##         panel.grid.major.x = element_blank())
## 
## p <- ggplot(mpg, aes(displ, hwy, colour=class)) + geom_point() +
##   theme_bw()
## 
## # Demonstrate capped axis lines, right plot
## p2 <- p + coord_capped_cart(bottom='right', left='none', gap = 0.03) +
##   theme(panel.border = element_blank(), axis.line = element_line(),
##         panel.grid.major.x = element_blank())
## 
## # Demonstrate shared legend
## grid_arrange_shared_legend(p1, p2, position='bottom')

## ----brackets,echo=FALSE-------------------------------------------------
p <- ggplot(mpg, aes(as.factor(cyl), hwy, colour=class)) +
  geom_point(position=position_jitter(width=0.3)) +
  theme_bw()

# Demonstrate brackets, left plot
p1 <- p + coord_flex_cart(bottom=brackets_horisontal(length=unit(0.08, 'npc'))) +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major.x = element_blank())

p <- ggplot(mpg, aes(displ, hwy, colour=class)) + geom_point() +
  theme_bw()

# Demonstrate capped axis lines, right plot
p2 <- p + coord_capped_cart(bottom='right', left='none', gap = 0.03) +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major.x = element_blank())

# Demonstrate shared legend
grid_arrange_shared_legend(p1, p2, position='bottom')

## ----facet_rep-----------------------------------------------------------
df <- diamonds[sample.int(nrow(diamonds), 2000),]
d <- ggplot(df, aes(carat, price)) +
  geom_point(aes(colour = clarity)) + 
  coord_capped_cart(bottom='none', left='bottom') +
  facet_grid(.~cut) + theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(size = 0.4),
        legend.position='bottom')
d + facet_rep_grid(.~cut)

