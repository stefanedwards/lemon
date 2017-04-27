## ----setup,include=FALSE-------------------------------------------------
library(knitr)

knitr::opts_chunk$set(fig.height=4, fig.width=6)

## ------------------------------------------------------------------------
library(ggplot2)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- ggplot(dsamp, aes(carat, price)) +
  geom_point(aes(colour = clarity)) +
  theme(legend.position = c(0.1, 0.7))
)

## ----echo=FALSE,fig.height=3, fig.width=3--------------------------------
d

## ----reposition_legend---------------------------------------------------
library(splot)
reposition_legend(d, 'top left')

## ----reposition_legend_small,eval=FALSE,fig.height=2, fig.width=2--------
#  reposition_legend(d, 'top left')

## ------------------------------------------------------------------------
d2 <- d + facet_grid(.~cut)
ggplot_gtable(ggplot_build(d2))

## ----reposition_legend_facet1--------------------------------------------
reposition_legend(d2, 'top left', panel = 'panel-3-1')

## ----reposition_legend_facet2--------------------------------------------
reposition_legend(d + facet_wrap(~cut, ncol=3), 'top left', panel='panel-3-2')

## ----reposition_legend_facet3--------------------------------------------
d3 <- d + facet_wrap(~cut, ncol=3) + scale_color_discrete(guide=guide_legend(ncol=3))
reposition_legend(d3, 'center', panel='panel-3-2')

