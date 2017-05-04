# lemon 0.2.0.9000

* TODO: Copy in non-exported ggplot2 and gtable functions in preparation for 
CRAN submission (render_axis and absoluteGrob).

* Update g_legend to be authored by baptiste
(https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs/047381b48b0f0ef51a174286a595817f01a0dfad)

* Add baptiste and Shaun Jackson (grid_arrange_shared_legend) to contributing
authors.

# lemon 0.2.0

* Legend functions (`g_legend`, `grid_arrange_shared_legend`, `reposition_legend`).

# lemon 0.1.0

* Capped axis functions (`capped_horisontal`, `capped_vertical`), the flexible
Cartesian coordinates (`coord_capped_cart`, `coord_capped_flip`, 
`coord_flex_cart`, `coord_flex_flip`, `coord_flex_fixed`), and brackets 
(`brackets_horisontal`, `brackets_vertical`).

* Facets that repeat the labels (`facet_rep_grid`, `facet_rep_wrap`).

* `knitr` printing functions (`knit_print.data.frame`, etc.).