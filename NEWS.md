# lemon 0.3.9000

* **TODO:** Create `grobHeight` on gtables.

* **TODO:** Create CITATION file.

* **TODO:** Extend `lemon_print` to work on cross-tabulation tables.

* **New feature**: `geom_siderange` which kinda works like `geom_rug`,
  but displays a line within the plotting area and follows usual rules
  of aesthetics i.e. colour, and with added benefit of defining 
  starting and ending point symbol.

# lemon 0.3.3-1

* Bug fix: `linesize` did not work on `geom_pointline`.

# lemon 0.3.3

* Added argument `offset` to `reposition_legend` to better nudge legend into place.

* `brackets_horizontal` and `brackets_vertical` now uses 'npc' as default
  unit, as well as defaults to this unit if given a numeric.

* `brackets_horizontal` and `brackets_vertical` skips changing ticks to brackets
  if theme(axis.ticks = element_blank()) and thus not created at all.

* 'horisontal' is not an English word. Has been thoroughly replaced by 'horizontal'.
  Functions with not-English word in names are still available, but not advertised.

* Extended `facet_rep_grid` and `facet_rep_wrap` to allow user to specify which
  sided tick labels should be kept. Can still use logicals for `repeat.tick.labels`.
  
* `coord_capped_*` and `coord_flex_*` now checks if the provided `brackets_*`
  and `capped_*` functions corresponds to the correct orientation.

* **New feature** `geom_pointpath` and `geom_pointline` that combines
  `geom_point` with `geom_path` and `geom_line`, respectively.
  These geoms apply any position adjustments (e.g.\ `position_jitter`) to 
  *both* points and lines.
  Also adds a gap between point and end of lines that grows with pointsize.

# lemon 0.3.2

* `grid_arrange_shared_legend` can now accept other grobs in the ... arguments.
  It can therefore make a bit more complex plots, as shown under 'Examples' in
  the legend-vignette.

# lemon 0.3.1

* `reposition_legend` now places legend *under* axis lines, 
  and further accepts x and y arguments for nudging.

# lemon 0.3.0

* Added `.dot` functions for supplying `setwd()` safe relative file paths.

* `knit_print` functions are removed (without deprecating them) and replaced
  by `lemon_print`.
  `lemon_print` has been extended to also work on tables (i.e. a summary), but
  does not work nicely for cross-tabulation tables.

# lemon 0.2.0

* Legend functions (`g_legend`, `grid_arrange_shared_legend`, `reposition_legend`).
  Two first functions provided by Baptiste Auguié and Shaun Jackson.

# lemon 0.1.0

* Capped axis functions (`capped_horisontal`, `capped_vertical`), the flexible
Cartesian coordinates (`coord_capped_cart`, `coord_capped_flip`, 
`coord_flex_cart`, `coord_flex_flip`, `coord_flex_fixed`), and brackets 
(`brackets_horisontal`, `brackets_vertical`).

* Facets that repeat the labels (`facet_rep_grid`, `facet_rep_wrap`).

* `knitr` printing functions (`knit_print.data.frame`, etc.) [REMOVED per 0.2.9000].
