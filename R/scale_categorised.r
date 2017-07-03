library(dplyr)
library(ggplot2)


#' Discrete scale constructor.
#'
#' @export
#' @inheritParams continuous_scale
#' @param breaks One of: 
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     transformation object
#'   - A character vector of breaks
#'   - A function that takes the limits as input and returns breaks
#'     as output
#' @param limits A character vector that defines possible values of the scale
#'   and their order.
#' @param drop Should unused factor levels be omitted from the scale?
#'    The default, `TRUE`, uses the levels that appear in the data;
#'    `FALSE` uses all the levels in the factor.
#' @param na.translate Unlike continuous scales, discrete scales can easily show
#'   missing values, and do so by default. If you want to remove missing values
#'   from a discrete scale, specify `na.translate = FALSE`.
#' @param na.value If `na.translate = TRUE`, what value aesthetic
#'   value should missing be displayed as? Does not apply to position scales
#'   where `NA` is always placed at the far right.
#' @keywords internal
categorised_scale <- function(aesthetics, scale_name, palette, name = waiver(),
                           breaks = waiver(), labels = waiver(), limits = NULL, expand = waiver(),
                           na.translate = TRUE, na.value = NA, drop = TRUE,
                           guide = "legend", position = "left", super = ScaleDiscrete) {
  
  ggplot2:::check_breaks_labels(breaks, labels)
  
  position <- match.arg(position, c("left", "right", "top", "bottom"))
  
  if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
    guide <- "none"
  }
  
  ggproto(NULL, super,
          call = match.call(),
          
          aesthetics = aesthetics,
          scale_name = scale_name,
          palette = palette,
          
          range = discrete_range(),
          limits = limits,
          na.value = na.value,
          na.translate = na.translate,
          expand = expand,
          
          name = name,
          breaks = breaks,
          labels = labels,
          drop = drop,
          guide = guide,
          position = position
  )
}

#' Position scales for discrete data
#'
#' You can use continuous positions even with a discrete position scale -
#' this allows you (e.g.) to place labels between bars in a bar chart.
#' Continuous positions are numeric values starting at one for the first
#' level, and increasing by one for each level (i.e. the labels are placed
#' at integer positions).  This is what allows jittering to work.
#'
#' @inheritDotParams discrete_scale -expand -position
#' @param expand a numeric vector of length two giving multiplicative and
#'   additive expansion constants. These constants ensure that the data is
#'   placed some distance away from the axes.
#' @param position The position of the axis. `left` or `right` for y
#' axes, `top` or `bottom` for x axes
#' @rdname scale_discrete
#' @family position scales
#' @export
#' @examples
#' ggplot(diamonds, aes(cut)) + geom_bar()
#'
#' \donttest{
#' # The discrete position scale is added automatically whenever you
#' # have a discrete position.
#'
#' (d <- ggplot(subset(diamonds, carat > 1), aes(cut, clarity)) +
#'       geom_jitter())
#'
#' d + scale_x_discrete("Cut")
#' d + scale_x_discrete("Cut", labels = c("Fair" = "F","Good" = "G",
#'   "Very Good" = "VG","Perfect" = "P","Ideal" = "I"))
#'
#' # Use limits to adjust the which levels (and in what order)
#' # are displayed
#' d + scale_x_discrete(limits = c("Fair","Ideal"))
#'
#' # you can also use the short hand functions xlim and ylim
#' d + xlim("Fair","Ideal", "Good")
#' d + ylim("I1", "IF")
#'
#' # See ?reorder to reorder based on the values of another variable
#' ggplot(mpg, aes(manufacturer, cty)) + geom_point()
#' ggplot(mpg, aes(reorder(manufacturer, cty), cty)) + geom_point()
#' ggplot(mpg, aes(reorder(manufacturer, displ), cty)) + geom_point()
#'
#' # Use abbreviate as a formatter to reduce long names
#' ggplot(mpg, aes(reorder(manufacturer, displ), cty)) +
#'   geom_point() +
#'   scale_x_discrete(labels = abbreviate)
#' }
scale_x_categorised <- function(..., expand=waiver(), position='bottom') {
  sc <- ggplot2:::discrete_scale(c('x','xmin','xmax','xend'), 'position_d', identity, ...,
                       expand=expand, guide='none', position=position, super=ScaleDiscretePosition)
  sc$range_c <- ggplot2:::continuous_range()
  sc
}


# The discrete position scale maintains two separate ranges - one for
# continuous data and one for discrete data.  This complicates training and
# mapping, but makes it possible to place objects at non-integer positions,
# as is necessary for jittering etc.

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleCategorisedPosition <- ggproto("ScaleCategorisedPosition", ScaleDiscretePosition,
   train = function(self, x) {
     if (is.discrete(x)) {
       self$range$train(x, drop = self$drop, na.rm = !self$na.translate)
     } else {
       self$range_c$train(x)
     }
   },
   
   get_limits = function(self) {
     if (self$is_empty()) return(c(0, 1))
     
     self$limits %||% self$range$range %||% integer()
   },
   
   is_empty = function(self) {
     is.null(self$range$range) && is.null(self$limits) && is.null(self$range_c$range)
   },
   
   reset = function(self) {
     # Can't reset discrete scale because no way to recover values
     self$range_c$reset()
   },
   
   map = function(self, x, limits = self$get_limits()) {
     if (is.discrete(x)) {
       seq_along(limits)[match(as.character(x), limits)]
     } else {
       x
     }
   },
   
   dimension = function(self, expand = c(0, 0)) {
     c_range <- self$range_c$range
     d_range <- self$get_limits()
     
     if (self$is_empty()) {
       c(0, 1)
     } else if (is.null(self$range$range)) { # only continuous
       expand_range(c_range, expand[1], expand[2] , 1)
     } else if (is.null(c_range)) { # only discrete
       expand_range(c(1, length(d_range)), expand[1], expand[2], 1)
     } else { # both
       range(
         expand_range(c_range, expand[1], 0 , 1),
         expand_range(c(1, length(d_range)), 0, expand[2], 1)
       )
     }
   },
   
   get_breaks = function(self, limits = self$get_limits()) {
     ggproto_parent(ScaleDiscrete, self)$get_breaks(limits)
   },
   
   clone = function(self) {
     new <- ggproto(NULL, self)
     new$range <- discrete_range()
     new$range_c <- continuous_range()
     new
   }
)


df <- data.frame(cat=rep(rep(letters[1:3], times=3:1), 3)) %>% mutate(x=runif(n()))



(p <- ggplot(df, aes(x=cat, y=x, colour=cat)) + geom_point() +
  scale_x_categorised()
)

# try these:
#aes(order=cat)
# scale_x_ordered(aes(order=..))
