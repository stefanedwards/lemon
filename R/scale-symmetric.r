# See ggplot2/R/range.r, ggplot2/R/scale-.r, and ggplot2/R/scale-continuous.r

# Upgrades a x or y continuous scale to a symmetric.
set_symmetric_scale <- function(sc, mid) {

  sc$mid <- mid
  sc$train <- function(self, x) {
    if (length(x) == 0)
      return()
    self$range$train(c(x, 2*self$mid-x))
  }

  sc$get_limits <- function(self) {
    l <- ggplot2::ggproto_parent(ScaleContinuousPosition, self)$get_limits()
    l2 <- 2*self$mid - rev(l)
    #str(c(l,l2))

    c(min(l[1],l2[1]), max(l[2],l2[2]))
  }

  sc
}

#' Symmetrix position scale for continuous x and y
#'
#' \code{scale_x_symmetric} and \code{scale_y_symmetric} are like the default
#' scales for continuous x and y, but ensures that the resulting scale is
#' centered around \code{mid}. Does not work when setting limits on the scale.
#'
#' @param mid Value to center the scale around.
#' @param ... Values passed on to \code{\link[ggplot2]{scale_continuous}}.
#' @rdname scale_symmetric
#' @import ggplot2
#' @export
#' @examples
#' library(ggplot2)
#' df <- expand.grid(a=c(-1,0,1), b=c(-1,0,1))
#' rnorm2 <- function(x,y,n,sdx,sdy) {
#'   if (missing(sdy))
#'     sdy <- sdx
#'   data.frame(a=x,b=y,x=rnorm(n,x,sdx), y=rnorm(n,y,sdy))
#' }
#' df <- mapply(rnorm2,df$a, df$b, MoreArgs=list(n=30,sdx=1),SIMPLIFY=FALSE)
#' df <- do.call(rbind, df)
#' (p <- ggplot(df, aes(x=x,y=y)) + geom_point() +
#'   facet_grid(a~b, scales='free_x')
#' )
#' p + scale_x_symmetric(mid=0)
scale_x_symmetric <- function(mid=0, ...) {
  set_symmetric_scale(ggplot2::scale_x_continuous(...), mid=mid)
}

#' @rdname scale_symmetric
#' @export
#  @inheritParams scale_x_symmetric
scale_y_symmetric <- function(mid=0, ...) {
  set_symmetric_scale(ggplot2::scale_y_continuous(...), mid=mid)
}

#' #' @rdname lemon-ggproto
#' #' @keywords internal
#' #' @format NULL
#' #' @usage NULL
#' #' @export
#' #' @import ggplot2
#' #' @import scales
#' ScaleContinuousPositionSymmetric <- ggproto("ScaleContinuousPositionSymmetric",
#'                                             `_inherit`=ggplot2::ScaleContinuousPosition,
#'
#'   range = ggplot2:::continuous_range(),
#'   na.value = NA_real_,
#'   rescaler = scales::rescale, # Used by diverging and n colour gradients x
#'   oob = scales::censor,
#'   minor_breaks = waiver(),
#'
#'   is_discrete = function() FALSE,
#'
#'   train = function(self, x) {
#'     if (length(x) == 0) return()
#'     self$range$train(x)
#'   }
#' )
