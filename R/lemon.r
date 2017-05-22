#' Freshing up your ggplots
#'
#' Collection of misc. functions for changing subtle aspects of ggplots.
#' Works mostly on gtables produced prior to printing.
#'
#' @section Functions for axis:
#'
#' See \code{\link{coord_capped_cart}} and \code{\link{coord_flex_cart}}.
#' The latter is a shorthand version of the former.
#' It automatically uses \code{\link{capped_horisontal}} and
#' \code{\link{capped_vertical}}, but both accepts these as well as
#' \code{\link{brackets_horisontal}} and \code{\link{brackets_vertical}}.
#'
#' @section Legends:
#'
#' \describe{
#'   \item{Extract legend}{\code{\link{g_legend}}}
#'   \item{Many plots, one legend}{\code{\link{grid_arrange_shared_legend}}}
#'   \item{Place legend exactly on plot}{\code{\link{reposition_legend}}}
#' }
#'
#' @section Facets:
#'
#' \code{\link{facet_rep_grid}} and \code{\link{facet_rep_wrap}} are extensions
#' to the wellknown \code{\link[ggplot2]{facet_grid}} and
#' \code{\link[ggplot2]{facet_wrap}} where axis lines and labels are drawn on
#' all panels.
#'
#' @section Extending knitr:
#'
#' We automatically load knitr's \code{\link[knitr]{knit_print}} for
#' data frames and dplyr tables to provide automatic pretty printing of
#' data frame using \code{\link[knitr]{kable}}.
#'
#' See \code{\link{lemon_print}} or \code{vignette('lemon_print', 'lemon')}.
#'
#' Relative paths safe from hanging directory: \code{\link{.dot}}.
#'
#' @docType package
#' @name lemon
#' @author Stefan McKinnon Edwards <sme@@iysik.com>
#' 
#' Contributions from \href{http://baptiste.github.io/}{Baptiste Auguié} on
#' \code{\link{g_legend}} and \code{\link{grid_arrange_shared_legend}}.
#' 
#' Contributions from \href{http://rpubs.com/sjackman}{Shaun Jackman} on
#' \code{\link{grid_arrange_shared_legend}}.
#' @import ggplot2 lattice
#' @source \url{https://github.com/stefanedwards/lemon}
"_PACKAGE"
