
#' knitr extension: Always use `kable` for data frames.
#'
#' Convenience function for working with R Notebooks that ensures data frames
#' (and dplyr tables) are printed with \code{\link[knitr]{kable}} while
#' allowing RStudio to render the data frame dynamically for inline display.
#'
#' \code{load_lemon_print} pushes \code{lemon_print} functions into relevant
#' S3 functions (\code{knit_print}) in the global namespace, potentially
#' overriding pre-existing functions.
#'
#' @section Knitr chunks:
#'
#' To supply \code{\link[knitr]{kable}} functions such as \code{caption} or
#' \code{col.names}, set chunk options \code{kable.opts}:
#'
#' \preformatted{
#' ```{r kable.opts=list(caption='This is kable table caption.')}`
#' data.frame
#' ```
#' }
#'
#' To disable, temporarily, specify chunk option:
#' \preformatted{
#' ```{r render=normal_print}`
#' data.frame
#' ```
#' }
#'
#'
#' @param x an data frame or dplyr table object to be printed
#' @param options Current chunk options are passed through this argument.
#' @param ... Ignored for now.
#' @seealso \code{\link[knitr]{knit_print}}, \code{\link[knitr]{kable}}
#' @rdname lemon_print
#' @export
#' @import knitr
lemon_print_data_frame = function(x, options, ...) {
  opts <- options$`kable.opts`
  if (is.null(opts)) opts <- NULL
  opts <- RCurl::merge.list(list(x=x, digits=2), opts)
  res = paste(c("","", do.call(knitr::kable, opts)), collapse="\n")
  asis_output(res)
}

#' @rdname lemon_print
#' @param ignore Does not print warning when overwriting pre-existing names.
#' @export
load_lemon_print <- function(ignore=FALSE) {
  look <- c('knit_print.data.frame',
            'knit_print.tbl_df',
            'knit_print.grouped_df')
  preexisting <- which(look %in% ls(pos=1))
  if (length(preexisting) > 0 & !ignore) {
    warning('`load_lemon_print` is overwriting ',
            paste(look[preexisting], collapse=', '), ' in global namespace.',call.=FALSE)
  }
  assign('knit_print.data.frame', lemon_print_data_frame, pos=1)
  assign('knit_print.tbl_df', lemon_print_data_frame, pos=1)
  assign('knit_print.grouped_df', lemon_print_data_frame, pos=1)
}
