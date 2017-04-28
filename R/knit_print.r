
#' knitr extension: Always use `kable` for data frames.
#' 
#' Convenience function for working with R Notebooks that ensures data frames
#' (and dplyr tables) are printed with \code{\link[knitr]{knit_print}} while
#' allowing RStudio to render the data frame dynamically for inline display.
#' \strong{NB:} Automatically enables when package \code{splot} is loaded!
#' 
#' To disable these functions use \code{options(knit_print=FALSE)}. 
#' Set to \code{TRUE} to re-enable.
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
#' 
#' 
#' 
#' @param x an data frame or dplyr table object to be printed
#' @param options Current chunk options are passed through this argument.
#' @param ... Ignored for now.
#' @seealso \code{\link[knitr]{knit_print}}, \code{\link[knitr]{kable}}
#' @rdname knit_print
#' @keywords print
#' @export
#' @import knitr
knit_print.data.frame = function(x, options, ...) {
  if (getOption('knit_print', default=TRUE) == FALSE) 
    return(knitr::normal_print(x, options, ...))
  opts <- options$`kable.opts`
  if (is.null(opts)) opts <- NULL
  opts <- RCurl::merge.list(list(x=x, digits=2), opts)
  res = paste(c("","", do.call(knitr::kable, opts)), collapse="\n")
  asis_output(res)
}

#' @inheritParams knit_print.tbl_df
#' @rdname knit_print
#' @export
#' @keywords print
knit_print.tbl_df = function(x, ...) {
  knit_print.data.frame(x, ...)
}

#' @inheritParams knit_print.tbl_df
#' @rdname knit_print
#' @export
#' @keywords print
knit_print.grouped_df = function(x, ...) {
  knit_print.data.frame(x, ...)
}
