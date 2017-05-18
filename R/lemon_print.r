
#' knitr extension: Always use `kable` for data frames.
#'
#' Convenience function for working with R Notebooks that ensures data frames
#' (and dplyr tables) are printed with \code{\link[knitr]{kable}} while
#' allowing RStudio to render the data frame dynamically for inline display.
#' 
#' 
#' These functions divert data frame and summary output to 
#' \code{\link[knitr]{kable}} for nicely printing the output.
#' 
#' For \emph{options to \code{kable}}, they can be given directly as 
#' chunk-options (see arguments to \code{\link[knitr]{kable}}), or though
#' as a list to a special chunk-option \code{kable.opts}.
#'
#' For more examples, see \code{vignette('lemon_print', package='lemon')}.
#'
#' @section Knitr usage:
#'
#' To use for a single chunk, do
#' \preformatted{
#' ```{r render=lemon_print,caption='My data frame'}
#' data.frame
#' ```
#' }
#' 
#' \strong{Note:} We are \emph{not} calling the function, 
#' but instead refering to it.
#' 
#' An alternate route for specifying \code{\link[knitr]{kable}} arguments is as:
#' 
#' \preformatted{
#' ```{r render=lemon_print,kable.opts=list(align='l')}
#' data.frame
#' ```
#' }
#' 
#' The option \code{kable.opts} takes precendence over arguments given directly
#' as chunk-options.
#' 
#'  
#'
#' To enable as default printing method for \emph{all chunks}, include
#' 
#' \preformatted{
#'   knit_print.data.frame <- lemon_print
#'   knit_print.table <- lemon_print
#' }
#'
#' \strong{Note:} We are \emph{not} calling the function, 
#' but instead assigning the \code{\link[knitr]{knit_print}} functions
#' for some classes.
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
lemon_print <- function(x, options, ...) {
  UseMethod('lemon_print', x)
}

#' @inheritParams lemon_print
#' @export
#' @rdname lemon_print
#' @import RCurl
lemon_print.data.frame = function(x, options, ...) {
  kable.opts <- options$`kable.opts`
  opts <- options[c('format','digits','row.names','col.names','caption','align','format.args','escape')]
  if (is.null(kable.opts)) kable.opts <- list()
  opts <- RCurl::merge.list(kable.opts, opts)
  opts$x <- x
  res = paste(c("","", do.call(knitr::kable, opts)), collapse="\n")
  asis_output(res)
}

#' @inheritParams lemon_print
#' @export
#' @rdname lemon_print
lemon_print.table <- function(x, options, ...) {
  # Do nothing for cross-tabulation tables.
  if (is.null(dimnames(x))) return(knitr::knit_print(unclass(x), options, ...))
  if (!is.null(names(dimnames(x))) & all(names(dimnames(x)) == '')) 
    return(knitr::knit_print(unclass(x), options,  ...))
  
  # detect if we have a summary
  if (length(dim(x)) == 2 & all(dimnames(x)[[1]] == '') & all(dimnames(x)[[2]] != '')) {
    l <- grepl("NA's[ ]+:[[:digit:]]+", x)
    x[l] <- gsub("NA's", "`NA`s", x[l], fixed=TRUE)
    x[is.na(x)] <- ' '
    options$`kable.opts` <- RCurl::merge.list(options$`kable.opts`, 
                                              list(row.names=FALSE, 
                                                   align='l'))
    lemon_print.data.frame(x, options, ...)
  } else {
    knitr::knit_print(unclass(x), options, ...)
  }
}
