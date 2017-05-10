
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
  opts <- RCurl::merge.list(opts, list(digits=2))
  opts$x <- x
  res = paste(c("","", do.call(knitr::kable, opts)), collapse="\n")
  asis_output(res)
}

#' @inheritParams lemon_print_data_frame
#' @export
#' @rdname lemon_print
lemon_print_table <- function(x, options=list(), ...) {
  l <- grepl("NA's[ ]+:[[:digit:]]+", x)
  x[l] <- gsub("NA's", "`NA`s", x[l], fixed=TRUE)
  x[is.na(x)] <- ' '
  #margins <- names(dimnames(x))  # to stuff if x is a cross-tabulated table.
  #if (!is.null(margins)) {
  #  x <- cbind(dimnames(x)[[2]], x)
  #  colnames(x)[1] = margins[2]
  #  colnames(x)[2] = paste(margins[1],colnames(x)[2],sep='<br>')
  #}
  options$`kable.opts` <- RCurl::merge.list(options$`kable.opts`, 
                                            list(row.names=FALSE, 
                                                 align='l'))
  lemon_print_data_frame(x, options, ...)
}

#' @rdname lemon_print
#' @param ignore Logical; when \code{TRUE}, print warning when overwriting 
#'               pre-existing names in \code{\link[base]{.GlobalEnv}}.
#' @param safe Logical; when \code{TRUE}, does not overwrite pre-existing names
#'             in \code{\link[base]{.GlobalEnv}}.
#' @export
load_lemon_print <- function(ignore=FALSE, safe=FALSE) {
  set <- list('knit_print.data.frame'=lemon_print_data_frame,
              'knit_print.tbl_df'=lemon_print_data_frame,
              'knit_print.grouped_df'=lemon_print_data_frame,
              'knit_print.table'=lemon_print_table)
  look <- names(set)
  preexisting <- which(look %in% ls(pos=1, all.names=TRUE))
  if (safe) ignore <- TRUE
  if (length(preexisting) > 0 & !ignore) {
    warning('`load_lemon_print` is overwriting ',
            paste(look[preexisting], collapse=', '), ' in global namespace.',call.=FALSE)
  }
  if (length(preexisting) > 0 & safe) look <- look[!preexisting]
  if (length(look) == 0) return(invisible(NULL))
  dput(list(preexisting, look, set))
  invisible(sapply(look, function(n) assign(n, set[[n]], pos=1)))
}
