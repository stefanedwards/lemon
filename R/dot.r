#' Create paths that are safe from changing working directory.
#'
#' The \code{.dot} functions creates functions that allows relative-like 
#' specification of paths, but are safe from changing working directory.
#'
#' @param x File path that is appended to \code{BASEDIR}.
#' @param root Root of your working directory, 
#'             from which \code{x} is relative too.
#' @param mustExist Logical value; if \code{TRUE} and the resulting path does
#'                  not exist, it raises an error.
#' @param relative For \code{.dot}, sets default for the returned function.
#'                 For the returned function, when \code{TRUE}, the function
#'                 returns a path relative to \code{root}.
#' @param create Logical values, creates the target directory when \code{TRUE} (default).
#' @return A function that returns file paths constructed from 
#'         \code{root}, \code{x}, and \code{...}.
#'         
#'         \emph{Side effect:} It creates the directory.
#' @export
#' @rdname dot
#' @examples
#' 
#' .data <- .dot('data')
#' .data('input.txt')
#' .data(c('a.txt','b.txt'))
#' 
#' .dot2(c('rawdata','results'))
#' .rawdata('rawfile.csv')
#' .results('myresults.txt')
.dot <- function(x, root=getwd(), mustExist=FALSE, relative=FALSE, create=TRUE) {
  .rel <- relative
  f <- function(..., relative=.rel) {
    p <- c(...)
    if (length(p) == 0) p <- ''
    if (relative) {
      file.path(file.path(x, .Platform$file.sep, p))
    } else {
      file.path(root, x,  p)
    }
  }
  if (mustExist & !dir.exists(f(''))) stop(paste('Target directory',f(''),'does not exist.'))
  if (create) null <- dir.create(f(''), FALSE, TRUE)
  return(f)
}

#' \code{.dot2} allows specification of multiple .dot functions while 
#' broadcasting the functions' names and target.
#' This function also pushes the function into the calling environment,
#' potentially overwriting previous funtions with same name.
#'
#' @param names Character vector of names
#' @param quiet Logical value, suppresses output to stdout() when \code{TRUE}.
#' @param ... Arguments passed on to \code{.dot}.
#' @rdname dot
#' @export
.dot2 <- function(names, quiet=FALSE, ...) {
  for (s in names) {
    target <- sprintf('.%s', s)
    f <- .dot(s, ...)
    assign(target, f, pos=parent.frame())
    if (!quiet) cat(sprintf('`%s`', target), 'now points to', f(''), '\n')
  }
}
