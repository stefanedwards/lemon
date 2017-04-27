## ----setup,include=FALSE-------------------------------------------------
library(knitr)

knitr::opts_chunk$set(fig.height=4, fig.width=6, verbatim=FALSE)

# Show chunk verbatim
# Source: http://stackoverflow.com/questions/19908158/show-an-r-markdown-chunk-in-the-final-output
# Set verbatim option as last and it will not be printed. ;)
hook_source_def = knit_hooks$get('source')
knit_hooks$set(source = function(x, options){
  if (!is.null(options$verbatim) && options$verbatim){
    opts = gsub(",\\s*verbatim\\s*=\\s*TRUE\\s*", "", options$params.src)
    bef = sprintf('\n\n    ```{r %s}\n', opts, "\n")
    stringr::str_c(bef, paste(knitr:::indent_block(x, "    "), collapse = '\n'), "\n    ```\n")
  } else {
     hook_source_def(x, options)
  }
})


## ------------------------------------------------------------------------
data(USArrests)
head(USArrests)

## ------------------------------------------------------------------------
library(splot)

## ----kable.opts=list(caption="Data frame is now printed using `kable`."),verbatim=TRUE----
head(USArrests)

## ----disable_knit_print--------------------------------------------------
options(knit_print=FALSE)
head(USArrests)

## ------------------------------------------------------------------------
kable(head(USArrests))

## ----reenable_knit_print-------------------------------------------------
options(knit_print=TRUE)
head(USArrests)

## ------------------------------------------------------------------------
kable(head(USArrests))

