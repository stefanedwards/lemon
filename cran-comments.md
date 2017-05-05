# Comments 

## Notes
  
* checking R code for possible problems ... NOTE
  Found the following assignments to the global environment:
  File 'lemon/R/lemon_print.r':
  assign(n, set[[n]], pos = 1)
  
  This comes from the function `load_lemon_print`, as I decided loading the
  package should _not_ overwrite `knit_print.*` names. 
  `load_lemon_print` pushes some functions into the global environment to 
  utilise knitr's knit_print function for some specific data structures.

* checking top-level files ... NOTE
  Non-standard file/directory found at top level:
  'cache'

## Warnings

* checking dependencies in R code ... WARNING
  'loadNamespace' or 'requireNamespace' call not declared from: 'lattice'

  We are not importing `lattice` in this package. 
  I cannot trace the origin of this message.
  
* checking for unstated dependencies in examples ... OK
  WARNING
  'qpdf' is needed for checks on size reduction of PDFs
  
  ??
  