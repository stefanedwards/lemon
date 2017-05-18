library(render)


my_render <- function(...) {
  stat <- render('edwards2017.rmd')
  
  #correct tex
  
  file.copy('edwards2017.tex', 'edwards2017.bak', overwrite=TRUE)
  tex <- paste(readLines('edwards2017.tex'), collapse='\n')
  tex <- gsub('\\textbackslash{}begin\\{Schunk\\}', '\\begin{Schunk}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}end\\{Schunk\\}', '\\end{Schunk}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}begin\\{Sinput\\}', '\\begin{Sinput}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}end\\{Sinput\\}', '\\end{Sinput}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}begin\\{Soutput\\}', '\\begin{Soutput}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}end\\{Soutput\\}', '\\end{Soutput}', tex, fixed=TRUE)
  
  tex <- gsub('\'\\{e\\}', '\\\'{e}', tex, fixed=TRUE)
  tex <- gsub('\\textpm,1', '\\textpm\\,1', tex, fixed=TRUE)

  #m <- gregexpr('\\\\begin\\{Schunk\\}(.*?)\\\\end\\{Schunk\\}', tex)[[1]]
  #if (m[1] != -1) {
  #  
  #}
  writeLines(tex, 'edwards2017.tex')
  
  system2('pdflatex','RJwrapper')
  system2('bibtex','RJwrapper')
  system2('pdflatex','RJwrapper')
  system2('pdflatex','RJwrapper')
}
