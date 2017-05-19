library(render)


my_render <- function(render=FALSE) {
  if (render) stat <- render('edwards2017.rmd')
  
  #correct tex
  
  file.copy('edwards2017.tex', 'edwards2017.bak', overwrite=TRUE)
  tex <- paste(readLines('edwards2017.tex'), collapse='\n')
  tex <- gsub('\\textbackslash{}begin\\{Schunk\\}', '\\begin{Schunk}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}end\\{Schunk\\}', '\\end{Schunk}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}begin\\{Sinput\\}', '\\begin{Sinput}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}end\\{Sinput\\}', '\\end{Sinput}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}begin\\{Soutput\\}', '\\begin{Soutput}', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}end\\{Soutput\\}', '\\end{Soutput}', tex, fixed=TRUE)
  tex <- gsub('\\begin{Schunk}', '', tex, fixed=TRUE)
  tex <- gsub('\\end{Schunk}', '', tex, fixed=TRUE)
  
  tex <- gsub('\\textbackslash{}begin\\{figure\\}{[}h!{]}', '\\begin{figure}[h!]', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}begin\\{figure\\}{[}hb!{]}', '\\begin{figure}[hb!]', tex, fixed=TRUE)
  tex <- gsub('\\textbackslash{}end\\{figure\\}', '\\end{figure}', tex, fixed=TRUE)
  
  tex <- gsub('\'\\{e\\}', '\\\'{e}', tex, fixed=TRUE)
  tex <- gsub('\\textpm,1', '\\textpm\\,1', tex, fixed=TRUE)
  
  #m <- gregexpr('\\\\begin\\{Schunk\\}(.*?)\\\\end\\{Schunk\\}', tex)[[1]]
  #if (m[1] != -1) {
  #  
  #}
  writeLines(tex, 'edwards2017.tex')
  
  tools::texi2pdf('RJwrapper')
}

## Updates:
# Abstract
# change_width
"
The bracket functions accepts arguments to change the direction of the end `ticks',
as well as the length of the bracket and the length of the end `ticks'.
"
