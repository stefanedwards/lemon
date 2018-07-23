# Axis annotations


#' @include ggplot2.r
#' @include lemon-plot.r
NULL



  
# annotated_y_axis   ## adds a label
# annotated_y_axis_custom   ## adds a random grob

#' @param label Text to print
#' @param y Position on panel's y-scale for label
#' @param colour Colour of label
#' @param side left or right side to print annotation
#' @param ... ???
#' @example inst/examples/axis-annotation-ex.r
annotated_y_axis <- function(label, y, colour=waiver(), side='right', ...) {
  aa <- ggplot2::ggproto(NULL, AxisAnnotationY,
    side = side,
    params = list(
      label = label,
      y = y,
      colour = colour,
      ...
    )
  )
  
  prependClass(aa, 'axis_annotation')
}


ggplot_add.axis_annotation <- function(object, plot, object_name) {
  plot <- as.lemon_plot(plot)
  plot$axis_annotation$add(object)
  plot
}

# Axis_annotation base class ------
# Modelled after Scales in ggplot2/R/scale

#' @import ggplot2
AxisAnnotation <- ggplot2::ggproto('AxisAnnotation', NULL,
  side = waiver(),
  aesthetic = NULL,
  params = list()
)

AxisAnnotationY <- ggplot2::ggproto('AxisAnnotationY', AxisAnnotation, 
  aesthetic = 'y'
)

# Annotation "slot" in the plot ----------------
# Modelled after ScalesList in ggplot2/R/scales-.r


axis_annotation_list <- function() {
  ggproto(NULL, AAList)
}


#' @import ggplot2
AAList <- ggplot2::ggproto("AAList", NULL,
  annotations = list(),
  
  
  #find = function(self, aesthetic) {
  #£  vapply(self$scales, function(x) any(aesthetic %in% x$aesthetics), logical(1))
  #},
  
  #has_scale = function(self, aesthetic) {
  #  any(self$find(aesthetic))
  #},
  
  # self.annotations holds a heriachical list of all annotations, i.e.
  # self.annotations$x = list(...)
  # self.annotations$y = list(...)
  # but only after first is added.
  add = function(self, new_annotation) {
    if (is.null(new_annotation)) {
      return()
    }
    if (!inherits(new_annotation, 'AxisAnnotation'))
      stop('Not sure what to do with an object of type',class(new_annotation),'.')
    
    new_aes <- new_annotation$aesthetic[1]
    if (is.null(new_aes)) 
      stop('Adding a axis annotation requires an annotation class with either "x" or "y" as aesthetic.')

    if (is.null(self$annotations[[new_aes]])) {
      self$annotations[[new_aes]] <- list(new_annotation)
    } else {
      self$annotations[[new_aes]][[length(self$annotations[[new_aes]]) + 1]] <- new_annotation
    }
  },
  
  n = function(self, aesthetic=NULL) {
    if (is.null(aesthetic))
      return(sum(vapply(self$annotations, length, integer(1))))
    
    vapply(aesthetic, function(x) length(self$annotations[[x]]), integer(1))
    #there <- aesthetic %in% names(self$annotations)
    #notthere <- structure(rep.int(0, sum(!there)), .Names=aesthetic[!there])
    #if (sum(there) > 0) {
    #  there <- vapply(aesthetic[there], function(x) length(self$annotations[x]), integer(1))
    #  return(c(there, notthere)[aesthetic])
    #} else {
    #  return(notthere[aesthetic])
    #}
    
  },
  
  # input = function(self) {
  #   unlist(lapply(self$scales, "[[", "aesthetics"))
  # },
  
  # This actually makes a descendant of self, which is functionally the same
  # as a actually clone for most purposes.
  # clone = function(self) {
  #   ggproto(NULL, self, scales = lapply(self$scales, function(s) s$clone()))
  # },
  # 
  # non_position_scales = function(self) {
  #   ggproto(NULL, self, scales = self$scales[!self$find("x") & !self$find("y")])
  # },
  # 
  # get_scales = function(self, output) {
  #   scale <- self$scales[self$find(output)]
  #   if (length(scale) == 0) return()
  #   scale[[1]]
  # }
  
  # axis annotations can be shown on top, left, right, or bottom.
  # defaults to where the secondary axis is (i.e. not the primary)
  # the annotation itself does not need to know which side it has to be on.
  get_annotations = function(self, side) {
    aes <- switch(side, top='x', bottom='x', left='y', right='y', NA)
    
    i <- which(vapply(self$annotations[aes], function(a) {a$side == side}, logical(1)))
    if (length(i) == 0)
      return()
    
    self$annotations[aes][i]
  }

)
