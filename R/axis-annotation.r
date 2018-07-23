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
annotated_y_axis <- function(label, y, 
                             colour = waiver(), 
                             side = waiver(), 
                             digits = 2, 
                             parsed = FALSE,
                             print_label = TRUE,
                             print_value = TRUE,
                             print_both = TRUE,
                             sep = ' = ',
                             ...) {
  
  if (!missing(print_both) && print_both) {
    print_label <- TRUE
    print_value <- TRUE
  }
  
  aa <- ggplot2::ggproto(NULL, if (parsed) AxisAnnotationYParsed else AxisAnnotationY,
    side = side,
    params = list(
      label = label,
      y = y,
      value = y,
      colour = colour,
      digits = digits,
      print_label = print_label,
      print_value = print_value,
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
AxisAnnotationSimple <- ggplot2::ggproto('AxisAnnotationSimple', AxisAnnotation,
  side = waiver(),
  aesthetic = NULL,
  params = list(
    value = NA,
    label = NA,
    print_label = TRUE,
    print_value = TRUE,
    colour = waiver(),
    sep = ' = '
  ),
  parse = function(self) {
    if (!self$params$print_label)
      return(self$params$value)
    if (!self$params$print_value)
      return(self$params$label)
    paste0(self$params$label, self$params$sep, self$params$value)
  }
)
AxisAnnotationParsed <- ggplot2::ggproto('AxisAnnotationParsed', AxisAnnotation)

AxisAnnotationY <- ggplot2::ggproto('AxisAnnotationY', AxisAnnotationSimple, 
  aesthetic = 'y'
)
AxisAnnotationYParsed <- ggplot2::ggproto('AxisAnnotationYParsed', AxisAnnotationParsed,
  aesthetic = 'y',
  parse = function(self) {
    stop('Doesn\'t work yet.')
  }
)



# Annotation "slot" in the plot ----------------
# Modelled after ScalesList in ggplot2/R/scales-.r


axis_annotation_list <- function() {
  ggproto(NULL, AAList)
}


#' @import ggplot2
#' @import grid
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
      n <- length(self$annotations[[new_aes]])
      self$annotations[[new_aes]][n+1] <- list(new_annotation)
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
  
  draw = function(self, side, is.primary=FALSE, range, theme) {
    annotations <- self$get_annotations(side, is.primary)
    if (length(annotations) == 0)
      return(zeroGrob())
    
    aes <- switch(side, top='x', bottom='x', left='y', right='y', NA)
    element.side <- switch(side, top='.top', right='.right', '')
    x = switch(side, top=0.5, bottom=0.5, left=1, right=0)
    y = switch(side, top=0, bottom=1, left=0.5, right=0.5)
    hjust = switch(side, top=0.5, bottom=0.5, left=1, right=0)
    vjust = switch(side, top=1, bottom=0, left=0.5, right=0.5)
    
    gp <- render_gpar(theme, paste0('axis.text.', aes, element.side))
    if (aes == 'y') {
      vp <- grid::viewport(yscale = range)
    } else if (aes == 'x') {
      vp <- grid::viewport(xscale = range)
    }
    
    # coerce to single data.frame where possible
    are_simple <- vapply(annotations, function(a) inherits(a, 'AxisAnnotationSimple'), logical(1))
    collective <- lapply(annotations[are_simple], function(a) {
      data.frame(label=a$parse(), 
                 pos=a$params$value, 
                 col=a$params$colour %|W|% gp$col,
                 x = x,
                 y = y,
                 hjust=hjust,
                 vjust=vjust,
                 rot=0
                 )
    })
    collective <- do.call(rbind, collective)
    collective[aes] <- collective$pos
    if (nrow(collective) > 0) {
      simple_text <- with(collective, grid::textGrob(
        label = label, x = x, y = y, default.units='native',
        hjust = hjust, vjust = vjust, rot=rot,
        gp = grid::gpar(
          col = col,
          fontsize = gp$fontsize,
          fontfamily = gp$fontfamily,
          fontface = gp$font
        ),
        vp = vp
      ))
    } else {
      simple_text <- zeroGrob()
    }
    
    simple_text
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
  #' @param side top, left, right, bottom, etc.
  #' @param is.primary When TRUE, only get annotations whose side are set to that
  #'   side. When FALSE (default), also get those that are waiver.
  get_annotations = function(self, side, is.primary=FALSE) {
    aes <- switch(side, top='x', bottom='x', left='y', right='y', NA)
    
    if (self$n(aes) == 0)
      return()
    
    if (is.primary) {
      # only those with side as set
      i <- which(vapply(self$annotations[[aes]], function(a) {
        !is.waive(a$side) && a$side == side
      }, logical(1)))
    } else {
      i <- which(vapply(self$annotations[[aes]], function(a) {
        is.waive(a$side) || a$side == side
      }, logical(1)))
    }
    
    self$annotations[[aes]][i]
  }

)
