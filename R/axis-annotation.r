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
                             hjust = waiver(),
                             vjust = waiver(),
                             size = waiver(),
                             fontface = waiver(),
                             family = waiver(),
                             rot = waiver(),
                             ...) {
  
  if (!missing(print_both) && print_both) {
    print_label <- TRUE
    print_value <- TRUE
  }
  
  aa <- ggplot2::ggproto(NULL, 
    `_inherit`=if (parsed) AxisAnnotation else AxisAnnotationText,
    aesthetic = 'y',
    side = side,
    params = list(
      label = label,
      y = y,
      value = y,
      colour = colour,
      digits = digits,
      print_label = print_label,
      print_value = print_value,
      sep = sep,
      hjust = hjust,
      vjust = vjust,
      size = size,
      fontface = fontface,
      family = family,
      rot = rot,
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
  params = list(
    value = NA,
    label = NA,
    print_label = TRUE,
    print_value = TRUE,
    sep = ' = ',
    digits = 2,
    colour = waiver(),
    hjust = waiver(),
    vjust = waiver(),
    size = waiver(),
    fontface = waiver(),
    family = waiver(),
    rot = waiver()
  ),
  
  label = function(self) {
    if (!self$params$print_label)
      return(self$params$value)
    if (!self$params$print_value)
      return(self$params$label)
    paste0(self$params$label, self$params$sep, round(self$params$value, self$params$digits))
  },
  
  draw = function(self, side, range, theme) {
    aes <- switch(side, top='x', bottom='x', left='y', right='y', NA)
    if (aes == 'y') {
      vp <- grid::viewport(yscale = range)
    } else if (aes == 'x') {
      vp <- grid::viewport(xscale = range)
    }
    
    element <- switch(side, top='.top', right='.right', '')
    el <- ggplot2::calc_element(paste0('axis.text.', aes, element), theme)

    grid::textGrob(
      label = self$label(),
      x = switch(side, top=self$params$value, bottom=self$params$value, 
                 left=1, right=0),
      y = switch(side, top=0, bottom=1, 
                 left=self$params$value, right=self$params$value),
      default.units = 'native',
      vp = vp,
      rot = self$params$rot %|W|% el$angle,
      hjust = self$params$hjust %|W|% el$hjust,
      vjust = self$params$vjust %|W|% el$vjust,
      gp = grid::gpar(
        col = self$params$colour %|W|% el$colour,
        fontsize = self$params$size %|W|% el$size,
        fontfamily = self$params$family %|W|% el$family,
        fontface = self$params$fontface %|W|% el$face
      )
    )
  }
)

AxisAnnotationText <- ggplot2::ggproto('AxisAnnotationText', AxisAnnotation)

AxisAnnotationBquote <- ggplot2::ggproto('AxisAnnotationBquote', AxisAnnotation,
  label = function(self) {
     bquote(quote(self$params$label), list(value=self$params$value, y=self$params$y))
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
  },
  
  draw = function(self, side, is.primary=FALSE, range, theme) {
    annotations <- self$get_annotations(side, is.primary)
    if (length(annotations) == 0)
      return(zeroGrob())
    
    # coerce to single data.frame where possible
    are_simple <- vapply(annotations, function(a) inherits(a, 'AxisAnnotationText'), logical(1))
    if (sum(are_simple) > 0) {
      temp <- AxisAnnotationText$draw(side, range, theme)
      
      summed <- lapply(annotations[are_simple], function(a) {
        data.frame(label = a$label(),
                   value = a$params$value,
                   colour = a$params$colour %|W|% temp$gp$col,
                   hjust = a$params$hjust %|W|% temp$hjust,
                   vjust = a$params$vjust %|W|% temp$vjust,
                   rot = a$params$rot %|W|% temp$rot,
                   fontface = a$params$fontface %|W|% temp$gp$fontface,
                   fontfamily = a$params$family %|W|% temp$gp$fontfamily,
                   fontsize = a$params$size %|W|% temp$gp$fontsize,
                   stringsAsFactors = FALSE
                   )
      })
      summed <- do.call(rbind, summed)
      if (aes == 'x') {
        summed$x <- summed$value
        summed$y <- as.numeric(temp$y)
      } else if (aes == 'y') {
        summed$x <- as.numeric(temp$x)
        summed$y <- summed$value
      }
      textgrob <- grid::textGrob(
        label = summed$label,
        x = summed$x,
        y = summed$y,
        default.units = 'native',
        hjust = summed$hjust,
        vjust = summed$vjust,
        rot = summed$rot,
        gp = grid::gpar(
          col = summed$colour,
          fontface = summed$fontface,
          fontsize = summed$fontsize,
          fontfamily = summed$fontfamily
        ),
        vp = temp$vp
      )
    } else {
      textgrob <- ggplot2::zeroGrob()
    }
    textgrob
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
