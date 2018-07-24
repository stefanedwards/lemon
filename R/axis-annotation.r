# Axis annotations


#' @include ggplot2.r
#' @include lemon-plot.r
NULL



  
# annotated_y_axis   ## adds a label
# annotated_y_axis_custom   ## adds a random grob

#' Annotations in the axis
#' 
#' @rdname annotated_axis
#' @param label Text to print
#' @param y Position on panel's y-scale for label
#' @param colour Colour of label
#' @param side left or right, or top or bottom side to print annotation
#' @param ... ???
#' @example inst/examples/axis-annotation-ex.r
#' @export 
annotated_y_axis <- function(label, y, 
                             colour = waiver(), 
                             side = waiver(), 
                             tick = TRUE,
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
    `_inherit`=if (parsed) AxisAnnotationBquote else AxisAnnotationText,
    aesthetic = 'y',
    side = side,
    params = list(
      label = label,
      tick = tick,
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
#' @rdname annotated_axis
#' @export
#' @inheritParams annotated_y_axis
annotated_x_axis <- function(label, x, 
                             colour = waiver(), 
                             side = waiver(), 
                             tick = TRUE,
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
                         `_inherit`=if (parsed) AxisAnnotationBquote else AxisAnnotationText,
                         aesthetic = 'x',
                         side = side,
                         params = list(
                           label = label,
                           tick = tick,
                           x = x,
                           value = x,
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
    tick = TRUE,
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
      vp <- grid::viewport(yscale = range, xscale=c(0,1))
    } else if (aes == 'x') {
      vp <- grid::viewport(xscale = range, yscale=c(0,1))
    }
    x = switch(side, top=self$params$value, bottom=self$params$value, 
               left=1, right=0)
    x = grid::unit(x, 'native')
    y = switch(side, top=0, bottom=1, 
               left=self$params$value, right=self$params$value)
    y = grid::unit(y, 'native')
    
    # make tick
    
    tel <- ggplot2::calc_element('axis.ticks', theme)
    if (length(tel) > 0 & self$params$tick) {
      tellength <- ggplot2::calc_element('axis.ticks.length', theme)
      x1 = switch(side, top=x, bottom=x, 
                  left=x-tellength, right=x+tellength)
      y1 = switch(side, top=y+tellength, bottom=y-tellength,
                  left=y, right=y)
      lg <- grid::linesGrob(grid::unit.c(x, x1), grid::unit.c(y, y1),
                      vp = vp,
                      #arrow = tel$arrow,
                      gp = grid::gpar(
                        col = self$params$colour %|W|% tel$colour,
                        lwd = tel$size,
                        lty = tel$linetype,
                        lineend = tel$lineend
                      ))
      
      x = switch(side, top=x, bottom=x, 
                  left=x-tellength*1.2, right=x+tellength*1.2)
      y = switch(side, top=y+tellength*1.2, bottom=y-tellength*1.2,
                  left=y, right=y)
    } else {
      tellength <- grid::unit(0, 'cm')
      lg <- zeroGrob()
    }
    
    # make label
     
    element <- switch(side, top='.top', right='.right', '')
    el <- ggplot2::calc_element(paste0('axis.text.', aes, element), theme)

    tg <- grid::textGrob(
      label = self$label(),
      x = x,
      y = y,
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
    grid::gList(tg, lg)
  }
)

AxisAnnotationText <- ggplot2::ggproto('AxisAnnotationText', AxisAnnotation)

AxisAnnotationBquote <- ggplot2::ggproto('AxisAnnotationBquote', AxisAnnotation,
  label = function(self) {
    l <- self$params$label
    l <- gsub("\\.\\(y\\)", round(self$params$value, self$params$digits), l)
    l <- gsub("\\.\\(val\\)", round(self$params$value, self$params$digits), l)
    parse(text=l)
  },
  draw = function(self, side, range, theme) {
    g <- ggplot2::ggproto_parent(AxisAnnotation, self)$draw(side, range, theme)
    g[[1]]$label <- self$label()
    g
  }
)

# Annotation "slot" in the plot ----------------
# Modelled after ScalesList in ggplot2/R/scales-.r


axis_annotation_list <- function() {
  ggproto(NULL, AAList)
}

#' @rdname lemon-ggproto
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
    
    temp <- AxisAnnotationText$draw(side, range, theme)
    temp.lg <- temp[[2]]
    temp <- temp[[1]]
    # coerce to single data.frame where possible
    are_simple <- vapply(annotations, function(a) inherits(a, 'AxisAnnotationText'), logical(1))
    if (sum(are_simple) > 0) {
      
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
        summed$x <- grid::unit(summed$value, 'native')
        summed$y <- temp$y
      } else if (aes == 'y') {
        summed$x <- temp$x
        summed$y <- grid::unit(summed$value, 'native')
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
      if (!is.zero(temp.lg)) {
        if (aes == 'x') {
          summed$x1 <- summed$x
          summed$y1 <- temp.lg$y[1]
        } else if (aes == 'y') {
          summed$x1 <- temp.lg$x[1]
          summed$y1 <- summed$y
        }
        lg <- grid::polylineGrob(
          y = unit.c(summed$y, summed$y1),
          x = unit.c(summed$x, summed$x1),
          id = rep(1:2, times=nrow(summed)),
          gp = temp.lg$gp,
          vp = temp.lg$vp
        )
      } else {
        lg <- ggplot2::zeroGrob()
      }
    } else {
      textgrob <- ggplot2::zeroGrob()
      lg <- ggplot2::zeroGrob()
    }
    
    if (sum(!are_simple) > 0) {
      rest <- do.call(grid::gList,
        lapply(annotations[!are_simple], function(a) a$draw(side, range, theme)))
    } else {
      rest <- ggplot2::zeroGrob()
    }
    grid::grobTree(textgrob, lg, rest, vp=temp$vp, gp=temp$gp)
  },

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
