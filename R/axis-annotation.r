# Axis annotations


#' @include ggplot2.r
#' @include lemon-plot.r
NULL




# annotated_y_axis   ## adds a label
# annotated_y_axis_custom   ## adds a random grob

#' Annotations on the axis
#'
#' @section Showing values:
#' See \link[grDevices]{plotmath} for using mathematical expressions.
#' The function uses a simple replacement strategy where the literal strings
#' \code{.(y)} and \code{.(val)} are replaced by the value after round of to
#' a number of digits, as given by argument \code{digits}.
#'
#' @rdname annotate_axis
#' @param label Text to print
#' @param y,x Position of the annotation.
#' @param side left or right, or top or bottom side to print annotation
#' @param parsed Logical (default \code{FALSE}),
#'   when \code{TRUE}, uses mathplot for outputting expressions.
#'   See section "Showing values".
#' @param print_label,print_value,print_both
#'   Logical; what to show on annotation. Label and/or value.
#'   \code{print_both} is shortcut for setting both \code{print_label} and
#'   \code{print_value}. When both is TRUE, uses argument \code{sep} to
#'   separate the label and value.
#' @param ... Style settings for label and tick:
#'   colour, hjust, vjust, size, fontface, family, rot.
#'   When \code{waiver()} (default),
#'   the relevant theme element is used.
#' @example inst/examples/axis-annotation-ex.r
#' @export
annotate_y_axis <- function(label, y,
                             side = waiver(),
                             print_label = TRUE,
                             print_value = TRUE,
                             print_both = TRUE,
                             parsed = FALSE,
                             ...) {

  if (!missing(print_both)) {
    print_label <- print_both
    print_value <- print_both
  }

  aa <- ggplot2::ggproto(NULL,
    `_inherit`=if (parsed) AxisAnnotationBquote else AxisAnnotationText,
    aesthetic = 'y',
    side = side,
    params = list(
      label = label,
      y = y,
      value = y,
      print_label = print_label,
      print_value = print_value,
      ...
    )
  )


  prependClass(aa, 'axis_annotation')
}
#' @rdname annotate_axis
#' @export
#  @inheritParams annotate_y_axis
annotate_x_axis <- function(label, x,
                             side = waiver(),
                             print_label = TRUE,
                             print_value = TRUE,
                             print_both = TRUE,
                             parsed = FALSE,
                             ...) {

  if (!missing(print_both)) {
    print_label <- print_both
    print_value <- print_both
  }


  aa <- ggplot2::ggproto(NULL,
     `_inherit`=if (parsed) AxisAnnotationBquote else AxisAnnotationText,
     aesthetic = 'x',
     side = side,
     params = list(
       label = label,
       x = x,
       value = x,
       print_label = print_label,
       print_value = print_value,
       ...
     )
  )

  prependClass(aa, 'axis_annotation')
}


# Axis_annotation base class ------
# Modelled after Scales in ggplot2/R/scale

#' @section Axis annotations:
#' Drawing axis annotations requires overloading the normal
#' `ggplot_build` and `ggplot_gtable` to add extra columns and rows for these
#' annotations (see gtable_show_grill).
#'
#' See file `axis-annotation.r`.
#'
#' @md
#' @import grid
#' @import ggplot2
#' @rdname lemon-ggproto
#' @format NULL
#' @usage NULL
# Property 'reducible' is set to TRUE for inherited classes for which their
# labels can easily be reduced to vectors. Bquote objects, not so much?
AxisAnnotation <- ggplot2::ggproto('AxisAnnotation', NULL,
  side = waiver(),
  aesthetic = NULL,
  reducible=FALSE,
  params = list(
    value = NA,
    label = NA,
    print_label = TRUE,
    print_value = TRUE,
    sep = ' = ',
    digits = 2,
    tick = waiver(),
    colour = waiver(),
    hjust = waiver(),
    vjust = waiver(),
    size = waiver(),
    fontface = waiver(),
    family = waiver(),
    rot = waiver()
  ),

  get_param = function(self, x) {
    mine <- self$params[x]
    mine <- mine[!sapply(mine, is.null)]
    if (length(x) > length(mine)) {
      mine <- c(mine, AxisAnnotation$params[setdiff(x, names(mine))])
    }
    if (length(x) == 1)
      return(mine[[1]])
    return(mine)
  },

  label = function(self) {
    if (!self$get_param('print_label'))
      return(round(self$get_param('value'), self$get_param('digits')))
    if (!self$get_param('print_value'))
      return(self$get_param('label'))
    paste0(self$get_param('label'), self$get_param('sep'),
           round(self$get_param('value'), self$get_param('digits')))
  }
)

AxisAnnotationText <- ggplot2::ggproto('AxisAnnotationText', AxisAnnotation, reducible=TRUE)

AxisAnnotationBquote <- ggplot2::ggproto('AxisAnnotationBquote', AxisAnnotation,
  reducible = FALSE,
  params = list(
    print_value = FALSE
  ),
  label = function(self) {
    l <- self$get_param('label')
    l <- gsub("\\.\\(y\\)", round(self$get_param('value'), self$get_param('digits')), l)
    l <- gsub("\\.\\(val\\)", round(self$get_param('value'), self$get_param('digits')), l)
    parse(text=l)
  },
  draw = function(self, side, range, theme) {
    g <- ggplot2::ggproto_parent(AxisAnnotation, self)$draw(side, range, theme)
    g[[1]]$label <- self$label()
    g
  }
)


#' @export
#' @keywords internal
ggplot_add.axis_annotation <- function(object, plot, object_name, ...) {
  plot <- as.lemon_plot(plot)
  plot$axis_annotation <- plot$axis_annotation$clone()
  plot$axis_annotation$add(object)
  plot
}

# Annotation "slot" in the plot ----------------
# Modelled after ScalesList in ggplot2/R/scales-.r


axis_annotation_list <- function() {
  ggproto(NULL, AAList)
}

as.is <- function(x) {x}


#' @export
#' @keywords internal
ggplot_add.axis_annotation <- function(object, plot, object_name, ...) {
  plot <- as.lemon_plot(plot)
  plot$axis_annotation <- plot$axis_annotation$clone()
  plot$axis_annotation$add(object)
  plot
}


#' @rdname lemon-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @import grid
#' @import scales
AAList <- ggplot2::ggproto("AAList", NULL,
  annotations = NULL,

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

    self$annotations <- c(self$annotations, list(new_annotation))
  },

  # See ggplot2/R/scales-.r for cloning.
  # Might be necessary to avoid updating a referenced object.
  clone = function(self) {
    ggproto(NULL, self, annotations=lapply(self$annotations, as.is))
  },

  n = function(self, aesthetic=NULL) {
    if (is.null(aesthetic))
      return(length(self$annotations))

    res <- table(vapply(self$annotations, function(a) a$aesthetic, character(1)))[aesthetic]
    names(res) <- aesthetic
    res[is.na(res)] <- 0
    res
  },


  draw = function(self, side, is.primary=FALSE, range, theme) {
    annotations <- self$get_annotations(side, is.primary)
    if (length(annotations) == 0)
      return(zeroGrob())

    #aes <- switch(side, top='x', bottom='x', left='y', right='y', NA)

    label_render <- switch(side,
       top = "axis.text.x.top", bottom = "axis.text.x",
       left = "axis.text.y", right = "axis.text.y.right"
    )
    tick_render <- switch(side,
       top = 'axis.ticks.x.top', bottom = 'axis.ticks.x',
       left = 'axis.ticks.y', right = 'axis.ticks.y.right'
    )

    default <- ggplot2::calc_element(label_render, theme)
    tick = is.null(render_gpar(theme, tick_render))

    # coerce to single data.frame where possible
    are_reducible <- vapply(annotations, function(a) a$reducible %||% FALSE, logical(1))
    if (sum(are_reducible) > 0) {

      labels <- lapply(annotations[are_reducible], function(a) {
        a$label()
      })

      params <- lapply(annotations[are_reducible], function(a) {
        data.frame(
          values = a$get_param('value'),
          colour = a$get_param('colour') %|W|% default$colour,
          hjust = a$get_param('hjust') %|W|% default$hjust,
          vjust = a$get_param('vjust') %|W|% default$vjust,
          rot = a$get_param('rot') %|W|% default$angle,
          face = a$get_param('fontface') %|W|% default$face,
          family = a$get_param('family') %|W|% default$family,
          size = a$get_param('size') %|W|% default$size,
          tick = a$get_param('tick') %|W|% tick,
          stringsAsFactors = FALSE
        )
      })
      params <- do.call(rbind, params)

      params$tickcolour <- ifelse(params$tick, params$colour, NA)

      axisgrob <- guide_axis(scales::rescale(params$values, from=range), labels, side, theme, default, params)
    } else {
      axisgrob <- guide_axis(NA, NA, side, theme, element_blank(), data.frame(tickcolour=NA))
    }

    gt_index <- which(axisgrob$childrenOrder == 'axis')
    if (sum(!are_reducible) > 0) {

      order <- switch(side,
        top = list(names=c('label','tick'), t=c(1,2), l=c(1,1), r=c(1,1), b=c(1,2)),
        bottom = list(names=c('tick','label'), t=c(1,2), l=c(1,1), r=c(1,1), b=c(1,2)),
        right = list(names=c('tick','label'), t=c(1,1), l=c(1,2), r=c(1,2), b=c(1,1)),
        left = list(names=c('label','tick'), t=c(1,1), l=c(1,2), r=c(1,2), b=c(1,1))
      )

      for (i in which(!are_reducible)) {
        a <- annotations[[i]]
        gp_df <- data.frame(
          values = a$get_param('value'),
          colour = a$get_param('colour') %|W|% default$colour,
          hjust = a$get_param('hjust') %|W|% default$hjust,
          vjust = a$get_param('vjust') %|W|% default$vjust,
          rot = a$get_param('rot') %|W|% default$angle,
          face = a$get_param('fontface') %|W|% default$face,
          family = a$get_param('family') %|W|% default$family,
          size = a$get_param('size') %|W|% default$size,
          tick = a$get_param('tick') %|W|% tick,
          stringsAsFactors = FALSE
        )

        next_grobs <- guide_axis(
          scales::rescale(a$get_param('value'), from=range),
          a$label(), side, theme, default, gp_df)

        axisgrob$children[[gt_index]] <- gtable_add_grob(
          x = axisgrob$children[[gt_index]],
          grobs = next_grobs$children[[gt_index]]$grobs[1:2],
          t=order$t, l=order$l, r=order$l, b=order$b, clip='off',
          name=paste(order$names, i, sep='-')
        )
      }
    }
    axisgrob
  },

  # axis annotations can be shown on top, left, right, or bottom.
  # defaults to where the secondary axis is (i.e. not the primary)
  # the annotation itself does not need to know which side it has to be on.
  #  @param side top, left, right, bottom, etc.
  #  @param is.primary When TRUE, only get annotations whose side are set to that
  #    side. When FALSE (default), also get those that are waiver.
  get_annotations = function(self, side, is.primary=FALSE) {
    aes <- switch(side, top='x', bottom='x', left='y', right='y', NA)

    if (is.primary) {
      get <- vapply(self$annotations, function(a) {
        a$aesthetic == aes && (!is.waive(a$side) && a$side == side)
      }, logical(1))
    } else {
      get <- vapply(self$annotations, function(a) {
        a$aesthetic == aes && (is.waive(a$side) || a$side == side)
      }, logical(1))
    }

    self$annotations[get]
  }

)
