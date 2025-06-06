# Main function -----------------------------------------------------------

#' @title Layout panels in a grid with nested strips
#'
#' @description `facet_nested()` forms a matrix of panels defined by row
#'   and column faceting variables and nests grouped facets.
#'
#' @inheritParams facet_grid2
#' @param nest_line a theme element, either `element_blank()` or inheriting
#'   from [ggplot2::element_line()]. Lines are drawn between layers of strips
#'   indicating hierarchy. The element inherits from the
#'   [`ggh4x.facet.nestline`][theme_extensions] element in the theme.
#' @param solo_line A `logical(1)` indicating whether parent strips with a
#'  single child should be drawn with a `nest_line` (`TRUE`) or the line only
#'  applies to parents with multiple children (`FALSE`, default). Only relevant
#'  when `nest_line` is drawn.
#' @param resect  a `unit` vector of length 1, indicating how much the
#'   nesting line should be shortened.
#' @param strip An object created by a call to a strip function, such as
#'   [ggh4x::strip_nested()].
#' @param bleed `r lifecycle::badge("deprecated")` the `bleed` argument has
#'   moved to the `strip_nested()` function.
#'
#' @details This function inherits the capabilities of
#'   [ggh4x::facet_grid2()].
#'
#'   Unlike `facet_grid()`, this function only automatically expands
#'   missing variables when they have no variables in that direction, to allow
#'   for unnested variables. It still requires at least one layer to have all
#'   faceting variables.
#'
#'   Hierarchies are inferred from the order of variables supplied to
#'   `rows` or `cols`. The first variable is interpreted to be the
#'   outermost variable, while the last variable is interpreted to be the
#'   innermost variable. They display order is always such that the outermost
#'   variable is placed the furthest away from the panels. For more information
#'   about the nesting of strips, please visit the documentation of
#'   [ggh4x::strip_nested()].
#'
#' @export
#'
#' @return A *FacetNested* ggproto object that can be added to a plot.
#' @family facetting functions
#' @seealso See [ggh4x::strip_nested()] for nested strips. See
#'   [ggplot2::facet_grid()] for descriptions of the original
#'   arguments. See [grid::unit()] for the construction of a
#'   `unit` vector.
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point()
#'
#' # Similar to `facet_grid2(..., strip = strip_nested())`
#' p + facet_nested(~ vs + cyl)
#'
#' # The nest line inherits from the global theme
#' p + facet_nested(~ cyl + vs, nest_line = element_line(colour = "red")) +
#'   theme(ggh4x.facet.nestline = element_line(linetype = 3))
facet_nested <- function(
  rows = NULL,
  cols = NULL,
  scales = "fixed",
  space  = "fixed",
  axes   = "margins",
  remove_labels = "none",
  independent = "none",
  shrink = TRUE,
  labeller = "label_value",
  as.table = TRUE,
  switch = NULL,
  drop = TRUE,
  margins = FALSE,
  nest_line = element_line(inherit.blank = TRUE),
  solo_line = FALSE,
  resect = unit(0, "mm"),
  render_empty = TRUE,
  strip = "nested",
  bleed = NULL
) {
  strip <- resolve_strip(strip)
  if (!is.null(bleed)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "facet_nested(bleed)",
      details = paste0("The `bleed` argument should be set in the ",
                       "`strip_nested()` function instead.")
    )
    strip$params$bleed <- isTRUE(bleed)
  }
  # Convert logical to elements for backward compatibility
  if (isTRUE(nest_line)) {
    nest_line <- element_line()
  }
  if (isFALSE(nest_line)) {
    nest_line <- element_blank()
  }
  if (!(is_theme_element(nest_line, "line") ||
        is_theme_element(nest_line, "blank"))) {
    cli::cli_abort(
      "The {.arg nest_line} argument must be {.cls element_blank} or inherit \\
      from {.cls element_line}."
    )
  }

  params <- list(
    nest_line = nest_line,
    solo_line = isTRUE(solo_line),
    resect = resect
  )

  new_grid_facets(
    rows, cols,
    scales, space, axes, remove_labels, independent,
    shrink, labeller, as.table, switch,
    drop, margins, render_empty, strip,
    params = params,
    super = FacetNested
  )
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
FacetNested <- ggproto(
  "FacetNested", FacetGrid2,
  map_data = function(data, layout, params) {
    # Handle empty data
    if (empty(data)) {
      return(cbind(data, PANEL = integer(0)))
    }
    # Setup variables
    rows <- params$rows
    cols <- params$cols
    vars <- c(names(rows), names(cols))

    if (length(vars) == 0) {
      data$PANEL <- layout$PANEL
      return(data)
    }

    margin_vars <- list(intersect(names(rows), names(data)),
                        intersect(names(cols), names(data)))

    # Add variables
    data <- reshape_add_margins(data, margin_vars, params$margins)
    facet_vals <- eval_facets(c(rows, cols), data, params$.possible_columns)

    # Only set as missing if it has no variable in that direction
    missing_facets <- character(0)
    if (!any(names(rows) %in% names(facet_vals))){
      missing_facets <- c(missing_facets,
                          setdiff(names(rows), names(facet_vals)))
    }
    if (!any(names(cols) %in% names(facet_vals))){
      missing_facets <- c(missing_facets,
                          setdiff(names(cols), names(facet_vals)))
    }

    # Fill in missing values
    if (length(missing_facets) > 0) {
      to_add <- unique(layout[missing_facets])
      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))
      data <- data[data_rep, , drop = FALSE]
      rownames(data) <- NULL
      facet_vals <- cbind(facet_vals[data_rep, , drop = FALSE],
                          to_add[facet_rep, , drop = FALSE])
      rownames(facet_vals) <- NULL
    }

    # Match columns to facets
    if (nrow(facet_vals) == 0) {
      data$PANEL <- -1
    } else {
      facet_vals[] <- lapply(facet_vals[], as.factor)
      facet_vals[] <- lapply(facet_vals[], addNA, ifany = TRUE)
      layout[]     <- lapply(layout[], as.factor)
      keys <- join_keys(facet_vals, layout,
                        by = vars[vars %in% names(facet_vals)])
      data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
    }
    data
  },
  vars_combine = function(
    data, env = emptyenv(), vars = NULL, drop = TRUE
  ) {
    if (length(vars) == 0) {
      return(new_data_frame())
    }

    possible_columns <- unique0(unlist(lapply(data, names)))

    values <- lapply(data, eval_facets, facets = vars,
                     possible_columns = possible_columns)
    values <- values[lengths(values) > 0]
    has_all <- unlist(lapply(values, length)) == length(vars)
    if (!any(has_all)) {
      missing <- lapply(values, function(x) setdiff(names(vars), names(x)))
      missing_vars <- paste0(
        c("Plot", paste0("Layer ", seq_len(length(data) - 1))),
        " is missing {.var ", missing[seq_along(data)], "}"
      )
      names(missing_vars) <- rep("x", length(data))

      cli::cli_abort(c(paste0(
        "At least one layer must contain all faceting variables: \\
        {.var {names(vars)}}"
      ), missing_vars))
    }
    base <- unique0(vec_rbind(!!!values[has_all]))
    if (!drop) {
      base <- unique_combs(base)
    }
    for (value in values[!has_all]) {
      if (empty(value))
        next
      old <- base[setdiff(names(base), names(value))]
      new <- unique(value[intersect(names(base), names(value))])
      if (drop) {
        new <- unique_combs(new)
      }
      # This is different than vanilla ggplot2
      old[setdiff(names(base), names(value))] <- rep("", nrow(old))
      base <- rbind(base, df.grid(old, new))
    }
    if (empty(base)) {
      cli::cli_abort("Facetting variables must have at least one value.")
    }
    base
  },

  finish_panels = function(self, panels, layout, params, theme) {
    add_nest_indicator(panels, params, theme)
  }
)

# Helper functions -----------------------------------------------

add_nest_indicator <- function(panels, params, theme) {
  # Convert nest line to proper element
  nest_line <- params$nest_line
  if (is.null(nest_line) || isFALSE(nest_line)) {
    return(panels)
  }
  nest_line <- inherit_element(
    nest_line, calc_element("ggh4x.facet.nestline", theme)
  )
  if (is_theme_element(nest_line, "blank")) {
    return(panels)
  }
  solo <- isTRUE(params$solo_line)

  # Find strips
  layout <- panels$layout
  layout$index <- seq_len(nrow(layout))
  is_strip <- grepl("^strip-", layout$name)
  layout   <- layout[is_strip, ]

  active <- unit(c(0, 1), "npc") + c(1, -1) * params$resect

  # browser()

  h_strip <- layout
  if (!solo) {
    h_strip <- h_strip[h_strip$l != h_strip$r,]
  } else {
    h_strip  <- h_strip[grepl("^strip-b|^strip-t", h_strip$name), ]
  }
  if (nrow(h_strip) > 0) {
    index <- h_strip$index
    is_secondary <- any(grepl("^strip-b", h_strip$name))
    passive <- rep(as.numeric(is_secondary), 2)
    # Draw the line
    indicator <- element_grob(nest_line, x = active, y = passive)

    if (solo) {
      pos <- if (is_secondary) function(x) 1L else nrow
      is_inner <- vapply(panels$grobs[index], function(gt) {
        gt$layout$t == pos(gt)
      }, logical(1))
      index <- index[!is_inner]
    }

    # Add the line to the strip-grob
    panels$grobs[index] <- lapply(
      panels$grobs[index],
      function(gt) {
        with(vec_slice(gt$layout, 1), gtable_add_grob(
          gt, indicator, t = t, l = l, r = r, b = b, z = z,
          name = "nester", clip = "off"
        ))
      }
    )
    # These offsets ensure that the strip-grob with the nest line is on top
    # of the lower-layer strip-grobs
    offset  <- vapply(panels$grobs[index], function(grob) {
      grob$layout[["t"]][1]
    }, numeric(1))
    if (!is_secondary) {
      nlevels <- dim(panels$grobs[[index[1]]])[1]
      offset <- nlevels - offset
    }
    panels$layout$z[index] <- panels$layout$z[index] + offset
  }

  v_strip <- layout
  if (!solo) {
    v_strip <- v_strip[v_strip$t != v_strip$b,]
  } else {
    v_strip <- v_strip[grepl("^strip-r|^strip-l", v_strip$name), ]
  }
  if (nrow(v_strip) > 0) {
    index <- v_strip$index
    is_secondary <- any(grepl("^strip-r", v_strip$name))
    passive <- rep(as.numeric(!is_secondary), 2)
    # Draw the line
    indicator <- element_grob(nest_line, x = passive, y = active)

    if (solo) {
      pos <- if (is_secondary) function(x) 1L else ncol
      is_inner <- vapply(panels$grobs[index], function(gt) {
        gt$layout$l == pos(gt)
      }, logical(1))
      index <- index[!is_inner]
    }

    # Add the line to the strip grob
    panels$grobs[index] <- lapply(
      panels$grobs[index],
      function(gt) {
        with(vec_slice(gt$layout, 1), gtable_add_grob(
          gt, indicator, t = t, l = l, r = r, b = b, z = z,
          name = "nester", clip = "off"
        ))
      }
    )
    # These offsets ensure that the strip-grob with the nest line is on top
    # of the lower-layer strip-grobs
    offset  <- vapply(panels$grobs[index], function(grob) {
      grob$layout[["l"]][1]
    }, numeric(1))
    if (!is_secondary) {
      nlevels <- dim(panels$grobs[[index[1]]])[2]
      offset <- nlevels - offset
    }
    panels$layout$z[index] <- panels$layout$z[index] + offset
  }

  return(panels)
}

