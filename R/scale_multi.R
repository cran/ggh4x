# Main functions ----------------------------------------------------------

#' Multiple gradient colour scales
#'
#' @description Maps multiple aesthetics to multiple colour fill gradient
#'   scales. It takes in listed arguments for each aesthetic and disseminates
#'   these to [ggplot2::continuous_scale()].
#'
#' @param ...,colours,values,na.value,guide,colors listed arguments in
#'   [`scale_colour_gradientn()`][ggplot2::scale_colour_gradient] (e.g. `colours =
#'   list(c("white", "red"), c("black", "blue"))`).
#' @param aesthetics a `character` vector with names of aesthetic mapping.
#'
#' @details This function should only be called after all layers that this
#'   function affects are added to the plot.
#'
#'   The list elements of the listed arguments are assumed to follow the
#'   `aesthetics` order, i.e. the n*th* list element belongs to the n*th*
#'   aesthetic. When there are more list elements than n aesthetics, only the
#'   first n*th* list elements are taken. When there are more `aesthetics`
#'   than list elements, the first list element is used for the remaining
#'   aesthethics.
#'
#'   In contrast to other `scale_*_continous`-family functions, the
#'   `guide` argument is interpreted before adding it to the plot instead
#'   of at the time of plot building. This behaviour ensures that the
#'   `available_aes` argument of the guides are set correctly, but may
#'   interfere with the [ggplot2::guides()] function.
#'
#' @return A nested list-like structure of the class `MultiScale`.
#'
#' @export
#'
#' @examples
#' # Setup dummy data
#' df <- rbind(data.frame(x = 1:3, y = 1, v = NA, w = 1:3, z = NA),
#'             data.frame(x = 1:3, y = 2, v = 1:3, w = NA, z = NA),
#'             data.frame(x = 1:3, y = 3, v = NA, w = NA, z = 1:3))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_raster(aes(fill1 = v)) +
#'   geom_raster(aes(fill2 = w)) +
#'   geom_raster(aes(fill3 = z)) +
#'   scale_fill_multi(aesthetics = c("fill1", "fill2", "fill3"),
#'                    colours = list(c("white", "red"),
#'                                   c("black", "blue"),
#'                                   c("grey50", "green")))
scale_fill_multi <- function(
  ..., colours, values = NULL, na.value = "transparent",
  guide = "colourbar", aesthetics = "fill", colors
) {

  # Convert to proper spelling of colour
  colours <- if (missing(colours)){
    if (missing(colors)){
      c("white", "black")
    } else {
      colors
    }
  } else {
    colours
  }

  # Distribute arguments across multiple scales
  scales <- distribute_scale_multi(aesthetics = aesthetics,
                                   colours    = colours,
                                   values     = values,
                                   na.value   = na.value,
                                   guide      = guide,
                                   ...)

  structure(list(scales = scales,
                 aes = aesthetics,
                 replaced_aes = standardise_aes_names("fill")),
            class = "MultiScale")
}

#' @rdname  scale_fill_multi
#' @export
scale_colour_multi <- function(
  ..., colours, values = NULL, na.value = "transparent",
  guide = "colourbar", aesthetics = "colour", colors
) {

  # Convert to proper spelling of colour
  colours <- if (missing(colours)){
    if (missing(colors)){
      c("white", "black")
    } else {
      colors
    }
  } else {
    colours
  }

  # Distribute arguments across multiple scales
  scales <- distribute_scale_multi(
    aesthetics = aesthetics,
    colours    = colours,
    values     = values,
    na.value   = na.value,
    guide      = guide,
    ...
  )

  structure(list(scales = scales,
                 aes = aesthetics,
                 replaced_aes = standardise_aes_names("colour")),
            class = "MultiScale")
}


# Helpers -----------------------------------------------------------------

distribute_scale_multi <- function(
  aesthetics, colours, values, na.value, guide, ...
) {
  # Interpret extra arguments
  extra_args <- lapply(seq(aesthetics), function(i){
    lapply(list(...), pickvalue, i)
  })
  if (new_guide_system) {
    extra_args <- lapply(extra_args, function(x) {
      names(x)[names(x) == "trans"] <- "transform"
      x
    })
  }

  # Interpret guides
  guide <- lapply(seq(aesthetics), function(i){
    this_guide <- pickvalue(guide, i)
    if (all(class(this_guide) == "character") && length(this_guide) == 1) {
      if (standardise_aes_names(this_guide) ==
          standardise_aes_names("colourbar")) {
        this_guide <- guide_colourbar()
      } else if (this_guide == "legend") {
        this_guide <- guide_legend()
      }
    }
    if (inherits(this_guide, "Guide")) {
      old <- this_guide
      this_guide <- ggproto(NULL, old, available_aes = aesthetics[[i]])
    } else if (inherits(this_guide, "guide")) {
      this_guide$available_aes = aesthetics[[i]]
    } else {
      cli::cli_abort(c(
        "{.pkg ggh4x}'s author hasn't programmed this path yet.",
        i = "Choose a legend or colourbar guide."
      ))
    }
    return(this_guide)
  })

  # Interpret scales
  scales <- lapply(seq(aesthetics), function(i){
    aes <- aesthetics[[i]]
    pass_args <- list(
      aesthetics = aes,
      scale_name = paste0("MultiScale_", aes),
      palette = scales::gradient_n_pal(colours = pickvalue(colours, i),
                                       values  = pickvalue(values,  i)),
      na.value = pickvalue(na.value, i),
      guide    = pickvalue(guide,    i)
    )
    if (new_guide_system) pass_args$scale_name <- NULL
    pass_args <- append(pass_args, pickvalue(extra_args, i))
    out <- do.call("continuous_scale", pass_args)
    return(out)
  })
  return(scales)
}


pickvalue <- function(x, i){
  if (class(x)[[1]] != "list"){
    return(x)
  } else {
    i <- if (i > length(x))
      1
    else i
    return(x[[i]])
  }
}
