# Constructor -------------------------------------------------------------

#' @title Run length encoding
#'
#' @description Run length encoding takes a vector of values and calculates the
#' lengths of consecutive repeated values.
#'
#' @inheritParams ggplot2::stat_density
#' @param geom Use to override the default connection between
#'   `geom_rect()` and `stat_rle()`.
#' @param align A `character` of length one that effect the computed
#' `start` and `end` variables. One of the following:
#' \describe{
#'   \item{`"none"`}{Take exact start and end `x` values.}
#'   \item{`"center"`}{Return start and end `x` values in between an
#'   end and the subsequent start.}
#'   \item{`"start"`}{Align start values with previous end values.}
#'   \item{`"end"`}{Align end values with next start values.}
#' }
#'
#' @details
#' The data is first ordered on the `x` aesthetic before run lengths are
#' calculated for the `label` aesthetic. In contrast to `base::rle()`, `NA`s
#' are considered equivalent values, not different values.
#'
#' @section Aesthetics: `stat_rle()` understands the following
#'   aesthetics (required aesthetics are in bold)
#' \itemize{
#'   \item{**x**}
#'   \item{**label**}
#'   \item{group}
#' }
#'
#' @section Computed variables:
#' \describe{
#'   \item{start}{The `x` values at the start of every run.}
#'   \item{end}{The `x` values at the end of every run.}
#'   \item{start_id}{The index where a run starts.}
#'   \item{end_id}{The index where a run ends.}
#'   \item{run_id}{The index of a run.}
#'   \item{runlength}{The length of a run.}
#'   \item{runvalue}{The value associated with a run.}
#' }
#'
#' @return A `ggplot2` layer
#' @export
#' @name stat_rle
#'
#' @examples
#' df <- data.frame(
#'   x = seq(0, 10, length.out = 100),
#'   y = sin(seq(0, 10, length.out = 100)*2)
#' )
#'
#' # Label every run of increasing values
#' ggplot(df) +
#'   stat_rle(aes(x, label = diff(c(0, y)) > 0),
#'            align = "end") +
#'   geom_point(aes(x, y))
#'
#' # Label every run above some threshold
#' ggplot(df) +
#'   stat_rle(aes(x, label = y > 0),
#'            align = "center") +
#'   geom_point(aes(x, y))
#'
#' # Categorising runs, more complicated usage
#' ggplot(df) +
#'   stat_rle(aes(stage(x, after_stat = run_id),
#'                after_stat(runlength),
#'                label = cut(y, c(-1, -0.6, 0.6, 1)),
#'                fill = after_stat(runvalue)),
#'            geom = "col")
stat_rle <- function(
  mapping = NULL,
  data = NULL,
  geom = "rect",
  position = "identity",
  ...,
  align = "none",
  na.rm = FALSE,
  orientation = "x",
  show.legend = NA,
  inherit.aes = TRUE
) {
  align <- match.arg(align, c("none", "centre", "center", "start", "end"))
  if (align == "center") {
    align <- "centre"
  }
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatRle,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list2(
      na.rm       = na.rm,
      orientation = orientation,
      align       = align,
      ...
    )
  )
}

# ggproto class -----------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggh4x_extensions
StatRle <- ggproto(
  "StatRle",
  Stat,
  required_aes = c("x", "label"),
  default_aes = aes(
    xmin = after_stat(start),
    xmax = after_stat(end),
    ymin = after_stat(-Inf),
    ymax = after_stat(Inf),
    fill = after_stat(runvalue)
  ),
  dropped_aes = c("x", "label"),
  setup_params = function(data, params) {
    params$flipped_aes <- isTRUE(params$orientation == "y")
    params
  },
  extra_params = c("na.rm", "orientation", "align"),
  compute_group = function(data, flipped_aes = FALSE, align, scales) {
    data <- data[order(data$x), ]
    n <- nrow(data)

    run <- vec_unrep(data$label)

    start_id <- {end_id <- cumsum(run$times)} - run$times + 1

    if (align == "centre") {
      start <- (data$x[pmax(start_id, 1L)] + data$x[pmax(start_id - 1, 1L)]) / 2
      end   <- (data$x[pmin(end_id,   n)]  + data$x[pmin(end_id   + 1, n)])  / 2
    } else if (align == "end") {
      start <- data$x[pmax(start_id - 1, 1L)]
      end   <- data$x[end_id]
    } else if (align == "start") {
      start <- data$x[start_id]
      end   <- data$x[pmin(end_id + 1, n)]
    } else {
      start <- data$x[start_id]
      end   <- data$x[end_id]
    }

    data_frame0(
      start     = start,
      end       = end,
      start_id  = start_id,
      end_id    = end_id,
      run_id    = seq_along(run$key),
      runlength = run$times,
      runvalue  = run$key
    )
  }
)
