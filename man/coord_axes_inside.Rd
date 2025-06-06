% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coord_axes_inside.R
\name{coord_axes_inside}
\alias{coord_axes_inside}
\title{Cartesian coordinates with interior axes}
\usage{
coord_axes_inside(
  xlim = NULL,
  ylim = NULL,
  xintercept = 0,
  yintercept = 0,
  labels_inside = FALSE,
  ratio = NULL,
  expand = TRUE,
  default = FALSE,
  clip = "on"
)
}
\arguments{
\item{xlim, ylim}{Limits for the x and y axes.}

\item{xintercept, yintercept}{A \code{numeric(1)} for the positions where the
orthogonal axes should be placed. If these are outside the bounds of the
limits, the axes are placed to the nearest extreme.}

\item{labels_inside}{One of \code{"x"}, \code{"y"}, \code{"both"} or \code{"none"} specifying
the axes where labels should be placed inside the panel along the axes.
\code{TRUE} is translated as \code{"both"} and \code{FALSE} (default) is translated as
\code{"none"}.}

\item{ratio}{Either \code{NULL}, or a \code{numeric(1)} for a fixed aspect ratio,
expressed as \code{y / x}.}

\item{expand}{If \code{TRUE}, the default, adds a small expansion factor to
the limits to ensure that data and axes don't overlap. If \code{FALSE},
limits are taken exactly from the data or \code{xlim}/\code{ylim}.}

\item{default}{Is this the default coordinate system? If \code{FALSE} (the default),
then replacing this coordinate system with another one creates a message alerting
the user that the coordinate system is being replaced. If \code{TRUE}, that warning
is suppressed.}

\item{clip}{Should drawing be clipped to the extent of the plot panel? A
setting of \code{"on"} (the default) means yes, and a setting of \code{"off"}
means no. In most cases, the default of \code{"on"} should not be changed,
as setting \code{clip = "off"} can cause unexpected results. It allows
drawing of data points anywhere on the plot, including in the plot margins. If
limits are set via \code{xlim} and \code{ylim} and some data points fall outside those
limits, then those data points may show up in places such as the axes, the
legend, the plot title, or the plot margins.}
}
\value{
A \code{CoordAxesInside} object, which can be added to a plot.
}
\description{
This coordinate system places the plot axes at interior positions. Other
than this, it behaves like \code{\link[ggplot2:coord_cartesian]{coord_cartesian()}} or
\code{\link[ggplot2:coord_fixed]{coord_fixed()}} (the latter if the \code{ratio} argument
is set).
}
\examples{
# A standard plot
p <- ggplot(mpg, aes(scale(displ), scale(hwy))) +
  geom_point() +
  theme(axis.line = element_line())

# By default, axis text is still placed outside the panel
p + coord_axes_inside()

# However, this can simply be changed
p + coord_axes_inside(labels_inside = TRUE)

# The place where the axes meet can be changed
p + coord_axes_inside(xintercept = 1, yintercept = -1)

# Axes snap to the nearest limit when out-of-bounds
p + coord_axes_inside(xintercept = -5, yintercept = Inf, clip = "off")

# Can be combined with other non-default axes
p + guides(x = guide_axis(minor.ticks = TRUE)) +
  coord_axes_inside()
}
