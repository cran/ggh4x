% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_facet.R
\name{scale_facet}
\alias{scale_facet}
\alias{scale_x_facet}
\alias{scale_y_facet}
\title{Position scales for individual panels in facets}
\usage{
scale_x_facet(expr, ..., type = "continuous")

scale_y_facet(expr, ..., type = "continuous")
}
\arguments{
\item{expr}{An \code{expression} that, when evaluated in the facet's layout
data.frame, yields a \code{logical} vector. See details.}

\item{...}{Other arguments passed to the scale.}

\item{type}{A \code{character(1)} indicating the type of scale, such that
\verb{scale_(x/y)_\{type\}} spells a scale function. Defaults to \code{"continuous"}.}
}
\value{
A \code{scale_facet} object that can be added to a plot.
}
\description{
This function adds position scales (x and y) of individual panels. These
can be used to fine-tune limits, breaks and other scale parameters for
individual panels, provided the facet allows free scales.
}
\details{
These scale functions work through the mechanism of the
\code{\link[=facetted_pos_scales]{facetted_pos_scales()}} function, and the same limitations apply: scale
transformations are applied after \code{stat} transformations, and the \code{oob}
argument of scales is ignored.

For the \code{expr} argument, the expression will be evaluated in the context
of the plot's layout. This is an internal \code{data.frame} structure that
isn't normally exposed, so it requires some extra knowledge. For most
facets, the layout describes the panels, with one panel per row. It
typically has \code{COL}, \code{ROW} and \code{PANEL} columns that keep track of what
panel goes where in a grid of cells. In addition, it contains the
facetting variables provided to the \code{facets} or \code{rows} and \code{cols} arguments
of the facets. For example, if we have a plot facetted on the \code{var}
variable with the levels \code{A}, \code{B} and \code{C}, as 1 row and 3 columns, we might
target the second \code{B} panel with any of these expressions: \code{var == "B"},
\code{PANEL == 2} or \code{COL == 2}. We can inspect the layout structure by using
\code{ggplot_build(p)$layout$layout}, wherein \code{p} is a plot.

When using multiple \verb{scale_(x/y)_facet()}, the \code{expr} argument can target
the same panels. In such case, the scales added to the plot first overrule
the scales that were added later.
}
\examples{
# A standard plot with continuous scales
p <- ggplot(mtcars, aes(disp, mpg)) +
  geom_point() +
  facet_wrap(~ cyl, scales = "free")

# Adding a scale for a value for a facetting variable
p + scale_x_facet(cyl == 8, limits = c(200, 600))

# Adding a scale by position in the layout
p + scale_x_facet(COL == 3, limits = c(200, 600))

# Setting the default scale and making an exception for one panel
p + scale_y_continuous(limits = c(0, 40)) +
  scale_y_facet(PANEL == 1, limits = c(10, 50))

# Using multiple panel-specific scales
p + scale_y_facet(PANEL == 1, limits = c(10, 50)) +
  scale_y_facet(cyl == 6, breaks = scales::breaks_width(0.5))

# When multiple scales target the same panel, the scale added first gets
# priority over scales added later.
p + scale_y_facet(COL == 2, limits = c(10, 40)) +
  scale_y_facet(cyl \%in\% c(4, 6), breaks = scales::breaks_width(1))

# A standard plot with discrete x scales
p <- ggplot(mtcars, aes(factor(cyl), mpg)) +
  geom_boxplot() +
  facet_wrap(~ vs, scales = "free")

# Expanding limits to show every level
p + scale_x_facet(vs == 1, limits = factor(c(4, 6, 8)), type = "discrete")

# Shrinking limits to hide a level
p + scale_x_facet(vs == 0, limits = factor(c(4, 6)), type = "discrete")
}
\seealso{
The \code{\link[=facetted_pos_scales]{facetted_pos_scales()}} function.
}
