% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_pointpath.R
\name{geom_pointpath}
\alias{geom_pointpath}
\title{Point Paths}
\usage{
geom_pointpath(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  arrow = NULL,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{arrow}{Arrow specification as created by
\code{\link[grid:arrow]{grid::arrow()}}.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A \emph{Layer} ggproto object.
}
\description{
The point path geom is used to make a scatterplot wherein the points are
connected with lines in some order. This geom intends to mimic the
\code{type = 'b'} style of base R line plots.
}
\details{
The \code{mult} is a numeric value to
scale the proportion of gaps in the line around points.

While the need for this geom is not very apparent, since it can be
approximated in a variety of ways, the trick up its sleeve is that it
dynamically adapts the inter-point segments so these  don't deform under
different aspect ratios or device sizes.
}
\section{Aesthetics}{
 \code{geom_pointpath()} understands the following
aesthetics (required aesthetics are in bold):
\itemize{\item{\strong{\code{x}}} \item{\strong{\code{y}}}
\item{\code{alpha}} \item{\code{colour}} \item{\code{group}}
\item{\code{shape}} \item{\code{size}} \item{\code{stroke}}
\item{\code{linewidth}} \item{\code{linetype}} \item{\code{mult}}}
}

\examples{
ggplot(pressure, aes(temperature, pressure)) +
  geom_pointpath()

# Using geom_pointpath as annotation
ggplot() +
  annotate(
    "pointpath",
    x = c(1, 0.32, 0.31, -0.12, -0.81, -0.4, -0.81, -0.12, 0.31, 0.32, 1),
    y = c(0, 0.24, 0.95, 0.38, 0.59, 0, -0.59, -0.38, -0.95, -0.24, 0)
  )
}
