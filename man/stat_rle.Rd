% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_rle.R
\name{stat_rle}
\alias{stat_rle}
\title{Run length encoding}
\usage{
stat_rle(
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

\item{geom}{Use to override the default connection between
\code{geom_rect()} and \code{stat_rle()}.}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}'s \code{params} argument. These
arguments broadly fall into one of 4 categories below. Notably, further
arguments to the \code{position} argument, or aesthetics that are required
can \emph{not} be passed through \code{...}. Unknown arguments that are not part
of the 4 categories below are ignored.
\itemize{
\item Static aesthetics that are not mapped to a scale, but are at a fixed
value and apply to the layer as a whole. For example, \code{colour = "red"}
or \code{linewidth = 3}. The geom's documentation has an \strong{Aesthetics}
section that lists the available options. The 'required' aesthetics
cannot be passed on to the \code{params}. Please note that while passing
unmapped aesthetics as vectors is technically possible, the order and
required length is not guaranteed to be parallel to the input data.
\item When constructing a layer using
a \verb{stat_*()} function, the \code{...} argument can be used to pass on
parameters to the \code{geom} part of the layer. An example of this is
\code{stat_density(geom = "area", outline.type = "both")}. The geom's
documentation lists which parameters it can accept.
\item Inversely, when constructing a layer using a
\verb{geom_*()} function, the \code{...} argument can be used to pass on parameters
to the \code{stat} part of the layer. An example of this is
\code{geom_area(stat = "density", adjust = 0.5)}. The stat's documentation
lists which parameters it can accept.
\item The \code{key_glyph} argument of \code{\link[ggplot2:layer]{layer()}} may also be passed on through
\code{...}. This can be one of the functions described as
\link[ggplot2:draw_key]{key glyphs}, to change the display of the layer in the legend.
}}

\item{align}{A \code{character} of length one that effect the computed
\code{start} and \code{end} variables. One of the following:
\describe{
\item{\code{"none"}}{Take exact start and end \code{x} values.}
\item{\code{"center"}}{Return start and end \code{x} values in between an
end and the subsequent start.}
\item{\code{"start"}}{Align start values with previous end values.}
\item{\code{"end"}}{Align end values with next start values.}
}}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{orientation}{The orientation of the layer. The default (\code{NA})
automatically determines the orientation from the aesthetic mapping. In the
rare event that this fails it can be given explicitly by setting \code{orientation}
to either \code{"x"} or \code{"y"}. See the \emph{Orientation} section for more detail.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
A \code{ggplot2} layer
}
\description{
Run length encoding takes a vector of values and calculates the
lengths of consecutive repeated values.
}
\details{
The data is first ordered on the \code{x} aesthetic before run lengths are
calculated for the \code{label} aesthetic. In contrast to \code{base::rle()}, \code{NA}s
are considered equivalent values, not different values.
}
\section{Aesthetics}{
 \code{stat_rle()} understands the following
aesthetics (required aesthetics are in bold)
\itemize{
\item{\strong{x}}
\item{\strong{label}}
\item{group}
}
}

\section{Computed variables}{

\describe{
\item{start}{The \code{x} values at the start of every run.}
\item{end}{The \code{x} values at the end of every run.}
\item{start_id}{The index where a run starts.}
\item{end_id}{The index where a run ends.}
\item{run_id}{The index of a run.}
\item{runlength}{The length of a run.}
\item{runvalue}{The value associated with a run.}
}
}

\examples{
df <- data.frame(
  x = seq(0, 10, length.out = 100),
  y = sin(seq(0, 10, length.out = 100)*2)
)

# Label every run of increasing values
ggplot(df) +
  stat_rle(aes(x, label = diff(c(0, y)) > 0),
           align = "end") +
  geom_point(aes(x, y))

# Label every run above some threshold
ggplot(df) +
  stat_rle(aes(x, label = y > 0),
           align = "center") +
  geom_point(aes(x, y))

# Categorising runs, more complicated usage
ggplot(df) +
  stat_rle(aes(stage(x, after_stat = run_id),
               after_stat(runlength),
               label = cut(y, c(-1, -0.6, 0.6, 1)),
               fill = after_stat(runvalue)),
           geom = "col")
}
