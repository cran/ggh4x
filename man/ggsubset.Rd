% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conveniences.R
\name{ggsubset}
\alias{ggsubset}
\title{Passing a subset of data to ggplot2 layers.}
\usage{
ggsubset(rowtest = NULL, omit = NULL)
}
\arguments{
\item{rowtest}{logical \code{expression} indicating which rows to keep.}

\item{omit}{a \code{character} column name to exclude.}
}
\value{
A function that takes a \code{data.frame} as argument and returns a
subset of that \code{data.frame} according to \code{rowtest}
}
\description{
This is a convenience function to allow layer objects, such as geoms, to take
a subset of the data in the main \code{ggplot()} call, without storing a
duplicate of the subset in the ggplot object.
}
\details{
\code{ggsubset} is a wrapper around \code{subset.data.frame} where
the \code{subset} argument is set to \code{rowtest} and the \code{select}
argument to \code{-omit}. Since the \code{data} argument in the
\code{layer()} function can take a function with one argument, we can pass
the function returned from \code{ggsubset} as that argument to subset the
data by rows.
}
\examples{
ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point(data = ggsubset(Species == "setosa"))
}
\seealso{
See \code{\link[ggplot2:layer]{ggplot2::layer()}}, specifically the \code{data}
argument. See \code{\link[base:subset]{subset.data.frame()}} for the internal
function.
}
