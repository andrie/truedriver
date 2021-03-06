% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/truedriver.R
\name{plot.truedriver}
\alias{plot.truedriver}
\title{Plots true driver graph.}
\usage{
\method{plot}{truedriver}(x, GraphParameters = as.GraphParameter(),
  meansFunction = c("median", "mean"), means = NULL, ...)
}
\arguments{
\item{x}{A truedriver object}

\item{GraphParameters}{A list with graphics parameters.  See also \code{\link{as.GraphParameter}}}

\item{meansFunction}{Vector of length 2, containing functions to use for calculating midpoints of importance and satisfaction. Defaults to median and mean.}

\item{means}{Numeric vector of length 2. First element is mean of importance, second element is mean of satisfaction. If null, defaults to calculated means of data}

\item{...}{passed to ggplot opts}
}
\value{
A ggplot object
}
\description{
Plots true driver graph.
}
\seealso{
\code{\link{truedriver}}
}
