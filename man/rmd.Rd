% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chunk.R
\name{rmd}
\alias{rmd}
\alias{rmd,Chunk-method}
\title{Chunk R Markdown}
\usage{
rmd(x)

\S4method{rmd}{Chunk}(x)
}
\arguments{
\item{x}{S4 object of class Chunk}
}
\description{
Retrieve the R Markdown text from an object of class Chunk.
}
\examples{
x <- chunk(1 + 1)
rmd(x)
}
