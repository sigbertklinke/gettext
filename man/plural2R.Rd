% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plural2R.R
\name{plural2R}
\alias{plural2R}
\title{plural2R}
\usage{
plural2R(ctxt)
}
\arguments{
\item{ctxt}{character: one string with a plural form}
}
\value{
list with the input text, nplurals value and a R function plural(n)
}
\description{
Converts a plural from from text to list with the input text, nplurals value and a R function 
\code{plural(n)} which computes the plural index from \code{n}. 
\itemize{
\item Note 1: The index is increased by one since R starts with x[1] and C with x[0]
\item Note 2: Only a subset of C symbols are supported based on the test cases: 
\code{(}, \code{)}, \code{\%}, \code{<},  \code{>}, \code{>=}, \code{<=}, \code{&&}, \code{||}, \code{?:}, \code{=}, and \code{;}.
}
}
\examples{
# Belarusian plural form
p <- paste0('nplurals=3;',
     'plural=(n\%10==1 && n\%100!=11 ? 0 : n\%10>=2 && n\%10<=4 && (n\%100<10 || n\%100>=20) ? 1 : 2);')
l <- plural2R(p)
l$nplurals
l$plural(0:100)
}
