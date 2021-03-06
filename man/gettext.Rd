% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gettext.R
\name{gettext}
\alias{gettext}
\alias{G}
\title{gettext}
\usage{
gettext(
  id1,
  lang = getOption("gettext.lang"),
  domain = getOption("gettext.domain"),
  context = NA
)

G(
  id1,
  lang = getOption("gettext.lang"),
  domain = getOption("gettext.domain"),
  context = NA
)
}
\arguments{
\item{id1}{character: untranslated message(s)}

\item{lang}{character: language code (default: \code{getOption('gettext.lang')}, usually \code{"en"})}

\item{domain}{character: text domain (default: \code{getOption('gettext.domain')}, usually \code{NA})}

\item{context}{character: context (default: \code{NA})}
}
\value{
the translated string(s)
}
\description{
Returns a translated string from the untranslated string \code{id1}. An abbreviated form is \code{G} since 
R does not support \code{_} as unary operator.
}
\examples{
gettext("I saw her duck") # no translation at all
G("I saw her duck") 
}
