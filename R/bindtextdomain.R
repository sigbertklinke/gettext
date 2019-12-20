#' bindtextdomain
#'
#' Sets the path for a domain.
#'
#' @param domain character: text domain (default: \code{getOption('gettext.domain')}, usually \code{NA})
#' @param dir character: directory for the domain PO files
#'
#' @return a data frame with: \code{lang}, the assigned language, \code{domain}, the assigned domain, \code{iso639}, the identified language, and \code{file}, the filename
#' @export
#'
#' @examples
#' bindtextdomain('app2', system.file('shiny', 'app2', package='gettext'))
bindtextdomain <- function (domain=getOption('gettext.domain'), dir) {
  if (length(domain)>1) {
    if (length(domain)!=length(dir)) stop("Different length of 'domain' and 'dir'")
  } else {
    domain <- rep(domain, length(dir))
  }
  l <- NULL
  for (i in 1:length(dir)) {
    files <- list.files(path=dir[i], pattern="*.po$", full.names = TRUE)
    li    <- bind(files)
    li$domain <- rep(domain, nrow(li))
    l <- rbind(l, li)
  }
  l
}