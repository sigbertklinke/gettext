#' addPlural
#'
#' Adds a plural definition to internal translation object. It it already exists it will be overwritten.
#'
#' @param nplurals integer: number of plural forms (default: \code{1})
#' @param plural function: a function that computes the plural index from \code{n} (default: \code{function(n) {0}})
#' @param text character: a string from a PO file
#' @param lang character: language to translate to (default: \code{getOption('gettext.lang')}, usually \code{"en"})
#' @param domain character: text domain (default: \code{getOption('gettext.domain')}, usually \code{NA})
#' @param file character: source file (default: \code{NA})
#' @param line integer: line number in source file (default: \code{NA})
#'
#' @return invisibly the plural form stored
#' @export
#'
#' @examples
#' # Czech
#' pl <- addPlural(text='nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;')
#' print(pl)
addPlural <- function(nplurals=1, plural=function(n){0}, text=NULL, lang=getOption('gettext.lang'), domain=getOption('gettext.domain'), file=NA, line=NA) {
  if (!is.null(text)) {
    pl <- plural2R(text)
    nplurals <- pl$nplurals
    plural   <- pl$plural
  }
#  if ((nplurals<0) || (is.null(plural))) stop("invalid 'nplurals' or 'plural'")
  # lang,domain
  key <- makeKey(lang, domain) 
  if (!is.null(translation[[key]])) msgWarning('duplicate plural, overwritten', file=file, line=line)
  l <- list(nplurals=nplurals, plural=plural)
  translation[[key]] <- l
  invisible(l)
}