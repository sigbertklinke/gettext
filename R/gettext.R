#' gettext
#' 
#' Returns a translated string from the untranslated string \code{id1}. An abbreviated form is \code{G} since 
#' R does not support \code{_} as unary operator.
#'
#' @param id1 character: untranslated message 
#' @param lang character: language code (default: \code{getOption('gettext.lang')}, usually \code{"en"})
#' @param domain character: text domain (default: \code{getOption('gettext.domain')}, usually \code{NA})
#' @param context character: context (default: \code{NA})
#' 
#' @return the translated string
#' @export 
#'
#' @examples
#' gettext("I saw her duck") # no translation at all
#' G("I saw her duck") 
gettext <- function(id1, lang=getOption('gettext.lang'), domain=getOption('gettext.domain'), context=NA) {
  key <- makeKey(id1, NA, lang, domain, context)
  if (is.null(translation[[key]])) return(id1)
  translation[[key]][1]
}

#' @rdname gettext
#' @export
G <- function (id1, lang=getOption('gettext.lang'), domain=getOption('gettext.domain'), context=NA) {
  gettext(id1, lang, domain, context)
}