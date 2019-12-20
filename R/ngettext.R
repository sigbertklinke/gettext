#' ngettext
#' 
#' Returns a translated string from the untranslated string \code{id2}. An abbreviated form is \code{N} since 
#' R does not support \code{_} as unary operator.
#'
#' @param id1 character: untranslated singular message 
#' @param id2 character: untranslated plural message 
#' @param n integer: the number (e.g. item count) to determine the translation for the respective grammatical number
#' @param lang character: language code (default: \code{getOption('gettext.lang')}, usually \code{"en"})
#' @param domain character: text domain (default: \code{getOption('gettext.domain')}, usually \code{NA})
#' @param context character: context (default: \code{NA})
#' 
#' @return the translated string
#' @export 
#'
#' @examples
#' ngettext("I saw one duck", "I saw %.0f ducks", 1)
#' sprintf(ngettext("I saw %0.f duck", "I saw %.0f ducks", 3), 3) # no translation at all
#' sprintf(N("I saw a duck", "I saw %.0f ducks", 3), 3)
ngettext <- function (id1, id2, n, lang=getOption('gettext.lang'), domain=getOption('gettext.domain'), context=NA) {
  keyid <- makeKey(id1, id2, lang, domain, context)
  keypl <- makeKey(lang, domain)
  if (is.null(translation[[keyid]]) || is.null(translation[[keypl]])) {
    if (n==1) return(id1)
    return(id2)
  }
  translation[[keyid]][translation[[keypl]]$plural(n)]
} 

#' @rdname ngettext
#' @export
N <- function (id1, id2, n, lang=getOption('gettext.lang'), domain=getOption('gettext.domain'), context=NA) {
  ngettext(id1, id2, lang, domain, context)
}