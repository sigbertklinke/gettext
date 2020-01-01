#' ngettext
#' 
#' Returns a translated string from the untranslated string \code{id2}. An abbreviated form is \code{N} since 
#' R does not support \code{_} as unary operator.
#'
#' @param id1 character: untranslated singular message(s) 
#' @param id2 character: untranslated plural message(s)
#' @param n integer: the number (e.g. item count) to determine the translation for the respective grammatical number
#' @param lang character: language code (default: \code{getOption('gettext.lang')}, usually \code{"en"})
#' @param domain character: text domain (default: \code{getOption('gettext.domain')}, usually \code{NA})
#' @param context character: context (default: \code{NA})
#' 
#' @return the translated string(s)
#' @export 
#'
#' @examples
#' ngettext("I saw one duck", "I saw %.0f ducks", 1)
#' sprintf(ngettext("I saw %0.f duck", "I saw %.0f ducks", 3), 3) # no translation at all
#' sprintf(N("I saw a duck", "I saw %.0f ducks", 3), 3)
ngettext <- function (id1, id2, n, lang=getOption('gettext.lang'), domain=getOption('gettext.domain'), context=NA) {
  id  <- cbind(as.character(id1), as.character(id2))
  n   <- rep(n, nrow(id))
  ret <- ifelse(n[1:nrow(id)]==1, id[,1], id[,2])
  #
  keypl <- makeKey(lang, domain)
  if (nrow(id) && !is.null(translation[[keypl]])) {
    for (i in 1:nrow(id)) {
      keyid <- makeKey(id[i,1], id[i,2], lang, domain, context)
      if (!is.null(translation[[keyid]])) ret[i] <-  translation[[keyid]][translation[[keypl]]$plural(n)]
    }
  }
  ret
} 

#' @rdname ngettext
#' @export
N <- function (id1, id2, n, lang=getOption('gettext.lang'), domain=getOption('gettext.domain'), context=NA) {
  ngettext(id1, id2, lang, domain, context)
}