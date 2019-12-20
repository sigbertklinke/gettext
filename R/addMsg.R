#' makeKey
#'
#' @param ... list of strings
#'
#' @return a unique key 
#' @importFrom rjson toJSON
#' @export
#'
#' @examples
#' makeKey("test", 'de', '', NA)
#' makeKey('de', '')
makeKey <- function(...) {
  args <- list(...)
  res  <- vector('list' , length(args))
  for (i in 1:length(args)) {
    if (length(args[[i]])!=1) stop("length(arg)!=1")
    if (is.character(args[[i]])) res[[i]] <- args[[i]]
    if (is.na(args[[i]])) res[[i]] <- 0
  }
  toJSON(res)
}

#' addMsg
#'
#' Adds a message and its translation(s) to the library-internal translation environment, see the 
#' \href{https://www.gnu.org/software/gettext/manual/gettext.html#PO-Files}{GNU gettext manual}.
#'
#' @param id character: message id, see \code{msgid} 
#' @param str character: a vector with translations, see \code{msgstr} or \code{msgstr[N]}
#' @param plural character: plural form of a message, see  \code{msgid_plural} (default: \code{NULL})
#' @param context character: context, see \code{msgctxt} (default: \code{NA})
#' @param lang character: language code (default: \code{getOption('gettext.lang')}, usually \code{"en"})
#' @param domain character: text domain (default: \code{getOption('gettext.domain')}, usually \code{NA})
#' @param file character: source file (default: \code{NA})
#' @param line integer: line number in source file (default: \code{NA})
#'
#' @return invisibly the list added to the library-internal translation environment
#' @export
#'
#' @examples
#' options('gettext.lang'="de") # set the current language to 'de'
#' addMsg("Unknown system error", "Unbekannter Systemfehler")
#' addMsg("Unknown system error", "Error desconegut del sistema", lang="es")
#' addMsg("found %d fatal error", plural="found %d fatal errors", 
#'        str=c("%d Fehler gefunden", "%d Fehler gefunden"))
#' addMsg("We saw her duck", "Wir haben ihre Ente gesehen", context="duck as noun")
#' addMsg("We saw her duck", "Wir haben gesehen, wie sie sich duckte", context="duck as verb")
addMsg <- function(id, str, plural=NA, context=NA, 
                   lang=getOption('gettext.lang'), domain=getOption('gettext.domain'), 
                   file=NA, line=NA) {
  if (is.null(lang) || is.na(lang)) stop("set language with: options('gettext.lang'='language_code')")
  if (is.null(domain) || is.na(domain)) stop("set language with: options('gettext.domain'='mydomain')")
  # id,plural,lang,domain,context
  key <- makeKey(id, plural, lang, domain, context)
  if (!is.null(translation[[key]])) msgWarning('duplicate entry, overwritten', shQuote(id, "cmd"), file=file, line=line)
  translation[[key]] <- str
}