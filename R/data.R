#' ISO 639 language codes
#'
#' A data frame with two- and three letter codes from ISO 639-1 and ISO 693-2. 
#' Codes are taken from alpha-3 bibliographic, alpha-3 terminologic and alpha-2 code.
#' 
#' @docType data
#'
#' @usage data(iso639)
#'
#' @format A data frame with 1210 language codes in three variables
#' \itemize{
#' \item \code{code} a two- or three letter code 
#' \item \code{english} english name of the language
#' \item \code{french} french name of the language
#' }
#'
#' @keywords datasets
#'
#' @source \href{https://datahub.io/core/language-codes}{ISO Language Codes (639-1 and 693-2) and IETF Language Types}, 
#' file \href{https://datahub.io/core/language-codes/r/language-codes-full.csv}{language-codes-full.csv} 
#'
#' @examples
#' data(iso639)
#' head(iso639)
"iso639"

#' Plural forms
#'
#' A list of plural forms used in PO files.
#' 
#' @docType data
#'
#' @usage data(plural)
#'
#' @format A list with 144 plural forms used in PO files 
#' \itemize{
#' \item \code{nplurals} the number of plural forms
#' \item \code{plural} an R function which computes from \code{n} the index of plural form
#' \item \code{lang} ISO 639 language code. Note that for the same language might be several plural forms possible
#' \item \code{form} the orignal plural form as string
#' }
#'
#' @keywords datasets
#'
#' @source The plural forms are taken from the table in \href{http://docs.translatehouse.org/projects/localization-guide/en/latest/l10n/pluralforms.html}{Localization Guide 0.9.0 - Plural Forms}, 
#' 
#' @examples
#' data(plural)
#' plural[[1]]
#' lang <- sapply(plural, function(v) { v$lang })
#' # extract arabic
#' ar   <- which(lang=='ar')
#' plural[[ar]]
#' # test of plural form
#' plural[[ar]]$plural(0:100)
"plural"