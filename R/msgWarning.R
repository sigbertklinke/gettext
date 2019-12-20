#' msgWarning
#'
#' Appends a warning to the library-internal translation environment.
#'
#' @param msg character: message
#' @param param chracter: character vector with additional informations or \code{NULL}
#' @param file character: file name or \code{NA}
#' @param line integer: line number or \code{NA}
#'
#' @return  nothing
#' @export
#'
#' @examples
#' msgWarning('duplicate entry')
msgWarning <- function(msg, param=NULL, file=NA, line=NA) {
  warn <- list(paste(ifelse(is.na(file), '', file), msg))
  if (!is.null(param)) {
    if (is.numeric(line))   warn[[2]] <- paste(ifelse(is.na(line), '      ', sprintf("%5.0f:", line)), param)
    if (is.character(line)) warn[[2]] <- paste(ifelse(is.na(line), '  ', sprintf("%30s:",  line)), param)
  }
  translation$warning[[1+length(translation$warning)]] <- warn
}