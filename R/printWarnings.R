#' printWarnings
#'
#' Prints all warning messages for the translation object. 
#'
#' @return Invisibly the warnings as a character vector
#' @importFrom crayon red
#' @export
#'
#' @examples
#' printWarnings()
printWarnings <- function() {
  warn <- c()
  if (is.null(translation$warning) || (length(translation$warning)==0)) return(warn)
  lastmsg <-''
  for (i in 1:length(translation$warning)) {
    if (translation$warning[[i]][[1]]!=lastmsg) {
      lastmsg <- translation$warning[[i]][[1]]
      warn    <- c(warn, lastmsg) 
    }
    if (length(translation$warning[[i]])>1) warn <- c(warn, translation$warning[[i]][[2]]) 
  }
  cat(red(paste0(warn, "\n")))
  invisible(warn)
}