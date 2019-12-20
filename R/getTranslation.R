#' getTranslation 
#'
#' Returns a copy of the internal translation environment or a list of \code{str}'s of each object. 
#'
#' @param list logical: return list or list of \code{str}'s (default: \code{TRUE})
#'
#' @return list of strings
#' @importFrom utils capture.output str
#' @export
#'
#' @examples
#' getTranslation()
getTranslation <- function(list=TRUE) {
  if (list) return(as.list(translation))
  for (obj in ls(translation)) {
    ret[[obj]] <- capture.output(str(translation[[obj]]))
  }
  ret
}