#' dsub
#'
#' Performs dollar substitution in a text. The abbreviated form is `S`.
#'
#' @param txt character: text to replace in
#' @param ... named parameter or list with pattern and replacement
#'
#' @return replaced text
#' @export
#'
#' @examples
#' dsub('$a EUR', a=3)
#' dsub('$a EUR', list(a=3))
dsub <- function (txt, ...) {
  args <- list(...)
  rep  <- list()
  if (length(args)) {
    nargs <- names(args)  
    if (is.null(nargs)) nargs <- rep('', length(args))
    for (i in 1:length(nargs)) {
      if (nargs[i]=='') {
        if (is.list(args[[i]])) {
          for (argi in names(args[[i]])) rep[[argi]] <- toString(args[[i]][[argi]])
        }
      } else {
        rep[[nargs[i]]] <- toString(args[[i]])
      }
    }
  }
  nargs <- names(rep)
  for (i in order(nchar(nargs), decreasing=TRUE)) {
    txt <- gsub(paste0('$', nargs[i]), rep[[i]], txt, fixed=TRUE)
  }
  txt
}

#' @rdname dsub
#' @export
S <- function (txt, ...) { dsub(txt, ...) }