#' xgettext
#'
#' Extracts all text constants from R program including \code{gettext} calls.
#'
#' @param expr expression or character: expression or file name to analyse
#' @param ... ignored
#'
#' @return a two column matrix with text constants
#' @export
#'
#' @examples
#' xgettext(system.file('shiny', 'app1', 'app.R', package='gettext'))
#' xgettext(system.file('shiny', 'app2', 'app.R', package='gettext'))
xgettext <- function(expr, ...) {
  recurse_all <- function(x) {
    ret <- matrix('', ncol=4, nrow=0)
    if (is.atomic(x)) {
#      print(sprintf("atomic  %s", toString(x)))
      if (is.character(x)) ret <- rbind(ret, c('', x, '', NA))
    } else if (is.name(x)) {
#      print(sprintf("name  %s", toString(x)))
    } else if (is.call(x)) {
#      print(sprintf("call  %s", toString(x)))
      func <- toString(x[[1]])
      if (func %in% c('gettext', 'G', 'ngettext', 'N')) {
        args <- as.list(match.call(definition=match.fun(func), call=x))
        if (!('character' %in% class(args$id1))) args$id1 <- NA
        if (!('character' %in% class(args$id2))) args$id2 <- NA
        if (!('character' %in% class(args$context))) args$context <- NA
        ret <- rbind(ret, c(func, args$id1, args$id2, args$context))
      } else {
        for (i in 1:length(x)) ret <- rbind(ret, recurse_all(x[[i]]))
      }
    } else if (is.pairlist(x)) { # what is that good for?
#      print(sprintf("pairlist %s", toString(x)))
    } else if (is.expression(x)) {
      for (i in 1:length(x)) ret <- rbind(ret, recurse_all(x[[i]]))
    } else {
      # User supplied incorrect input
      stop("Don't know how to handle type ", typeof(x), 
           call. = FALSE)
    }
    ret
  }
  #
  if (is.character(expr)) {
    files <- expr
    expr  <- vector("expression", 0)
    for (file in files) expr <- c(expr, parse(file))
  } 
  if (!is.expression(expr)) stop(sprintf("expression expected, found: %s", paste0(class(expr), collapse=", ")))
  ret <- recurse_all(expr)
  if (nrow(ret)) {
    ret <- ret[!duplicated(ret),]
    class(ret) <- c('xxgettext', class(ret))
  } 
  colnames(ret) <- c('function', 'id1', 'id2', 'context')
  ret
}