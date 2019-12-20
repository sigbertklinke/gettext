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
#' xgettext(parse(system.file('app1', 'app.R', package='gettext')))
#' xgettext(parse(system.file('app2', 'app.R', package='gettext')))
xgettext  <- function(expr, ...) UseMethod("xgettext")

#' @export
#' @rdname xgettext
xgettext.default <- function(expr, ...) {
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
    ret   <- NULL
    for (file in files) ret <- rbind(ret, recurse_all(parse(expr)))
  } else {
    if (is.expression(expr)) ret <- recurse_all(expr) else stop('Neither filename nor expression given')
  }
  if (nrow(ret)) {
    ret <- ret[!duplicated(ret),]
    class(ret) <- c('xgettext', class(ret))
  } 

  colnames(ret) <- c('function', 'id1', 'id2', 'context')
  ret
}

#' print
#'
#' @param x xgettext object
#' @param ... further parameters given to \code{print.default}
#'
#' @return returns its argument invisibly
#' @export
#'
#' @examples
#' txt <- xgettext(parse(system.file('shiny', 'app1', 'app.R', package='gettext')))
#' txt
print.xgettext <- function (x, ...) { print.default(x, ...) }