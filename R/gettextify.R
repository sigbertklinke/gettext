
#' gettextify
#'
#' Takes a file, R code or R expression and replaces all text constants by \code{gettext(text_constant)}. 
#' The replacement is not done inside \code{gettext}, \code{G}, \code{ngettext}, \code{N}, \code{library} and
#' \code{require}.            
#' If \code{expr} is a character then \code{gettextify} tries first to open a file with the name.
#' If this fails then it wil be assumed that \code{expr} contains valid R code. 
#'
#' @param expr character or expression
#' @param exclude character: names of further function to exclude from replacement
#'
#' @return character vector with modified code
#' @export
#'
#' @examples
#' gettextify ('x<-"a"')
#' gettextify (system.file(package='gettext', 'shiny', 'app1', 'app.R'))
#' gettextify (parse(system.file(package='gettext', 'shiny', 'app1', 'app.R')))
gettextify <- function(expr, exclude=NULL) {
  recurse_all <- function(x) {
    if (is.atomic(x)) {
#      print(sprintf("atomic  %s", toString(x)))
      if (is.character(x)) x <- parse(text=sprintf('gettext("%s")', x))[[1]]
    } else if (is.name(x)) {
#      print(sprintf("name  %s", toString(x)))
    } else if (is.call(x)) {
#      print(sprintf("call  %s", toString(x)))
      func <- toString(x[[1]])
      if (!(func %in% c('gettext', 'G', 'ngettext', 'N', 'library', 'require', exclude))) {
        for (i in 1:length(x)) x[[i]] <- recurse_all(x[[i]])
      } 
    } else if (is.pairlist(x)) { # what is that good for?
#      print(sprintf("pairlist %s", toString(x)))
    } else if (is.expression(x)) {
      for (i in 1:length(x)) x[[i]] <- recurse_all(x[[i]])
    } else {
      # User supplied incorrect input
      stop("Don't know how to handle type ", typeof(x), 
           call. = FALSE)
    }
    x
  }
  #
  if (is.character(expr)) {
    res <- try(suppressWarnings(parse(expr)), silent = TRUE)
    if ('try-error' %in% class(res)) {
      res <- try(parse(text=expr), silent = TRUE)
    }
  } else {
    res <- expr
  }
  if (!is.expression(res)) stop('Neither filename, R code nor R expression given') 
  res <- recurse_all(res)
  c('library("gettext")', as.character(res))
}