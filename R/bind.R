#' bind
#'
#' Assigns a file, based on its name or a named parameter, to a language and a domain. 
#' The base name of a file is composed by a \strong{domain} and \strong{language code} 
#' separated by anon-letters. A language code starts with a two (ISO 639-1) 
#' letter code. 
#' 
#' The result can be used by \code{readPofile}. If called with no parameters then the 
#' internal set of test files is given back.
#'
#' @param ... list: list of parameters
#'
#' @return a data frame with: \code{lang}, the assigned language, \code{domain}, the assigned domain, \code{iso639}, the identified language, and \code{file}, the filename
#' @importFrom stringr str_extract_all
#' @export
#'
#' @examples
#' options('gettext.lang'='en')
#' bind('myproject_de.po')
#' bind(de='myproject.po')
#' bind(de=c('myproject1.po', 'myproject2.po'))
#' bind(de=c('myproject1_de.po', 'myproject2_de.po'), 
#'          en=c('myproject1_en.po', 'myproject2_en.po'))
#' files <- c("myproject_de.po", 
#'            "intl/for_use_owncloud_impress_cs_CZ.po",
#'            "intl/ibalsa-master-zh_HK.po",
#'            "spirit-po/test1.po")
#' bind(files)
#' bind()
bind <- function(...) {
  args  <- list(...)
  if (length(args)) {
    nargs  <- names(args)
    if (is.null(nargs)) nargs <- rep('', length(args))
    files <- unlist(args)    
    lang  <- rep(nargs, sapply(args, length))
  } else {
    files <- list.files(path=system.file('test', 'spirit-po', package="gettext"), 
                        pattern="\\.po$", full.names = TRUE)
    lang  <- rep('', length(files))
  }
  dotsplit <- strsplit(tolower(basename(files)), '.', fixed=TRUE)
  if(any((sapply(dotsplit, length)<2) & (lang==''))) stop("no extension found")
  noext  <- sapply(dotsplit, function(v) { paste(v[-length(v)], sep=".") })
  if (any(lang=='')) {
    ascii  <- strsplit(noext, '[^A-Za-z]')
    flang  <- sapply(ascii, function(v) { suppressWarnings(v[min(which(v %in% gettext::iso639$code))]) })
    dllist <- strsplit(noext, paste0('[^A-Za-z]', flang))
    domain <- sapply(dllist, '[', 1)  
    rlang  <- sapply(dllist, '[', 2)
    olang <- getOption('gettext.lang')
    flang[is.na(flang)] <- if(is.null(olang)) 'en' else olang
    llang <- paste0(flang, ifelse(is.na(rlang), '', rlang))
  }
  df <- data.frame(language = ifelse(lang=='', llang, lang),
                   domain   = ifelse(lang=='', domain, noext),
                   iso639   = ifelse(lang=='', flang, lang),
                   file     = files, stringsAsFactors = FALSE)
  class(df) <- c("bind", class(df))
  df
} 
