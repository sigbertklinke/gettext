#' readPofile
#'
#' Reads one or more PO file into the library internal translation object. 
#' If \code{file} has been created with \code{\link{language}} then several files with different
#' domains and languages can be read at once.
#'
#' @param file character vector with file names or languageFile object
#' @param lang character: language code (default: \code{getOption('gettext.lang')}, usually \code{"en"})
#' @param domain character: text domain (default: \code{getOption('gettext.domain')}, usually \code{NA})
#' @param append logical: append POfile to internal translation environment or overwrite (default)
#'
#' @return invisibly the internal used languageFile object
#' @importFrom stringr str_match
#' @export
#'
#' @examples
#' \dontrun{
#' readPofile('test_de.po', lang='de')
#' readPofile(languageFiles('test_de.po'))
#' }
#' 
readPofile <- function(file, lang=getOption('gettext.lang'), domain=getOption('gettext.domain'), append=FALSE) {
  read.file <- function(file) {
    fcont <- readLines(file)
    # determine lines types
    df <- data.frame(line=fcont, type=rep(0, length(fcont)), param=rep('', length(fcont)),
                     file=file, lno=1:length(fcont), used=rep(FALSE, length(fcont)),
                     stringsAsFactors = FALSE)
    # comments
    df$type[grepl('^\\s*\\#', df$line)]    <- 1
    df$type[grepl('^\\s*\\#\\.', df$line)] <- 2
    df$type[grepl('^\\s*\\#\\:', df$line)] <- 3
    df$type[grepl('^\\s*\\#\\,', df$line)] <- 4
    df$type[grepl('^\\s*\\#\\|', df$line)] <- 5
    # msg
    df$type[grepl('^\\s*msgid\\s*\\"', df$line)]   <- 10
    df$type[grepl('^\\s*msgid_plural\\s*\\"', df$line)]  <- 11
    df$type[grepl('^\\s*msgctxt\\s*\\"', df$line)] <- 14  
    df$type[grepl('^\\s*msgstr.*?\\"', df$line)]   <- 15
    df$type[grepl('^\\s*msgstr\\s*\\[.*?\\"', df$line)]  <- 16
    df$type[grepl('^\\s*\\"', df$line)]  <- 19
    # empty line
    df$type[grepl('^\\s*$', df$line)]  <- 9    
    # check for undefined line types
    index <- which(df$type==0)
    if (length(index)) {
      for (j in index) {
        msgWarning("Invalid line found, ignored\n",
                   sprintf("%s\n", df$line[j]), file=file, line=j)
      }
    }
    df
  }
  #
  checkchar <- function(df, escr = c('a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\"', '`', '?', 'x', 'u', 'U')) {
    quot  <- gregexpr('"', df$line, fixed=TRUE)
    first <- rep(1, length(quot))
    last  <- rep(1000000L, length(quot))
    lquot <- rep(0, length(quot))      
    esc   <- rep(FALSE, length(quot))
    escsq <- gregexpr('\\', df$line, fixed=TRUE)
    for (j in 1:length(quot)) {
      # check quotation mark
      v <- quot[[j]]
      if (v[1]!=-1) {
        w <- v[v>1]
        w <- w[!((w-1) %in% escsq[[j]])]
        if (v[1]==1) w <- c(1,w)
        lquot[j] <- length(w)
        first[j] <- min(w)
        last[j]  <- max(w)
      }
      # check escape sequences
      v <- escsq[[j]]
      if (v[1]>0) {
        linej  <- strsplit(df$line[j], '')[[1]]
        v      <- v[v<length(linej)]
        esc[j] <- all(!(linej[v+1] %in% escr))
      }
    }
    index <- which(esc)
    if (length(index)) {
      for (j in index) {
        msgWarning("Invalid escape sequence found, ignored\n",
                   sprintf("%s\n", df$line[j]), file=df$file[j], line=df$lno[j])
      }
    }
    index     <- which(lquot!=2)
    if (length(index)) {
      for (j in index) {
        msgWarning("Less or more than two \" found, bol, eol, first, and/or last will be used\n",
                   sprintf("%s\n", df$line[j]), file=df$file[j], line=df$lno[j])
      }
    }
    data.frame(nquot=lquot, first=first, last=last, esc=esc)
  }
  #
  continuation <- function(df) {
    for (j in nrow(df):1) {
      if ((j>1) && (df$type[j]==19)) df$param[j-1] <-  paste0(df$param[(j-1):j], collapse="")
    }
    df[df$type!=19,]
  }
  #
  buildmsg <- function (df, j, lang, file) {
    j0 <- j
    str    <- NULL
    plural <- NA
    #
    context <- NA
    if ((j>1) && (df$type[j-1]==14)) {
      context    <- df$param[j-1] 
      df$used[j-1] <- TRUE
    }
    #
    id         <- df$param[j]
    df$used[j] <- TRUE
    #
    j <- j+1
    while ((df$type[j] %in% c(11, 15, 16)) && (j<=nrow(df))) {
      if (df$type[j]==11) {
        plural     <- df$param[j]
        df$used[j] <- TRUE
      }
      if (df$type[j]==15) {
        if (is.null(str)) {
          str <- df$param[j]
        }
        else {
          if (!is.na(str[1]))
            msgWarning("Multiple 'msgstr', overwritten\n",
                       sprintf("%s\n", df$line[j]), file=file, line=j)
          str[1] <- df$param[j]
        }
        df$used[j] <- TRUE
      }
      if (df$type[j]==16) {
        pos <- 1+as.integer(str_match(df$line[j], '^\\s*msgstr\\s*\\[(.*?)\\]')[,2])
        if (is.null(str)) {
          # browser()
          str <- rep(NA, pos) 
        } else {
          if (pos>length(str)) str <- c(str, rep(NA, pos-length(str)))
        }
        if (!is.na(str[pos])) {
          msgWarning("Multiple 'msgstr[N]', overwritten\n",
                     sprintf("%s\n", df$line[j]), file=file, line=j)       
        }
        str[pos]   <- df$param[j]
        df$used[j] <- TRUE
      }
      j <- j+1
    }
    strna <- is.na(str) 
    if (any(strna)) {
      msgWarning("Missing 'msgstr[]', will use 'NA' as translation\n",
                 sprintf("%6s: %s\n", df$line[j]), file=file, line=j)
    }
    addMsg(id, str, plural=plural, context=context, lang=lang, domain=dom, file=file, line=j0)
    df
  }
  ## function body
  if (!append) cleanTranslation()
  if ('bind' %in% class(file)) {
    langfile <- file
  } else {
    langfile <- data.frame(lang=rep(lang, length(file)), domain=rep(domain, length(file)), file=file,
                           stringsAsFactors = FALSE)    
  }
  #  0 = unknown 
  #  1 = translator-comments
  #  2 = extracted-comments 
  #  3 = reference
  #  4 = flag
  #  5 = msgid previous-untranslated-string
  #  9 = empty line
  # 10 = msgid
  # 11 = msgid_plural
  # 14 = msgctxt  
  # 15 = msgstr
  # 16 = msgstr[n]
  # 19 = continuation
  #browser()
  for (i in 1:nrow(langfile)) {
    file <- langfile$file[i]
    dom  <- langfile$domain[i]
    lang <- langfile$lang[i]
    cat(file, "\n")
    if (file.exists(file)) {
      df   <- read.file(file)
      df   <- df[df$type>9,]         # delete all comments, empty and ignored lines
      dfcc <- checkchar(df)          # check quotation and escape sequences
      df$param  <- substr(df$line, dfcc$first+1, dfcc$last-1) # extract parameters
      df   <- continuation(df)       # merge continuation lines and delete 
      # build message table
      df$used <- rep(FALSE, nrow(df))
      msgpos  <- which(df$type==10)
      for (j in msgpos) {
        df <- buildmsg(df, j,  langfile$lang[i], file)
      }
      if (any(!df$used)) {
        msgWarning("Missing 'msgid', ignored or overwritten\n", 
                    sprintf("%s\n", df$file[!df$used]), file=file, line=df$lno[!df$used])
      }
      # detect nplural + plural
      # browser()
      # npl <- max(sapply(msg[[lang[i]]], function(v) { length(v$str) }))
      key <- makeKey('', NA, lang, dom, NA)
      if (!is.null(translation[[key]])) { 
        pll <- str_match(translation[[key]], 'Plural-Forms:\\s*(nplurals\\s*=\\s*[0-9]+\\s*;\\s*plural\\s*=.*?;)')
        if ((nrow(pll)==1) && !is.na(pll[1,2])) addPlural(text=pll[1,2], lang=lang, domain=dom)
      }
    } else {
      stop(sprintf("%s - file not found\n", file))
    }
  }
  # check plural attributes
  langs <- setdiff(names(translation), 'warning')
  for (lang in langs) {
    tlp <- attr(translation[[lang]], 'plural')
    if (!is.null(tlp)) {
      if (any(((tlp$nplurals!=tlp$nplurals[1]) | (tlp$plural!=tlp$plural[1])))) {
        msgWarning("non-identical plural definitions found\n", file=sQuote(lang),
                   sprintf("%2.0f %s\n", tlp$nplurals, tlp$plural), line=sprintf("%s:%0.f", tlp$file, tlp$line))
      }
    }
  }
  printWarnings()
  invisible(langfile)
}