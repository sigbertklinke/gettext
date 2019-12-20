#' plural2R
#'
#' Converts a plural from from text to list with the input text, nplurals value and a R function 
#' \code{plural(n)} which computes the plural index from \code{n}. 
#' \itemize{
#' \item Note 1: The index is increased by one since R starts with x[1] and C with x[0]
#' \item Note 2: Only a subset of C symbols are supported based on the test cases: 
#' \code{(}, \code{)}, \code{\%}, \code{<},  \code{>}, \code{>=}, \code{<=}, \code{&&}, \code{||}, \code{?:}, \code{=}, and \code{;}.
#' }
#' 
#' @param ctxt character: one string with a plural form
#'
#' @return list with the input text, nplurals value and a R function plural(n)
#' @importFrom stringr str_match_all
#' @export
#'
#' @examples
#' # Belarusian plural form
#' p <- paste0('nplurals=3;',
#'      'plural=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);')
#' l <- plural2R(p)
#' l$nplurals
#' l$plural(0:100)
plural2R <- function(ctxt) {
  ctxt     <- gsub(' ', '', ctxt, fixed=TRUE)
  # build token list
  alpha    <- str_match_all(ctxt, '([A-Za-z0-9\\.]+)')[[1]][,1]
  nonalpha <- str_match_all(ctxt, '([^A-Za-z0-9\\.]+)')[[1]][,1]
  nonalpha <- gsub('[\\(\\)]', '', nonalpha)
  if (!all(nonalpha %in% c("=",  ";",  ">",  "!=", "==", "?",  ":",  "%",  ">=", "&&", "<=", "<",  "||"))) 
    stop("Undefined C operator found, contact package maintainer")
  #
  token    <- c()
  repeat{
    if (!nchar(ctxt)) break
    if (length(alpha) && startsWith(ctxt, alpha[1])) {
      token <- c(token, alpha[1])
      ctxt  <- sub(alpha[1], '', ctxt, fixed=TRUE)
      alpha <- alpha[-1]
    }
    if (!nchar(ctxt)) break
    if (length(nonalpha) && startsWith(ctxt, nonalpha[1])) {
      token    <- c(token, nonalpha[1])
      ctxt     <- sub(nonalpha[1], '', ctxt, fixed=TRUE)
      nonalpha  <- nonalpha[-1]
    }
    if (!nchar(ctxt)) break
    if (startsWith(ctxt, '(')) {
      token <- c(token, '(')
      ctxt  <- sub('(', '', ctxt, fixed=TRUE)
    }
    if (!nchar(ctxt)) break
    if (startsWith(ctxt, ')')) {
      token <- c(token, ')')
      ctxt  <- sub(')', '', ctxt, fixed=TRUE)
    }
  } 
  # set evaluation priority
  # see https://en.cppreference.com/w/c/language/operator_precedence
  # "=",  ";",  ">",  "!=", "==", "?",  ":",  "%",  ">=", "&&", "<=", "<",  "||"
  baseprio <- 0
  prio  <- rep(0, length(token))
  for (i in 1:length(token)) {
    if (token[i]=='(')  { baseprio <- baseprio+20; prio[i] <- -1 }
    if (token[i]==')')  { baseprio <- baseprio-20; prio[i] <- -1 }
    if (token[i]=='%')  { prio[i] <- baseprio+13; token[i] <- '%%' }    
    if (token[i] %in% c('<', '>', '>=', '<=')) { prio[i] <- baseprio+10 }   
    if (token[i] %in% c('!=', '=='))           { prio[i] <- baseprio+9 }    
    if (token[i]=='&&') { prio[i] <- baseprio+5; token[i] <- '&' }    
    if (token[i]=='||') { prio[i] <- baseprio+4; token[i] <- '|'  }    
    if (token[i] %in% c('?', ':')) { prio[i] <- baseprio+3 }   
    if (token[i]=='=') { prio[i] <- baseprio+1 }
    if (token[i]==';') { prio[i] <- -1 }    
  }
  # build R expression
  maxprio <- max(prio, na.rm=TRUE) 
  while(maxprio>0) {
    pos <- which(prio==maxprio)
    oprio <- maxprio%%20
    if (oprio %in% c(13, 10, 9, 5, 4)) { # ltr
      for (posi in pos) {
        left <- posi-1
        while(prio[left]!=0) left <- left-1;
        right <- posi+1
        while(prio[right]!=0) right <- right+1;
        prio[left]  <- -1
        prio[right] <- -1
        prio[posi]  <- 0
        token[posi] <- paste0('(', token[left], token[posi], token[right], ')')
      }
    }
    if (oprio==3) { # rtl ?:
      for (posi in rev(pos)) {  
        if (token[posi]==':') {
          right <- posi+1
          while(prio[right]!=0) right <- right+1;
          middle <- posi-1
          while(prio[middle]!=0) middle <- middle-1;
          mark <- middle
          while(token[mark]!='?') mark <- mark-1;
          left <- mark-1
          while(prio[left]!=0) left <- left-1;
          prio[mark]  <- prio[left]  <- prio[middle] <- prio[right] <- -1   
          prio[posi]  <- 0
          token[posi] <- paste0('ifelse(', token[left], ',', token[middle], ',', token[right], ')')
        }
      }
    }
    if (oprio==1) { # rtl =
      for (posi in rev(pos)) {  
        left <- posi-1
        while(prio[left]!=0) left <- left-1;
        right <- posi+1
        while(prio[right]!=0) right <- right+1;
        prio[left]  <- -1
        prio[right] <- -1
        prio[posi]  <- 0
        token[posi] <- paste0(token[left], token[posi], token[right])
      }
    }
    maxprio <- max(prio, na.rm=TRUE) 
    #    browser()
  }
  cmd <- token[which(prio==0)]
  posn <- which(startsWith(cmd, 'nplurals='))
  posp <- which(startsWith(cmd, 'plural='))
  list(nplurals=eval(parse(text=cmd[posn])), 
       plural=eval(parse(text=paste0('function(n) { 1+', sub('plural=', '', cmd[posp]), '}')))   
  )  
}
