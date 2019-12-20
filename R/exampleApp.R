#' exampleApp
#'
#' Runs one (or more examples) Shiny apps from the package. With \code{runExample()} you get a vector of topics back.
#'
#' @param topic character: example(s) to show or \code{NULL}
#'
#' @return nothing
#' @export
#'
#' @examples
#' exampleApp() 
#' \dontrun{
#'   exampleApp('app2en') 
#'   exampleApp('app2de') 
#' }
exampleApp <- function(topic=NULL) {
  if (is.null(topic)) {
    files <- list.files(path=system.file('shiny', package='gettext'), pattern="*.R")
    files <- strsplit(basename(files), '.R')
    return(sapply(files, '[[', 1))
  }
  if (length(topic)>1) stop("More than one topic given")
  file <- paste0(system.file('shiny', package='gettext'), '/', topic[1], '.R')
  source(file)
}