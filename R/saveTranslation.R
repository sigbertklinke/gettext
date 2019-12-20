#' saveTranslation, readTranslation and cleanTranslation
#' 
#' \itemize{
#' \item \code{saveTranslation} saves the library-internal translation environment as an RDS file, for details see \code{\link[base]{saveRDS}}.
#' \item \code{readTranslation} restores the library-internal translation environment from an RDS file, for details see \code{\link[base]{readRDS}}.
#' \item \code{cleanTranslation} deletes in all domains and languages the translations in the library-internal translation environment.
#' }
#' 
#' @aliases readTranslation cleanTranslation
#' @inheritParams base::saveRDS
#' @inheritParams base::readRDS
#'
#' @return nothing
#' @export
#' 
#' @examples
#' \dontrun{
#' saveTranslation("translation.RDS")
#' cleanTranslation()
#' readTranslation("translation.RDS")
#' }
saveTranslation <- function(file, ascii = FALSE, version = 2, compress = TRUE, refhook = NULL) {
  saveRDS(translation, file,  ascii, version, compress, refhook)
}

#' @inheritParams base::readRDS
#' @param append logical: append to current translation environment (default: \code{TRUE})
#' @rdname saveTranslation
#' @export
readTranslation <- function (file, refhook = NULL, append=TRUE) {
  if (!append) cleanTranslation()
  l <- readRDS(file, refhook)
  for (name in names(l)) translation[[name]] <- l[[name]]
}

#' @rdname saveTranslation
#' @export
cleanTranslation <- function () {
  remove(list=ls(translation), envir=translation)
}
