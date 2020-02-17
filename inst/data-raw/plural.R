library("gettext")
file       <- system.file("data-raw", "pluralforms.csv", package="gettext")
pluralform <- read.csv(file, stringsAsFactors = FALSE)
plural <- list()
for (i in 1:nrow(pluralform)) {
  plural[[i]] <- plural2R(pluralform[i,2])
  plural[[i]]$lang <- trimws(pluralform[i,1])
  plural[[i]]$form <- trimws(pluralform[i,2])
}
file <- sprintf("%s/plural.rda", tempdir(TRUE))
save(plural, file=file, version=2)
