library("gettext")
pluralform <- read.csv("pluralforms.csv", stringsAsFactors = FALSE)
plural <- list()
for (i in 1:nrow(pluralform)) {
  plural[[i]] <- plural2R(pluralform[i,2])
  plural[[i]]$lang <- trimws(pluralform[i,1])
  plural[[i]]$form <- trimws(pluralform[i,2])
}
save(plural, file='../../data/plural.rda', version=2)
