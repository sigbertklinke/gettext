translation <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) { 
#  load(system.file('data', 'iso639.rda', package="gettext"), env=translation) 
  options('gettext.lang'='en',
          'gettext.domain'='')
}