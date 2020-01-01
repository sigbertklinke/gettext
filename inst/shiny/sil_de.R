launch.browser <- function(url) {
  url <- sprintf('%s/?lang=%s',url, 'de')
  invisible(.Call("rs_shinyviewer", url, getwd(), 3))
}
#
library("shiny")
runApp(system.file('shiny', 'silhouette', package='gettext'), launch.browser=launch.browser)