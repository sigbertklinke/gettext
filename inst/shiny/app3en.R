launch.browser <- function(url) {
  url <- sprintf('%s/?lang=%s',url, 'en')
  invisible(.Call("rs_shinyviewer", url, getwd(), 3))
}
#
library("shiny")
runApp(system.file('shiny', 'app3', package='gettext'), launch.browser=launch.browser)