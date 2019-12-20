library("shiny")
library("gettext")
readPofile('de_DE.po', lang='de')

server <- function(input, output, session) {
  observe({ # extract from URL the language
    query <- parseQueryString(session$clientData$url_search)
    lang <- if (is.null(query[['lang']])) 'en' else query[['lang']]
    options('gettext.lang'=lang) 
    updateSliderInput(session, 'nbin', gettext("Number of bins:"))   # update UI element
  })
  
  output$distPlot <- renderPlot({
    breaks <- seq(min(faithful$eruptions), max(faithful$eruptions), len=input$nbin+1)
    sub    <- sprintf(ngettext('%.0f bin', '%.0f bins', input$nbin), input$nbin)
    hist(faithful$eruptions, breaks=breaks, 
         xlab=gettext("Eruption time"), ylab=gettext("Frequency"), main=gettext("Old Faithful geyser"),
         sub=sub)
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("nbin", gettext("Number of bins:"), min = 1, max = 50, value = 20)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)