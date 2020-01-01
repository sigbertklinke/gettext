library("shinydashboard")
library("cluster")
library("gettext")
readPofile('de_DE.po', lang='de')

plot.silhouette <- function (x, nmax.lab = 40, max.strlen = 5, main = NULL, sub = NULL, 
          xlab = NA, col = "gray", 
          do.col.sort = length(col) > 1, border = 0, cex.names = par("cex.axis"), 
          do.n.k = TRUE, do.clus.stat = TRUE, ...) 
{
  if (!is.matrix(x) || ncol(x) != 3) 
    stop("No valid silhouette information (#{clusters} =? 1)")
  if (is.na(xlab)) xlab <- paste(gettext("Silhouette width "), expression(s[i]))
  n <- nrow(x)
  x <- sortSilhouette(x)
  s <- rev(x[, "sil_width"])
  space <- c(0, rev(diff(cli <- x[, "cluster"])))
  space[space != 0] <- 0.5
  axisnames <- (n < nmax.lab)
  if (axisnames) 
    names <- substring(rev(rownames(x)), 1, max.strlen)
  if (is.null(main)) {
    main <- gettext("Silhouette plot")
    if (!is.null(cll <- attr(x, "call"))) {
      if (!is.na(charmatch("silhouette", deparse(cll[[1]])))) 
        cll[[1]] <- as.name("FF")
      main <- paste(main, "of", sub("^FF", "", deparse(cll)))
    }
  }
  smry <- summary(x)
  k <- length(nj <- smry$clus.sizes)
  if (is.null(sub)) 
    sub <- paste(gettext("Average silhouette width : "), round(smry$avg.width, 
                                                      digits = 2))
  if (do.col.sort && (lc <- length(col)) > 1) {
    if (lc == k) 
      col <- col[cli]
    else if (lc != n) 
      col <- rep(col, length = n)
    col <- rev(col)
  }
  y <- barplot(s, space = space, names = names, xlab = xlab, 
               xlim = c(min(0, min(s)), 1), horiz = TRUE, las = 1, mgp = c(2.5, 
                                                                           1, 0), col = col, border = border, cex.names = cex.names, 
               axisnames = axisnames, ...)
  title(main = main, sub = sub, adj = 0)
  if (do.n.k) {
    
    mtext(paste("n =", n), adj = 0)
    cl <- gettext("clusters")
    mtext(substitute(k ~ ~ cl ~ ~C[j], list(k = k, cl=cl)), 
          adj = 1)
  }
  if (do.clus.stat) {
    mtext(expression(paste(j, " :  ", n[j], " | ", ave[i %in% 
                                                         Cj] ~ ~s[i])), adj = 1.04, line = -1.2)
    y <- rev(y)
    hasCodes <- !is.null(cx <- attr(x, "codes"))
    for (j in 1:k) {
      j. <- if (hasCodes) 
        cx[j]
      else j
      yj <- mean(y[cli == j.])
      text(1, yj, paste(j., ":  ", nj[j], " | ", format(smry$clus.avg.widths[j], 
                                                        digits = 1, nsmall = 2)), xpd = NA, adj = 0.8)
    }
  }
}

n  <- 25
cl <- matrix(rnorm(4*n), ncol=2)

ui <- dashboardPage(
  dashboardHeader(title = "Silhouette"),
  dashboardSidebar(
    sliderInput('cdist', gettext('Cluster distances'), 0, 10, 5, 0.1),
    sliderInput('pdist', gettext('Point location'), -7, 7, 0, 0.1),
    radioButtons('pid',  gettext('Point belongs to'), 
                choiceNames = gettext(c('red cluster', 'blue cluster')),
                choiceValues = 1:2)
  ),
  dashboardBody(
    plotOutput("cluster", height = 400)
  )
)

server <- function(input, output, session) {
  
  observe({ # extract from URL the language
    query <- parseQueryString(session$clientData$url_search)
    lang <- if (is.null(query[['lang']])) 'en' else query[['lang']]
    options('gettext.lang'=lang) 
    updateSliderInput(session, 'cdist', gettext('Cluster distances')) 
    updateSliderInput(session, 'pdist', gettext('Point location'))
    updateRadioButtons(session, 'pid', gettext('Point belongs to'), 
                       choiceNames = c(gettext('red cluster'), gettext('blue cluster')),
                       choiceValues = 1:2)    
  })
  
  output$cluster <- renderPlot({
    x1 <- cl[1:n,1]-input$cdist/2
    y1 <- cl[1:n,2]
    x2 <- cl[n+(1:n),1]+input$cdist/2
    y2 <- cl[n+(1:n),2]
    par(mfrow=c(1,2))
    plot(0,0, type="n", xlim=c(-7,7), ylim=c(-3,3), asp=TRUE, main=gettext("Cluster assignment"))
    abline(h=0, col="darkgrey")
    points(x1, y1, col="red", pch=19)
    points(x2, y2, col="blue", pch=19)  
    xp <- input$pdist
    yp <- 0
    points(xp, yp, col=ifelse(input$pid==1, "salmon", "lightblue"), pch=19)
    points(xp, yp, col="black")
    di  <- dist(cbind(c(xp, x1, x2), c(yp, y1, y2)))
    cl  <- as.integer(c(input$pid, rep(1, n), rep(2, n)))
    si  <- silhouette(cl, di)
    col <- c(ifelse(input$pid==1, 'salmon', 'lightblue'), rep("red", n), rep("blue", n))
    sic <- order(si[,'cluster'], -si[,'sil_width'])
    plot(si, col=col[sic], main=gettext("Silhouettes"))
  })
}

shinyApp(ui, server)