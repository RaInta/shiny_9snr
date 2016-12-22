#!/usr/bin/R

####################################################
### This Shiny app provides a means of interacting #
### with the results of a search in LIGO data for ##
### continuous gravitational waves from neutron ####
### star candidates in supernova remnants. See #####
### the following paper (ApJ):
### http://iopscience.iop.org/article/10.1088/0004-637X/813/1/39/meta
### Or browse it on the arXiv:
### https://arxiv.org/abs/1412.5942
###
###
####################################################
###
### Created: 16 June 2016, Ra Inta
### Last modified: 20161221, RI
###################################################


library(ggplot2)
library(Cairo)   
library(XML)
library(scales)
library(ggthemes)
library(shiny)


###################################################
###    Get metadata on all the search targets   ###
###################################################

target_properties  <- read.table('target_properties.dat', header=T, stringsAsFactors=F)
rownames(target_properties) <- target_properties$TargName

###
# Note: the header format of target_properties is:
#TargName D tau h_age
# We'll reference the h_age by the rowname later. 
###################################################


##################################################
# Load search results from LIGO S6 data XMLs
# This is an appropriate place for a function...
###

load_ul_data  <- function(targName="G111.7"){
    old_xml  <- paste(targName,"upper_limit_bands.xml", sep="/") 
    old_doc  <- xmlParse(old_xml)
    old_data <- xmlToDataFrame(nodes = getNodeSet(old_doc, "//upper_limit_band/loudest_nonvetoed_template"), stringsAsFactors=FALSE)
    old_data_h0 <- xmlToDataFrame(nodes = getNodeSet(old_doc, "//upper_limit_band/upper_limit_h0"), stringsAsFactors=FALSE)
    names(old_data_h0)  <- "upper_limit"
    old_data <- cbind(old_data,old_data_h0)
    old_data <- transform(old_data, freq=as.numeric(freq), twoF=as.numeric(twoF), twoF_H1=as.numeric(twoFH1), twoF_L1=as.numeric(twoFL1), upper_limit=as.numeric(upper_limit), cover_freq=as.numeric(cover_freq), cover_band=as.numeric(cover_band), f1dot=as.numeric(f1dot), f2dot=as.numeric(f2dot))
}
##################################################


##################################################
# Some CSS to animate a spinner while loading 
# Adapted from: https://github.com/daattali/advanced-shiny/blob/master/plot-spinner/app.R
### Note: this currently doesn't work as it should!
##################################################
mycss <- "
#plot-container {
 position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
    top: 50%;
    z-index: -1;
      margin-top: -33px;  /* half of the spinner's height */
      margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
"

##################################################

ui <- fluidPage(
  # Plot animated merger gif while waiting to load... 
  tags$head(tags$style(HTML(mycss))),
  theme = "bootstrap.css",
  titlePanel("Interactive upper limit plots"),
  fluidRow(
    column(width = 10, class = "well", align="center",offset=1,
      h4("Upper plot controls zoom for lower plots: upper limits, f1dot and f2dot"),
      fluidRow(
        column(width = 2,align="center",offset=0,
          ### Give a drop-down list of the targets to choose from.
          selectInput("target", "Select target:", choices = target_properties$TargName )
            )
        ),
      fluidRow(
        column(width = 8,align="center",offset=2,
          ### Make a place for the 'master' plot
          plotOutput("plot0", height = 400,
            brush = brushOpts(
              id = "plot0_brush",
              resetOnNew = TRUE
              )
            )
        )
        ),
      fluidRow(
        column(width = 8,align="center",offset=2,
	conditionalPanel(condition="$('html').hasClass('shiny-busy')",
			tags$div(
			    id = "plot-container",
			    tags$img(src = "merger.gif",
			    id = "loading-spinner")
			    )
			),
          plotOutput("plot1", height = 400)
          )
        ),
      fluidRow(
        column(width = 8,align="center",offset=2,
          plotOutput("plot2", height = 400)
        )
        ),
      fluidRow(
        column(width = 8,align="center",offset=2,
          plotOutput("plot3", height = 400)
        )
      ) # fluidRow #2
    )  # main column

  ) # fluidRow #1
)  # fluidPage

server <- function(input, output) {
  ##################################################
  # Linked plots 
    ranges2 <- reactiveValues(x = NULL, y = NULL)
  
    output$plot0 <- renderPlot({
    ### load XML depending on value of target chosen
    # Note: the dumb behavior of the reactiveValues
    # object means that we have to load the data for
    # _each_ plot! This makes for virtually unacceptable latency.
    ###
    ul_data <- load_ul_data(input$target)
    h_age <- target_properties[input$target,]$h_age
    ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$upper_limit ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + scale_y_log10(limits=c(10^(floor(log10(min(ul_data$upper_limit, na.rm=TRUE)))), 10^(ceiling(log10(max(ul_data$upper_limit, na.rm=TRUE)))))  ) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + xlab("Frequency (Hz)") + ggtitle(input$target) + ylab("h0") + theme(axis.text=element_text(size=12, family="xkcd"), axis.title=element_text(size=14, face="bold", family="xkcd"), plot.title = element_text(size = 16, face="bold", family= "xkcd")) + geom_line(data=ul_data, aes(x = ul_data$freq, y = h_age), size=1.5, colour="red", alpha=0.5) 
  })

  output$plot1 <- renderPlot({
    ul_data <- load_ul_data(input$target)
    h_age <- target_properties[input$target,]$h_age
    ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$upper_limit ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + scale_y_log10(breaks=pretty(ranges2$y, n=5)) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + xlab("Frequency (Hz)") + ggtitle(input$target) + ylab("h0") + theme(axis.text=element_text(size=12, family="xkcd"), axis.title=element_text(size=14, face="bold", family="xkcd"), plot.title = element_text(size = 16, face="bold", family= "xkcd")) + geom_line(data=ul_data, aes(x = ul_data$freq, y = h_age), size=1.5, colour="red", alpha=0.5) 
  })

  output$plot2 <- renderPlot({
    ul_data <- load_ul_data(input$target)
    h_age <- target_properties[input$target,]$h_age
    ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$f1dot, na.rm=TRUE ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + coord_cartesian(xlim = ranges2$x) + scale_y_continuous(breaks=pretty(range(ul_data$f1dot),n=5)) + xlab("Frequency (Hz)") + ylab("f1dot (Hz/s)") + theme(axis.text=element_text(size=12, family="xkcd"))
  })
  output$plot3 <- renderPlot({
    ul_data <- load_ul_data(input$target)
    h_age <- target_properties[input$target,]$h_age
    ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$f2dot, na.rm=TRUE ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + coord_cartesian(xlim = ranges2$x) + scale_y_continuous(breaks=pretty(ul_data$f2dot, n=5)) + xlab("Frequency (Hz)") + ylab("f2dot (Hz/s^{-2})") + theme(axis.text=element_text(size=12, family="xkcd"))
  })
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot0_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })

}

shinyApp(ui, server)
