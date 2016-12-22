library(ggplot2)
library(Cairo)   
library(XML)
library(scales)
library(ggthemes)
library(shiny)

##################################################
# User input data
# Unfortunately this needs to be manually edited for each target:

#load_target_data  <- function(targetName){ 
#
#ul_xml  <- "G347.3/upper_limit_bands.xml"
#veto_xml  <- "G347.3/veto_bands.xml"
#old_xml  <- "G347.3/upper_limit_bands_G347_old.xml"
#plot_title  <- "Upper limits for G347.3"
#hage=1.962991e-24
#save_file  <- "upper_limit_plot_G347_vsS6.png"
#}
##################################################

target_properties  <- read.table('target_properties.dat', header=T, stringsAsFactors=F)
rownames(target_properties) <- target_properties$TargName
# Note: the header format of target_properties is:
#TargName D tau h_age
# We'll reference the h_age by the rowname later. 



###################################################
## TODO Delete this seciton
#ul_xml  <- "G347.3/upper_limit_bands.xml"
#old_xml  <- "G347.3/upper_limit_bands_G347_old.xml"
#plot_title  <- "Upper limits for G347.3"
#hage=1.962991e-24
#save_file  <- "upper_limit_plot_G347_vsS6.png"
#veto_xml  <- "G347.3/veto_bands.xml"
###################################################

###################################################
## Load upper limit data from XML files
#ul_doc  <- xmlParse(ul_xml)
#ul_data <- xmlToDataFrame(nodes = getNodeSet(ul_doc, "//upper_limit_band/loudest_nonvetoed_template"), stringsAsFactors=FALSE)
#ul_data_h0 <- xmlToDataFrame(nodes = getNodeSet(ul_doc, "//upper_limit_band/upper_limit_h0"), stringsAsFactors=FALSE)
#names(ul_data_h0)  <- "upper_limit"
#ul_data <- cbind(ul_data,ul_data_h0)
##names(ul_data)
#ul_data <- transform(ul_data, freq=as.numeric(freq), twoF=as.numeric(twoF), twoF_H1=as.numeric(twoFH1), twoF_L1=as.numeric(twoFL1), upper_limit=as.numeric(upper_limit), cover_freq=as.numeric(cover_freq), cover_band=as.numeric(cover_band), f1dot=as.numeric(f1dot), f2dot=as.numeric(f2dot))
###################################################


###################################################
## Load veto band data from XML files
#veto_doc  <- xmlParse(veto_xml)
#veto_data <- xmlToDataFrame(nodes = getNodeSet(veto_doc, "//veto_band"), stringsAsFactors=FALSE)
#names(veto_data)[3]  <- "fscan_power"
##names(veto_data)
#veto_data  <- transform(veto_data, band=as.numeric(band), freq=as.numeric(freq), fscan_power=as.numeric(fscan_power)) 
###################################################


##################################################
# Compare to S6 versions; load from old data XML
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


# Initialise ul_data(?)
#ul_data <- load_ul_data()

##################################################
# Some CSS to obfuscate any error codes
# Adapted from: https://github.com/daattali/advanced-shiny/blob/master/error-custom-message/app.R
#css <- "
#.shiny-output-error { visibility: hidden; }
#.shiny-output-error:before {
#      visibility: visible;
#  content: 'An error occurred. Please contact the admin.'; }
#}
#"

##################################################

##################################################
# Some CSS to animate a spinner while loading 
# Adapted from: https://github.com/daattali/advanced-shiny/blob/master/plot-spinner/app.R
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
          selectInput("target", "Select target:", choices = target_properties$TargName )
            )
        ),
      fluidRow(
        column(width = 8,align="center",offset=2,
          #h4( paste("Input value:",input$target,sep=" ") ),
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
    #ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$upper_limit ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + scale_y_log10(limits=c(10^(floor(log10(min(ul_data$upper_limit, na.rm=TRUE)))), 10^(ceiling(log10(max(ul_data$upper_limit, na.rm=TRUE)))))  ) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + xlab("Frequency (Hz)") + ggtitle(plot_title) + ylab("h0") + theme(axis.text=element_text(size=12, family="xkcd"), axis.title=element_text(size=14, face="bold", family="xkcd"), plot.title = element_text(size = 16, face="bold", family= "xkcd")) + geom_line(data=ul_data, aes(x = ul_data$freq, y = hage), size=1.5, colour="red", alpha=0.5) + geom_point(data=old_data, aes(x = old_data$freq, y = old_data$upper_limit, fill="black")) 
    ul_data <- load_ul_data(input$target)
    h_age <- target_properties[input$target,]$h_age
    # Get rid of old_data
    ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$upper_limit ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + scale_y_log10(limits=c(10^(floor(log10(min(ul_data$upper_limit, na.rm=TRUE)))), 10^(ceiling(log10(max(ul_data$upper_limit, na.rm=TRUE)))))  ) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + xlab("Frequency (Hz)") + ggtitle(input$target) + ylab("h0") + theme(axis.text=element_text(size=12, family="xkcd"), axis.title=element_text(size=14, face="bold", family="xkcd"), plot.title = element_text(size = 16, face="bold", family= "xkcd")) + geom_line(data=ul_data, aes(x = ul_data$freq, y = h_age), size=1.5, colour="red", alpha=0.5) 
  })

  output$plot1 <- renderPlot({
    #ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$upper_limit ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + scale_y_log10(breaks=pretty(ranges2$y, n=5)) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + xlab("Frequency (Hz)") + ggtitle(plot_title) + ylab("h0") + theme(axis.text=element_text(size=12, family="xkcd"), axis.title=element_text(size=14, face="bold", family="xkcd"), plot.title = element_text(size = 16, face="bold", family= "xkcd")) + geom_line(data=ul_data, aes(x = ul_data$freq, y = hage), size=1.5, colour="red", alpha=0.5) + geom_point(data=old_data, aes(x = old_data$freq, y = old_data$upper_limit, fill="black")) 
    # Get rid of old_data
    ul_data <- load_ul_data(input$target)
    h_age <- target_properties[input$target,]$h_age
    ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$upper_limit ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + scale_y_log10(breaks=pretty(ranges2$y, n=5)) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) + xlab("Frequency (Hz)") + ggtitle(input$target) + ylab("h0") + theme(axis.text=element_text(size=12, family="xkcd"), axis.title=element_text(size=14, face="bold", family="xkcd"), plot.title = element_text(size = 16, face="bold", family= "xkcd")) + geom_line(data=ul_data, aes(x = ul_data$freq, y = h_age), size=1.5, colour="red", alpha=0.5) 
  })

  output$plot2 <- renderPlot({
    #ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$f1dot, na.rm=TRUE ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + coord_cartesian(xlim = ranges2$x) + scale_y_continuous(breaks=pretty(range(ul_data$f1dot),n=5)) + xlab("Frequency (Hz)") + ylab("f1dot (Hz/s)") + theme(axis.text=element_text(size=12, family="xkcd")) + geom_point(data=old_data, aes(x = old_data$freq, y = old_data$f1dot, fill="black")) 
    # Get rid of old_data
    ul_data <- load_ul_data(input$target)
    h_age <- target_properties[input$target,]$h_age
    ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$f1dot, na.rm=TRUE ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + coord_cartesian(xlim = ranges2$x) + scale_y_continuous(breaks=pretty(range(ul_data$f1dot),n=5)) + xlab("Frequency (Hz)") + ylab("f1dot (Hz/s)") + theme(axis.text=element_text(size=12, family="xkcd"))
  })
  output$plot3 <- renderPlot({
    #ggplot(data=ul_data, aes(x=ul_data$freq, y=ul_data$f2dot, na.rm=TRUE ) ) + geom_point(colour="skyblue", fill="tan") + guides(fill=FALSE, colour=FALSE) + theme_solarized(light = FALSE) + scale_colour_solarized("blue") + coord_cartesian(xlim = ranges2$x) + scale_y_continuous(breaks=pretty(ul_data$f2dot, n=5)) + xlab("Frequency (Hz)") + ylab("f2dot (Hz/s^{-2})") + theme(axis.text=element_text(size=12, family="xkcd")) + geom_point(data=old_data, aes(x = old_data$freq, y = old_data$f2dot, fill="black")) 
    # Get rid of old_data
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
