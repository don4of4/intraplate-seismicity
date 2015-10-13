
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(datasets)
#library(plyr)
#library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  # Create a reactive text
  text <- reactive({
    paste(input$bins[1], '-',input$bins[2])
  }) 
  
  # Return as text the selected variables
  #TODO: Accurately render each date
  output$caption <- renderText({
    text()
  })
  
  # Generate a plot of the requested variables
  output$plot <- renderPlot({
    #print(dataset)
    #plotdata <- dataset[ which(format(dataset$datetime, "%Y") >= input$bins[1] 
    #                   & format(dataset$datetime, "%Y") <= input$bins[2]), ]
    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    #print(typeof(input$bins[1]))
    #print(format(dataset$datetime, "%Y"))
    # print(plotdata)
    pp <- ggplot() +
      geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
      geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
      annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
      geom_point(data=plotdata, size=2, alpha = .7, aes(x=lon, y=lat, color=emw)) +
      scale_color_gradient(low="blue", high="red") +
      theme(plot.background = element_rect(fill = 'grey')) +
      geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)

    print(pp)
  })
  
  output$plot2 <- renderPlot({

    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    
    # Do K-med 
    
    # Graph it.
    pp <- ggplot() +
      geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
      geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
      annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
      geom_point(data=plotdata, size=2, alpha = .7, aes(x=lon, y=lat, color=emw)) +
      scale_color_gradient(low="blue", high="red") +
      theme(plot.background = element_rect(fill = 'grey')) +
      geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)
    
    print(pp)
  })
})