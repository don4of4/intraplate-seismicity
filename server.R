
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(datasets)
library(shinyRGL)
#library(plyr)
#library(dplyr)
library(ggplot2)
library(scatterplot3d)
library(ks)

shinyServer(function(input, output, clientData, session) {
  
  #plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
  
  #Download handler for CSV download button
  #   output$downloadCSV <- downloadHandler(
  #     filename = function(){
  #       paste('data_', input$bins[1], '-',input$bins[2], '.csv', sep = '')
  #     }, 
  #     content = function(file) {
  #       write.csv(plotdata, file)
  #       }, 
  #     contentType = 'text/csv'
  #     )
  #   
  #   #Download handler for png download button
  #   output$downloadPNG <- downloadHandler(
  #     filename = function(){
  #       paste('data_', input$bins[1], '-',input$bins[2], '.png', sep = '')
  #     }, 
  #     content = function(file) {
  #       write.csv(plotdata, file)
  #     }, 
  #     contentType = 'image/png'
  #   )
  
  # Create a reactive text
  text <- reactive({
    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    
    paste(input$bins[1], '-',input$bins[2], ' => ', nrow(plotdata),' events')
  }) 
  
  # Return as text the selected variables
  #TODO: Accurately render each date
  output$caption <- renderText({
    text()
  })
  
  #observeEvent(input$do, {
  #  updateSliderInput("bins", value = c(input$bins[1],input$bins[2]+5))
  #})
  observeEvent(input$increment_end_year, {
    updateSliderInput(session, "bins", value = c(input$bins[1],input$bins[2]+1))
  })
  observeEvent(input$decrement_end_year, {
    updateSliderInput(session, "bins", value = c(input$bins[1],input$bins[2]-1))
  })
  
  # Generate a plot of the requested variables
  output$plot <- renderPlot({
    
    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    plotstations <- subset(stations.iris, format(start, "%Y") >= input$bins[1] & 
                             format(start, "%Y") <= input$bins[2] & lat >= 33.5 & 
                             lat <= 45.5 & lon <= -69 & lon >= -85)
    pp <- ggplot() +
      geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
      geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
      annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
      geom_point(data=plotstations, size=4, alpha = .7, aes(x=lon, y=lat), color="yellow") +
      #geom_point(data=plotdata, size=3, alpha = .7, aes(x=lon, y=lat, color=emw)) +
      #scale_color_gradient(low="blue", high="red") +
      theme(plot.background = element_rect(fill = 'grey')) +
      geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)
    
    print(pp)
  })
  
  output$plot2 <- renderPlot({
      plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
      
      pp <- ggplot() +
        geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
        geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
        annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
        geom_point(size=2, alpha = .7, aes(dataset$lon, dataset$lat, color=dataset$emw)) +
        scale_color_gradient(low="blue", high="red") +
        theme(plot.background = element_rect(fill = 'grey')) +
        geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)
      
      print(pp)
  })
    
  output$plot3 <- renderPlot({
    
    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    coordinates=with(plotdata,data.frame(long=lon,lat=lat,depth=depth))
    
    # Do K-med 
    #dist  <- earth.dist(coordinates, dist=T)
    # Here the distance is still calculated in 2D, i.e. does not use the depth
    distm  <- dist(coordinates)
    #clust_result=pam(distm,5)
    clustering=pam(distm,pamk(coordinates,criterion="multiasw",usepam=FALSE)$nc,cluster.only=TRUE)
    # Graph it.
    with(plotdata,scatterplot3d(x=lon,y=lat,z=-depth,color=clustering))
    
    if (FALSE) {
      pp <- ggplot() +
        geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
        geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
        annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
        geom_point(size=2, alpha = .7, aes(plotdata$lon, plotdata$lat, color=factor(clust_result$clustering))) +
        scale_color_gradient(low="blue", high="red") +
        theme(plot.background = element_rect(fill = 'grey')) +
        geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1) +
        coord_fixed()
      
      print(pp)
    }    
  })
<<<<<<< HEAD



=======
  
  output$myWebGL <- renderWebGL({
    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    coordinates=with(plotdata,data.frame(long=lon,lat=lat,depth=-depth))
    
    # Do density estimation
    precision=50
    d<<-kde(coordinates,compute.cont=TRUE,gridsize=c(precision,precision,precision))
    # Graph it -> problem integrating rgl and shiny (shinyRGL does not work here for it is not developed anymore, need to find another way, perhaps save to file and generate html link)
    plot(d,cont=(1:5)*1/5*100,drawpoints=TRUE)
  })
  
>>>>>>> 11302c8c4c75d035ac44ff94c26a4776e5e9a90a
})
