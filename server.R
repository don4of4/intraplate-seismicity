if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny","datasets","ggplot2","scatterplot3d","ks")


shinyServer(function(input, output, clientData, session) {
  
  # Create a reactive text
  text <- reactive({
    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    plotstations <- subset(stations.iris, format(start, "%Y") >= input$bins[1] & 
                             format(start, "%Y") <= input$bins[2] & lat >= 33.5 & 
                             lat <= 45.5 & lon <= -69 & lon >= -85)
    deduped.plotstations <- unique( plotstations[2:2] )
    
    
    #Determine units and correct quantity to insert into caption
    captionUnit <- function(selectedTab, histo){
      unit <- " events"
      if (selectedTab == "Stations Plot" || ( selectedTab == "Statistics" & histo == 'svy')){unit <- " stations" }
      return(unit)
    }
    captionQuant <- function(selectedTab, histo){
      quant <- nrow(plotdata)
      if (selectedTab == "Stations Plot" || ( selectedTab == "Statistics" & histo == 'svy')){quant <- nrow(deduped.plotstations)}
      return(quant)
    }
    
    
    #Formatted caption with proper quant & unit variable values
    paste(input$bins[1], '-',input$bins[2], ' <=> ', captionQuant(input$tabs, input$histoParam), captionUnit(input$tabs, input$histoParam))
  }) 
  
  # Return as text the selected variables
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
  
  
  ## Generate a plot of the requested variables ##
  
  #Stations Plot:
  
  #Zoom features for Plot
  #ranges <- reactiveValues(lon = NULL, lat = NULL)
  ranges <- reactiveValues(latbrush = NULL, lonbrush = NULL)
  
  
  output$plot <- renderPlot({
    
    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    plotstations <- subset(stations.iris, format(start, "%Y") >= input$bins[1] & 
                             #format(start, "%Y") <= input$bins[2] & lat >= ranges$latmin & 
                             #lat <= ranges$latmax & lon <= ranges$lonmax & lon >= ranges$lonmin)
                             format(start, "%Y") <= input$bins[2] & lat >= 33.5 & 
                             lat <= 45.5 & lon <= -69 & lon >= -85)
    pp <- ggplot() +
      geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
      geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
      annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
      geom_point(data=plotstations, size=4, alpha = .7, aes(x=lon, y=lat), color="yellow", shape=17) +
      coord_cartesian(xlim = ranges$latbrush, ylim = ranges$lonbrush) + #for brush frame (was out)
      #geom_point(data=plotdata, size=3, alpha = .7, aes(x=lon, y=lat, color=emw)) + #was out
      #scale_color_gradient(low="blue", high="red") + #was out
      theme(plot.background = element_rect(fill = 'grey')) +
      geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)
    
    print(pp)
    ## Download ##
    datasetInput <- reactive({
      switch(input$downloadset,
             "stations" = plotstations,
             "earthquakes" = plotdata)
    })
    
    output$table <- renderTable({
      datasetInput()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { paste('output.csv', sep='') },
      content = function(file) {
        write.csv(datasetInput(), file)
      }
    )
  })
  
  #ranges <- reactiveValues(x = NULL, y = NULL)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot_dblclick, {
    print("DOUBLE CLICK IS REGISTERING") #test line
    brush <- input$plot_brush
    print("Selected coords:", brush$xmin, brush$xmax) #test line
    print(brush$xmin) #test line
    if (!is.null(brush)) {
      #plotstations <- subset(stations.iris, format(start, "%Y") >= input$bins[1] & 
      #format(start, "%Y") <= input$bins[2] & lat >= latmin & 
      #lat <= latmax & lon <= lonmax & lon >= lonmin)
      #                        format(start, "%Y") <= input$bins[2] & lat >= 40 & 
      #                       lat <= 45.5 & lon <= -80 & lon >= -85)
      #format(start, "%Y") <= input$bins[2] & lat >= 35 & lat <= 40 & lon <= -75 & lon >= -80
      #ranges$x <- c(brush$xmin, brush$xmax)
      #ranges$y <- c(brush$ymin, brush$ymax)
      
      #plotstations <- subset(plotstations, format(start, "%Y") >= input$bins[1] &
      #                             format(start, "%Y") <= input$bins[2] & lat >= 40 & 
      #                             lat <= 45.5 & lon <= -80 & lon >= -85)
      
      #test.data <- plotstationsZoom
      
      ranges$latbrush <- c(brush$xmin, brush$xmax)
      ranges$lonbrush <- c(brush$ymin, brush$ymax)
      
      
    } else {
      ranges$latbrush <- NULL
      ranges$lonbrush <- NULL
    }
  })
  
  #Earthquakes Plot:
  
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
  
  
  output$plot4 <- renderPlot({
    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    coordinates=with(plotdata,data.frame(long=lon,lat=lat,depth=depth))
    calc_coordinates=with(plotdata,data.frame(long=lon*100,lat=lat*100,depth=depth))
    model=dbscan(calc_coordinates,MinPts=input$minPts,eps=input$eps)
    clusters=predict(model,calc_coordinates)+1
    with(coordinates,scatterplot3d(x=long,y=lat,z=-depth,color=clusters))
    
  })
  
  output$plot5 <- renderPlot({
    plotdata <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    calc_coordinates=with(plotdata,data.frame(long=lon*100,lat=lat*100,depth=-depth))
    precision=50
    d<<-kde(calc_coordinates,compute.cont=TRUE,gridsize=c(precision,precision,precision))
    plot(d,cont=(1:5)*1/5*100,drawpoints=TRUE)
    
  })
  
  
  #Histogram Plot
  
  output$histoPlot <- renderPlot({
    #For histogram CE
    plotdata1 <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    # --> CE by mag
    plotdata1sort <- plotdata1[with(plotdata1, order(-emw)), ]
    plotdata1sort$events <- seq.int(nrow(plotdata1sort))
    # --> CE by time
    plotdata1sort2 <- plotdata1[with(plotdata1, order(datetime)), ]
    plotdata1sort2$events <- seq.int(nrow(plotdata1sort))
    
    #For histogram TE vs depth & Mag vs TE
    plotdata2 <- subset(dataset, format(datetime, "%Y") >= input$bins[1] & format(datetime, "%Y") <= input$bins[2])
    
    #For stations/year graph
    plotstations <- subset(stations.iris, 
                             format(start, "%Y") >= input$bins[1]
                             & format(start, "%Y") <= input$bins[2]
                             #& format(end, "%Y") >= input$bins[1]
                             & format(end, "%Y") >= input$bins[2]
                             & lat >= 33.5 & lat <= 45.5 
                             & lon <= -69 & lon >= -85)
    df <- plotstations[,c('sta','start')]
    df$start <- as.Date(df$start, "%Y")
    #df2 <- subset(df,format(start, "%Y"))
    deduped2.plotstations <- subset(df, !duplicated(df[,1]))
    
    selectHisto <- function(histoParam){
      switch(histoParam,
             magvce = plot(plotdata1sort$emw, plotdata1sort$events, type="p", main = "Cumulative # of Events vs Magnitude", xlab = "Magnitude", ylab = "Cumulative Number"),
             magvte = hist(plotdata2$emw, breaks = 8, main = "# of Events vs Magnitude", xlab="Magnitude", ylab="Events", col = 'darkblue', border='white'),
             cevt = plot(plotdata1sort2$datetime, plotdata1sort2$events, type="p", main = "Cumulative # of Events vs Magnitude", xlab = "Magnitude", ylab = "Cumulative Number"),
             tevd = hist(plotdata2$depth, breaks = 20, main = "# of Events vs Depth", xlab = "Depth", col = 'darkorange', border='white'),
             svy = hist(deduped2.plotstations$start, breaks = 20, main = "Stations Per Year", xlab="Year", ylab="Stations", col = 'yellow', border='white') #ylim=c(0,400),
      )
    }
    
    selectHisto(input$histoParam)
    
  })
  
  
  ##TEST PLOT##
  
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$tplot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$tplot_dblclick, {
    #print("hello")
    brush <- input$tplot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  #END TEST PLOT FUNCTIONS
  
})
