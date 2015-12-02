if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny","datasets","ggplot2","scatterplot3d","ks","fpc","dplyr","lubridate","stats","base")



shinyServer(function(input, output, clientData, session) {
  
  # Create a reactive text
  text <- reactive({
    plotdata <- subset(dataset, as.numeric(format(datetime, "%Y")) >= input$bins[1] & as.numeric(format(datetime, "%Y")) <= input$bins[2]
                       & lat <= input$manlatmax & lat >= input$manlatmin
                       & lon <= input$manlonmax & lon >= input$manlonmin)
    
    plotstations <- subset(stations.iris, as.numeric(format(start, "%Y")) >= input$bins[1] & 
                             as.numeric(format(start, "%Y")) <= input$bins[2] 
                             & lat <= input$manlatmax & lat >= input$manlatmin
                             & lon <= input$manlonmax & lon >= input$manlonmin)
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
  
  observeEvent(input$increment_end_year, {
    updateSliderInput(session, "bins", value = c(input$bins[1],input$bins[2]+1))
  })
  observeEvent(input$decrement_end_year, {
    updateSliderInput(session, "bins", value = c(input$bins[1],input$bins[2]-1))
  })
  
  observeEvent(input$increment_start_year, {
    updateSliderInput(session, "bins", value = c(input$bins[1]+1,input$bins[2]))
  })
  observeEvent(input$decrement_start_year, {
    updateSliderInput(session, "bins", value = c(input$bins[1]-1,input$bins[2]))
  })
  
  #Stations Plot:
  
  output$plot <- renderPlot({
    
    plotdata <- subset(dataset, as.numeric(format(datetime, "%Y")) >= input$bins[1] & as.numeric(format(datetime, "%Y")) <= input$bins[2])
    plotstations <- subset(stations.iris, as.numeric(format(start, "%Y")) >= input$bins[1] & as.numeric(format(start, "%Y")) <= input$bins[2] 
                           & lat >= input$manlatmin & lat <= input$manlatmax 
                           & lon <= input$manlonmax & lon >= input$manlonmin)
    pp <- ggplot() +
      geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
      geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
      annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
      geom_point(data=plotstations, size=4, alpha = .7, aes(x=lon, y=lat), color="yellow", shape=17) +
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
  
 
  
  #Earthquakes Plot:
  output$plot2 <- renderPlot({
    plotdata <- subset(dataset, as.numeric(format(datetime, "%Y")) >= input$bins[1] & as.numeric(format(datetime, "%Y")) <= input$bins[2] 
                       & lat <= input$manlatmax & lat >= input$manlatmin
                       & lon <= input$manlonmax & lon >= input$manlonmin)
    
    pp <- ggplot() +
      geom_polygon(aes(long,lat, group=group), fill="palegreen3", colour="grey60", data=county) +
      geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
      annotate("rect", xmin=-84, xmax=-71, ymin=35.5, ymax=43.5, colour="black", size=1, fill="blue", alpha="0.01") +
      #annotate("rect", xmin=input$manlonmin, xmax=input$manlonmax, ymin=input$manlatmin, ymax=input$manlatmax, colour="black", size=1, fill="blue", alpha="0.01") +
      geom_point(data=plotdata, size=2, alpha = .7, aes(x=lon, y=lat, color=emw)) +
      scale_color_gradient(low="blue", high="red") +
      theme(plot.background = element_rect(fill = 'grey')) +
      geom_abline(intercept = 3, slope = -.45, color = "grey", size = 1)
    
    print(pp)
  })
  
  #DBScan Plot:
  output$plot4 <- renderPlot({
    plotdata <- subset(dataset, as.numeric(format(datetime, "%Y")) >= input$bins[1] & as.numeric(format(datetime, "%Y")) <= input$bins[2] 
                       & lat <= input$manlatmax & lat >= input$manlatmin
                       & lon <= input$manlonmax & lon >= input$manlonmin)
  
    coordinates=with(plotdata,data.frame(long=lon,lat=lat,depth=depth))
    calc_coordinates=with(plotdata,data.frame(long=lon*100,lat=lat*100,depth=depth))
    model=dbscan(calc_coordinates,MinPts=input$minPts,eps=input$eps)
    clusters=predict(model,calc_coordinates)+1
    with(coordinates,scatterplot3d(x=long,y=lat,z=-depth,color=clusters))
    
  })
  
  #3D Plot:
  output$plot5 <- renderPlot({
    plotdata <- subset(dataset, as.numeric(format(datetime, "%Y")) >= input$bins[1] & as.numeric(format(datetime, "%Y")) <= input$bins[2] 
                       & lat <= input$manlatmax & lat >= input$manlatmin
                       & lon <= input$manlonmax & lon >= input$manlonmin)
  
    calc_coordinates=with(plotdata,data.frame(long=lon*100,lat=lat*100,depth=-depth))
    precision=50
    d<<-kde(calc_coordinates,compute.cont=TRUE,gridsize=c(precision,precision,precision))
    plot(d,cont=(1:5)*1/5*100,drawpoints=TRUE)
    
  })
  
  
  #Histogram Plot
  
  output$histoPlot <-  renderPlot({ 
    options( warn = 0 )
    
    #For histogram CE & histogram TE vs depth & Mag vs TE
    plotdata1 <- plotdata2 <- subset(dataset, as.numeric(format(datetime, "%Y")) >= input$bins[1] & as.numeric(format(datetime, "%Y")) <= input$bins[2]
      & lat <= input$manlatmax & lat >= input$manlatmin
      & lon <= input$manlonmax & lon >= input$manlonmin)
  
    # --> Cum. Number mag
    plotdata1sort <- plotdata1[with(plotdata1, order(-emw)), ]
    plotdata1sort$events <- seq.int(nrow(plotdata1sort))
    
    cum_function <- function(){ 
   
      # Use dplyr pipelining  %>% to query the data
      # Merges events of identical emw and counts them by that group.
      frequency_aggregate <- plotdata1 %>% select(emw)  %>% group_by(emw) %>% tally(sort = FALSE) %>% arrange(desc(emw))
      
      # Uses frequency_aggregate to generate cumulative frequency.  Trendline using loess method.
      cum_frequency <- frequency_aggregate %>% mutate(cum = cumsum(n)) %>% select(emw, cum)
      # Example of approx. how to get the trendline without stealing it from the plot (below)
        # cum_frequency_loess <- loess(cum ~ log10(emw), data=cum_frequency, span=0.5)
        # cum_frequency_trendline <- predict(cum_frequency_loess, se=TRUE)
      
      # eps = Machine epsilon -> I.e. Floating-point relative accuracy
      # Aka the smallest unit before we will see truncation
      eps <- .Machine$double.eps
      # Percision p (See: https://en.wikipedia.org/wiki/Machine_epsilon)
      p <- log2(.Machine$double.eps)*-1 + 1
      
      # Set our paritions (for derivative estimation) to  1/ max(1/4 the percision and p of 14)
      # Basically, we need a large sample, within reason.
      partitions <- 1/max(2^-(p/4),2^-14)
      
      # Consider also (future): http://stackoverflow.com/questions/14207250/determining-derivatives-from-gam-smooth-object
      
      # Do the initial graph and smoothing, note we request 1000 pts for the curve to reduce noise later
      cum_frequency_graph <- ggplot(aes(x=emw, y=cum), data=cum_frequency, xmin=0) + geom_point(size=3, shape=21, color="cyan4", fill="cyan4") + ggtitle("Cumulative Frequency on Magnitude") +
                             scale_x_continuous("Magnitude (MMS)") + scale_y_log10("Cumulative Frequency",limits=c(1,NA), breaks = (2**(1:10))) +
                             expand_limits(x=c(0,6)) + theme(text = element_text(size=18, face="bold")) + 
                             stat_smooth(aes(outfit=fit<<-..y..,outx=fitx<<-..x.., color="Loess Curve (span=0.7)"), formula=y~log(x),method="loess", se=FALSE, n = partitions, size=0.5, span = 0.7, na.rm = TRUE) +
                             stat_smooth(aes(color="Loess Curve (span=0.6)"), formula=y~log(x),method="loess", se=TRUE, size=1, span = 0.6, na.rm = TRUE) +
                             scale_colour_manual("Key:", breaks = c("Loess Curve (span=0.7)","Loess Curve (span=0.6)", "1st Der. * -1 (LC(s=0.7))","Mc"), values = c("blue","cyan","red","purple"))
      
      # Note aes(outfit=fit<<-..y..) is a hack to extract the y axis of the fit to variable "fit" ... see also fitx
      # Also note, we need to run the plot to grab the trend.
      # ALSO: Suppress na.rm warnings that leak (na.rm is supported to silence warnings about hidden points due to axis limits)
      #suppressWarnings(cum_frequency_graph) 
      #ggsave(cum_frequency_graph, file="cum_frequency_graph.png")
      
      print(cum_frequency_graph)
      
      # Convert our trendline from log10 scale
      cum_frequency_trendline <- data.frame(x=fitx, y=10^fit) 
     
      # Use finite differences to approximate the derivatives of the smoothed terms
      # See http://en.wikipedia.org/wiki/Finite_difference
      dY <- diff(cum_frequency_trendline$y)/diff(cum_frequency_trendline$x)*-1  # the derivative of your function
      dX <- rowMeans(embed(cum_frequency_trendline$x,2)) # centers the X values for plotting
      
      cum_frequency_derivative <- data.frame(dX, dY, abs_max=(dY==max(dY))) %>% filter(dY>=0)
      cum_frequency_derivative.max_pt <- cum_frequency_derivative %>% filter(abs_max==TRUE)
      
      # Plot derivative + Maximum
      cum_frequency_graph <- cum_frequency_graph + geom_line(data=cum_frequency_derivative, 
                             aes(x=dX, y=log(dY), color="1st Der. * -1 (LC(s=0.7))"), size=1, alpha=0.5, na.rm = TRUE) +
                             geom_vline(xintercept=cum_frequency_derivative.max_pt$dX[1]) + 
                             geom_point(data=cum_frequency_derivative.max_pt,aes(x=dX, y=dY), fill="purple", size=5, shape=25, na.rm = TRUE) + 
                             geom_text(data=cum_frequency_derivative.max_pt, aes(x=dX, y=dY, label=paste0("Mc = ",round(dX, digits=3))),hjust=-0.25, vjust=0.5, color="purple")
                             
      return(cum_frequency_graph) 
    }
    # --> CE by time
    plotdata1sort2 <- plotdata1[with(plotdata1, order(datetime)), ]
    plotdata1sort2$events <- seq.int(nrow(plotdata1sort))

    #For stations/year graph
    #-> grads stations within the geo boundaries, that are active
    plotstations <- subset(stations.iris, 
                           as.numeric(format(start, "%Y")) >= input$bins[1]
                             & as.numeric(format(start, "%Y")) <= input$bins[2]
                             & as.numeric(format(end, "%Y")) >= input$bins[2]
                             & lat <= input$manlatmax & lat >= input$manlatmin
                             & lon <= input$manlonmax & lon >= input$manlonmin)

    #-> creates a dataframe with formatted station, start and end, without duplicates
    df <- plotstations[,c('sta','start', 'end')]
    df$start <- as.Date(df$start, "%Y")
    df$end <- as.Date(df$end, "%Y")
    
    mutate_each(df, funs(year(.)), start:end) -> temp
    
    #active <- sapply(1:nrow(temp), function(x){
    #  seq(temp[x, 2], temp[x, 3], by = 1)}) %>%
    #  unlist %>%
    #  table %>%
    #  data.frame
    
    #activeSta$Freq used to determine active stations for given year
    
    #disabled not meaningful for factors
    #activeSta <- subset(active, . >= input$bins[1] & . <= input$bins[2] )
    
    #Displays plot based on the selected radio button
    selectHisto <- function(histoParam){
      switch(histoParam,
             magvce = cum_function(),
             magvte = hist(plotdata2$emw, breaks = 8, main = "Num. of Events vs Magnitude", xlab="Magnitude", ylab="Events", col = 'darkblue', border='white'),
             cevt = plot(plotdata1sort2$datetime, plotdata1sort2$events, type="p", main = "Cumulative Num. of Events over Time", xlab = "Magnitude", ylab = "Cumulative Number", log="y"),
             tevd = hist(plotdata2$depth, breaks = 15, main = "Num. of Events vs Depth", xlab = "Depth", col = 'darkorange', border='white')
             #svy = barplot(activeSta$Freq, names.arg = activeSta$.)
             )
    }
    
    return (selectHisto(input$histoParam))
  })
  
  
})
