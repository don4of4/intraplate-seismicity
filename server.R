if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny","datasets","ggplot2","scatterplot3d","ks","fpc","dplyr","lubridate","stats","scales","base","mapproj")
 

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
      if (selectedTab == "Stations Plot" || ( selectedTab == "Statistics" & histo == 'stations_vs_year')){unit <- " Stations" }
      return(unit)
    }
    captionQuant <- function(selectedTab, histo){
      quant <- nrow(plotdata)
      if (selectedTab == "Stations Plot" || ( selectedTab == "Statistics" & histo == 'stations_vs_year')){quant <- nrow(deduped.plotstations)}
      return(quant)
    } 
    
    
    #Formatted caption with proper quant & unit variable values
    paste(captionQuant(input$tabs, input$histoParam), captionUnit(input$tabs, input$histoParam), " from ", input$bins[1], '-',input$bins[2] )
  }) 
  
  # Return as text the selected variables
  output$caption <- renderUI({
    HTML(paste("<h3 style='text-align:center'>", text(), "</h3>"))
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
  observeEvent(input$lower_year_bound, {
    updateSliderInput(session, "bins", min=input$lower_year_bound)
  })
  
  #Stations Plot:
  
  output$plot <- renderPlot({
    
    plotdata <- subset(dataset, as.numeric(format(datetime, "%Y")) >= input$bins[1] & as.numeric(format(datetime, "%Y")) <= input$bins[2])
    plotstations <- subset(stations.iris, as.numeric(format(start, "%Y")) >= input$bins[1] & as.numeric(format(start, "%Y")) <= input$bins[2] 
                           & lat >= input$manlatmin & lat <= input$manlatmax 
                           & lon <= input$manlonmax & lon >= input$manlonmin)
    pp <- ggplot() + coord_map() +
      geom_polygon(aes(long,lat, group=group), fill="#a3dca3", colour="#a6a6a6", data=county) +
      geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
      geom_abline(intercept = 3, slope = -.45, color = "#cc99ff", size = 1) +
      annotate("rect", xmin=input$manlonmin, xmax=input$manlonmax, ymin=input$manlatmin, ymax=input$manlatmax, colour="black", size=1, fill=NA, alpha=".5") +
      geom_point(data=plotstations, size=4, alpha = .7, aes(x=lon, y=lat), color="yellow", shape=17) +
      theme(plot.background = element_rect(fill = 'grey')) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude")
      
    
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
    
    
    pp <- ggplot() + coord_map() +
      geom_polygon(aes(long,lat, group=group), fill="#a3dca3", colour="#a6a6a6", data=county) +
      geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="royalblue4", fill=NA) +
      geom_abline(intercept = 3, slope = -.45, color = "#cc99ff", size = 1) +
      annotate("rect", xmin=input$manlonmin, xmax=input$manlonmax, ymin=input$manlatmin, ymax=input$manlatmax, colour="black", size=1, fill=NA, alpha="0.5") +
      geom_point(data=plotdata, size=2, alpha = .7, aes(x=lon, y=lat, color=emw)) +
      scale_colour_gradientn(limits=c(1,6), colours=c("#540C8C","#1E0EFB","#FDFA00","#F8AF00","#F41F00")) + 
      theme(plot.background = element_rect(fill = 'grey')) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude")
      
    
    print(pp)
  })
  
  #DBScan Plot:
  output$plot4 <- renderPlot({
    plotdata <- subset(dataset, as.numeric(format(datetime, "%Y")) >= input$bins[1] & as.numeric(format(datetime, "%Y")) <= input$bins[2] 
                       & lat <= input$manlatmax & lat >= input$manlatmin
                       & lon <= input$manlonmax & lon >= input$manlonmin)
    coordinates=with(plotdata,data.frame(Longitude=lon,Latitude=lat,Depth=depth))
    
    calc_coordinates=with(plotdata,data.frame(Longitude=lon*100,Latitude=lat*100,Depth=depth))
    model=dbscan(calc_coordinates,MinPts=input$minPts,eps=input$eps)
    clusters=predict(model,calc_coordinates)+1
    with(coordinates,scatterplot3d(x=Longitude,y=Latitude,z=-Depth,color=clusters))
    
  })
  
  #3D Plot:
  output$plot5 <- renderPlot({
    plotdata <- subset(dataset, as.numeric(format(datetime, "%Y")) >= input$bins[1] & as.numeric(format(datetime, "%Y")) <= input$bins[2] 
                       & lat <= input$manlatmax & lat >= input$manlatmin
                       & lon <= input$manlonmax & lon >= input$manlonmin)
  
    calc_coordinates=with(plotdata,data.frame(Longitude=lon,Latitude=lat,Depth=-depth))
    precision=50
    d<<-kde(calc_coordinates,compute.cont=TRUE,gridsize=c(precision,precision,precision))
    plot(d,cont=(1:5)*1/5*100,drawpoints=TRUE)
    
  })
  
  
  #Histogram Plot
  
  output$histoPlot <-  renderPlot({ 
    options( warn = 0 )
    
    first_year <- input$bins[1]
    last_year <- input$bins[2]
      
    plotdata <- plotdata2 <- subset(dataset, as.numeric(format(datetime, "%Y")) >= first_year & as.numeric(format(datetime, "%Y")) <= last_year
      & lat <= input$manlatmax & lat >= input$manlatmin
      & lon <= input$manlonmax & lon >= input$manlonmin)
    
    plotstations <- subset(stations.iris, 
                           as.numeric(format(start, "%Y")) >= input$bins[1]
                           & as.numeric(format(start, "%Y")) <= input$bins[2]
                           & as.numeric(format(end, "%Y")) >= input$bins[2]
                           & lat <= input$manlatmax & lat >= input$manlatmin
                           & lon <= input$manlonmax & lon >= input$manlonmin)
  
    
    cum_frequency.mag.graph <- function(plotdata1){ 
      
      # --> Cum. Number mag
      plotdata1sort <- plotdata1[with(plotdata1, order(-emw)), ]
      plotdata1sort$events <- seq.int(nrow(plotdata1sort))
   
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
      
      a <- log10(length(plotdata1))
      
      # Plot derivative + Maximum
      cum_frequency_graph <- cum_frequency_graph + geom_line(data=cum_frequency_derivative, 
                             aes(x=dX, y=log(dY), color="1st Der. * -1 (LC(s=0.7))"), size=1, alpha=0.5, na.rm = TRUE) +
                             geom_vline(xintercept=cum_frequency_derivative.max_pt$dX[1]) + 
                             geom_point(data=cum_frequency_derivative.max_pt,aes(x=dX, y=dY), fill="purple", size=5, shape=25, na.rm = TRUE) + 
                             geom_text(data=cum_frequency_derivative.max_pt, aes(x=dX, y=dY, label=paste0("Mc = ",round(dX, digits=3))),hjust=-0.25, vjust=0.5, color="purple")
                             
      return(cum_frequency_graph) 
    }
    
    
    cum_frequency.time.graph <- function(plotdata){
      
      # Sort by time
      plotdata.sorted <- plotdata[with(plotdata, order(datetime)), ]
      
      # Add the cum. count to each row in order.
      plotdata.sorted$events <- seq.int(nrow(plotdata.sorted))
      
      cum_events_over_time.plot <- plot(plotdata.sorted$datetime, plotdata.sorted$events, type="p", main = "Cumulative Num. of Events over Time", 
                                         xlab = "Time", ylab = "Cumulative Number", log="y")
      return(cum_events_over_time.plot)
    }
    
    stations.year.hist <- function(plotstations, first_year, last_year){
      # (1) Place the data into a dataframe and convert the date
      plotstations$start <- as.Date(plotstations$start)
      plotstations$end <- as.Date(plotstations$end)
      
      # (2) Add additional col's simplifying the start and end year to only the year
      # (3) Group the stations by station name and start/end date (year only)
      # (4) Remove duplicates. Now we are left with duplicate rows for each station due to different channels OR gaps
      # (5) Select only the variables we want.
      
      station_data <- plotstations %>% mutate(start_year=year(start), end_year=ifelse(year(end) > year(Sys.Date()), year(Sys.Date()), year(end)) ) %>% 
        group_by(sta, start_year, end_year) %>% distinct() %>% select(sta, start_year, end_year)
      
      validate(
        need(length(station_data$sta) > 0, "No stations active in this time period.")
      )
      
      # (6) Iterate the station_data, expanding the date sequence, exploding it, then summarizing it to a freq. table.
        # Ineffecient way: do(data.frame(sta=.$sta, year=seq(from=.$start_year, to=.$end_year, by=1))) %>% select(sta, year) %>% distinct()
      # Fast way below.
      station_summary <- sapply(1:nrow(station_data), function(x){seq(station_data$start_year[x], station_data$end_year[x], by = 1)}) %>%
        unlist %>% data.frame(year=.) %>% filter(year >= first_year, year <= last_year) 
      
      freq_dist <- station_summary %>% group_by(year) %>% summarise(freq=n())
     
      validate(
        need(length(station_summary$year) > 0, "No stations with your parameters active in this time period.")
      )
      
      graph <- ggplot(station_summary, aes(x=year)) + geom_histogram(aes(fill=..count..), binwidth=1, colour="black") +
              coord_cartesian(xlim=c(min(station_summary$year), last_year+1), ylim=c(0,max(freq_dist$freq)+10)) +
              scale_y_continuous("Number of Stations", breaks=pretty_breaks(n=5)) +
              scale_x_continuous("Year", breaks=seq(min(station_summary$year),max(station_summary$year),5), 
                                 minor_breaks=seq(min(station_summary$year),max(station_summary$year),1)) +
              scale_fill_gradient("Count", low = "yellow", high = "green")
      return (graph)
      
    }
    
    events.depth.hist <- function(plotdata){
      return (hist(plotdata$depth, breaks = 15, main = "Num. of Events vs Depth", xlab = "Depth", col = 'darkorange', border='white'))
    }
    events.mag.hist <- function(plotdata){
      return (hist(plotdata$emw, breaks = 8, main = "Num. of Events vs Magnitude", xlab="Magnitude", ylab="Events", col = 'darkblue', border='white'))
    }
    
    #Displays plot based on the selected radio button
    selectHisto <- function(histoParam){
      switch(histoParam,
             magvce = cum_frequency.mag.graph(plotdata),
             cevt = cum_frequency.time.graph(plotdata),
             
             #magvte = plot(plotdata2$emw, log="y", type='h', lwd=10, lend=2, main = "Num. of Events vs Magnitude", xlab="Magnitude", ylab="Events", col = 'darkblue', border='white'),
            
             tevd = events.depth.hist(plotdata),
             magvte = events.mag.hist(plotdata),
             stations_vs_year = stations.year.hist(plotstations,first_year,last_year)
             )
    }
    
    return (selectHisto(input$histoParam))
  })
  
  
})
