
#install.packages("shiny")
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

#needs slider - slider is time, time slider throughout bounds. 1900 -> Present
#graph - graph is all points plotted, updates everytime slider is moved
##shows points where date is <= time (or use range slider)
##one slider, two graphs
## graph1 - all points plotted including line (lat and long + dots & mag color)
## graph2 - cluster analysis on graph (kmedoids clustering on it)

library(shiny)

shinyUI(pageWithSidebar(
  # Application title
  headerPanel("EES27 - Graphs Through Time"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("bins",
                "Time:",
                min = 1800,
                max = 2015,
                value = c(1800,2015),
                sep = "",
                step=5),
    actionButton("decrement_end_year", "End-date -5ys"),
    actionButton("increment_end_year", "End-date +5ys")
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    #dataset = data.frame(
    #  Longitude = c(35.5, 43.5),
    #  Magnitude = factor(dataset$emw),
    #  Latitude = c(-84,-71)
    #),
    h3(textOutput("caption")),
    plotOutput("plot"),
    plotOutput("plot2")
  )
))
