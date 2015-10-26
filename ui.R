
install.packages("shiny")
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

shinyUI(
  fluidPage(
  # Application title
  titlePanel("East Atlantic Seismic Data Through Time"),
  
  h4("EES 293 Research Project by Donald Scott, Dean Kroker and Andrea Stiffelman"),
  
  br(),
  
  #Panel for slider
  fluidRow(
    wellPanel(
      sliderInput("bins",
                  "Time:",
                  min = 1800,
                  max = 2015,
                  value = c(1800,2015),
                  sep = "",
                  step=5)
      )
  ),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
  sidebarPanel(
    h3("Adjust:"),
    h4("Time Range"),
    actionButton("decrement_end_year", "End-date -5ys"),
    actionButton("increment_end_year", "End-date +5ys"),
    h4("Filter A"),
    #checkboxInput(inputId = NULL, "Filter 1", value = FALSE),
    #checkboxInput(inputId = NULL, "Filter 2", value = FALSE),
    h4("Filter B")
    #actionButton("decrement_end_year", "End-date -5ys")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    #dataset = data.frame(
    #  Longitude = c(35.5, 43.5),
    #  Magnitude = factor(dataset$emw),
    #  Latitude = c(-84,-71)
    #),
    tabsetPanel(
    tabPanel("Main Plot", 
             plotOutput("plot",
                        dblclick = "plot_dblclick",
                        brush = brushOpts(
                          id = "plot_brush",
                          resetOnNew = TRUE
                        )
                        )
             ),
    tabPanel("K-Medoids Plot", plotOutput("plot2")),
    tabPanel("Summary"),
    tabPanel("Table") #tableOutput("plot2")
    ),
    column(10, h3(textOutput("caption")), offset=1)
    )
  ),
  
  #Download Panel
  fluidRow(
     wellPanel(
#       #For downloading CSV textual output
#       downloadButton(downloadCSV, label = "Download Data as CSV", class = NULL),
#       #For downloading png visual output
#       downloadButton(downloadPNG, label = "Download Plots as Image", class = NULL)
     )
  )
  
  )
)
