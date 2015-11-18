if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny", "rstudioapi")

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
library(shinyRGL)

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
      #fluidRow(column=6,
      sidebarPanel(
        h3("Additional Functions"),
        
        h4("Set Time Range"),
        actionButton("decrement_end_year", "End-date -1ys"),
        actionButton("increment_end_year", "End-date +1ys"),
        
        br(), br(),
        
        h4("Set Region"),
        h6("Manually enter coordinates to display.."),
        #fluidRow( #column(width=6,
        h5("Latitude"),
        numericInput("manlatmin", "Min:", 35.5, min = 35.5, max = 43.5, width='75px'),
        #),
        #column(width=6,
        numericInput("manlatmax", "Max:", 43.5, min = 35.5, max = 43.5, width='75px'),
        #) #),
        h5("Longitude"),
        numericInput("manlonmin", "Min:", -84, min = -84, max = -71, width='75px'),
        numericInput("manlonmax", "Max:", -71, min = -84, max = -71, width='75px'),
        
        br(), br(),
        
        conditionalPanel(condition="input.tabs == 'Statistics'", 
                         h4("Generate Statistics")),
        conditionalPanel(condition="input.tabs == 'Statistics'",
                         radioButtons("histoParam", "Select Graph:",
                                      c("Cumulative Events vs Magnitude" = "magvce",
                                        "# of Events vs Magnitude" = "magvte",
                                        "Cumulative Events vs Time" = "cevt",
                                        "# of Events vs Depth" = "tevd",
                                        "Stations vs Year" = "svy"),
                                      selected="magvce", inline=FALSE)),
        #),
        h4("Download Data"),
        selectInput("downloadset", "Choose a dataset:", 
                    choices = c("stations", "earthquakes")),
        downloadButton('downloadData', 'Download')
        
        #actionButton("decrement_end_year", "End-date -5ys")
      ), #end sidebar panel
      
      #), #end fluid row
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          
          tabPanel("Stations Plot",
                   plotOutput(
                     "plot",
                     dblclick = "plot_dblclick",
                     brush = brushOpts(
                       id = "plot_brush",
                       resetOnNew = TRUE
                     )
                   )
          ),
          tabPanel("Eathquakes Plot", plotOutput("plot2")),
          tabPanel("Density Plot (dbscan)", 
                   numericInput("minPts", "MinPts:", 20, min = 0, max = 1000, width='75px'),
                   numericInput("eps", "eps:", 43, min = 0, max = 1000, width='75px'),
                   plotOutput("plot4")
          ),
          tabPanel("3D Plot", plotOutput("plot5")),
          tabPanel("Statistics", plotOutput("histoPlot")),
          id="tabs"
        ), #end tab panel definitions
        
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
