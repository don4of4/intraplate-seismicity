if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny", "rstudioapi", "shinyRGL")

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

# slider - slider is time, time slider throughout bounds. 1900 -> Present
# graph - graph is all points plotted, updates everytime slider is moved
# shows points where date is<= time (or use range slider)
# one slider, two graphs
# graph1 - all points plotted including line (lat and long + dots & mag color)
# graph2 - cluster analysis on graph (kmedoids clustering on it)

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
        sliderInput("study_year_span",
                    "Time:",
                    min = 1800,
                    max = 2016,
                    value = c(1800,2015),
                    sep = "",
                    step=5)
      )
    ),
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        h3("Additional Functions"),
        
        h4("Set Time Range"),
        actionButton("decrement_end_year", "End-date -1ys"),
        actionButton("increment_end_year", "End-date +1ys"),
        actionButton("decrement_start_year", "Start-date -1ys"),
        actionButton("increment_start_year", "Start-date +1ys"),
        br(), br(),
        
        numericInput("lower_year_bound", "Min Slider Value:", 1800, min = 1000, max = 2015, width='175px'),
        
        
        h4("Set Region"),
        h6("Manually enter coordinates to display"),
        
        h5("Latitude"),
        column( width=10, offset=1,
        fluidRow(
          div(style="display:inline-block", numericInput("manlatmin", "Min:", 35.5, min = 35.5, max = 43.5, width='75px')),
          div(style="display:inline-block", numericInput("manlatmax", "Max:", 43.5, min = 35.5, max = 43.5, width='75px'))
        )),
        br(),
        
        h5("Longitude"),
        column( width=10, offset=1,
        fluidRow(
          div(style="display:inline-block", numericInput("manlonmin", "Min:", -84, min = -84, max = -71, width='75px')),
          div(style="display:inline-block", numericInput("manlonmax", "Max:", -71, min = -84, max = -71, width='75px'))
        )),
        
        br(),
        
        conditionalPanel(condition="input.tabs == 'Statistics'", 
                         h4("Generate Statistics")),
        conditionalPanel(condition="input.tabs == 'Statistics'",
                         radioButtons("histoParam", "Select Graph:",
                                      c(
                                        '[G] Cumulative Events & Magnitude' = "magvce",
                                        '[G] Cumulative Events & Time' = "cevt",
                                        '[H] Stations vs Year' = "stations_vs_year",
                                        '[H] Num. of Events vs Depth' = "tevd",
                                        '[H] Num. of Events vs Magnitude' = "magvte"
                                        ),
                                      selected="magvce", inline=FALSE)),
        br(), br(),
        
        h4("Download Data"),
        selectInput("downloadset", "Choose a dataset:", 
                    choices = c("stations", "earthquakes")),
        downloadButton('downloadData', 'Download')
      ), #end sidebar panel
      
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
          tabPanel("Earthquakes Plot", plotOutput("plot2")),
          tabPanel("Density Plot (dbscan)", 
                   fluidRow(
                     div(style="display:inline-block", numericInput("minPts", "MinPts:", 20, min = 0, max = 1000, width='75px')),
                     div(style="display:inline-block", numericInput("eps", "eps:", 43, min = 0, max = 1000, width='75px'))
                     ),
                   plotOutput("plot4")
          ),
          tabPanel("3D Plot", plotOutput("plot5")),
          tabPanel("Statistics", plotOutput("histoPlot")),
          id="tabs"
        ), #end tab panel definitions
        htmlOutput("caption")
      )
    )

  )
)
