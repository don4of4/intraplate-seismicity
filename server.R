
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(datasets)
library(ggplot2)

shinyServer(function(input, output) {
  # Create a reactive text
  text <- reactive({
    paste(input$bins, '-')
  }) 
  
  # Return as text the selected variables
  #TODO: Accurately render each date
  output$caption <- renderText({
    text()
  })
  
  # Generate a plot of the requested variables
  output$plot <- renderPlot({
    #modifiedplot <- p + ggplot()
    myplot <- p
    print(myplot)
  })
})