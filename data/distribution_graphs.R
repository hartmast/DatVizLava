library(tidyverse)
library(shiny)

# simple barplot

ui <- fluidPage(
  titlePanel("Poisson"),
  sliderInput(inputId = "mylambda", 
              label = "lambda",
              value = 10, min = 1, max = 100, step = 1),
  
  plotOutput(outputId = "reactivePlot"),
  textOutput(outputId = "varianceText")
)

server <- function(input, output) {
  
  output$reactivePlot <- renderPlot({
    cur <- dpois(1:200, lambda = input$mylambda)
    barplot(cur, names.arg = 1:200, xlab = "count", ylab = "probability")
  })
  output$varianceText <- renderText({
    cur <- dpois(1:200, lambda = input$mylambda)
    paste0("Variance:", round(var(cur), digits = 4))
  })
}


shinyApp(ui = ui, server = server)
