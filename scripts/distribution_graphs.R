library(tidyverse)
library(shiny)
library(stats)

# simple barplot

ui <- fluidPage(
  titlePanel("Poisson"),
  sliderInput(inputId = "mylambda", 
              label = "lambda",
              value = 10, min = 1, max = 100, step = 1),
  
  plotOutput(outputId = "reactivePlot"),
  textOutput(outputId = "varianceText"),
  titlePanel("Negative Binomial"),
  sliderInput(inputId = "mymu", 
              label = "mu",
              value = 10, min = 1, max = 100, step = 1),
  sliderInput(inputId = "mysize", 
              label = "size (dispersion parameter)",
              value = 10, min = 1, max = 100, step = 1),
  plotOutput(outputId = "reactivePlot2"),
  textOutput(outputId = "varianceText2")
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
  output$reactivePlot2 <- renderPlot({
    cur <- dnbinom(1:200, mu = input$mymu, size = input$mysize)
    barplot(cur, names.arg = 1:200, xlab = "count", ylab = "probability")
  })
  output$varianceText2 <- renderText({
    cur <- dnbinom(1:200, mu = input$mylambda)
    paste0("Variance:", round(var(cur), digits = 4))
  }
  )
}


shinyApp(ui = ui, server = server)


# visualize effect of exponential transform

mynumbers <- seq(-2, 3, .2)
mynumbers2 <- mynumbers - 2
plot(mynumbers, mynumbers2,
     type = "l") # linear relationship
abline(a = 0, b = 0, lty = 2, col = "grey")

plot(mynumbers, exp(mynumbers2), type = "l")
abline(a = 1, b = 0, lty = 2, col = "grey")



