library(shiny)
library(htmltools)
library(dplyr)
library(magrittr)

ui <- shinyUI(fluidPage(

  mainPanel(
      imageOutput("pdfview")
      )

))

server <- shinyServer(function(input, output) {


     output$pdfview  <- renderImage({
      
       # Return a list
       list(src = "test.jpg")
     })

  })



shinyApp(ui = ui, server = server)