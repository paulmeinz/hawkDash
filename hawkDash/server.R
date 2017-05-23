#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

load('enrollment.rdata')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  temp <- reactive({
    demo <- input$demo
    dots <- lapply(list("term", demo), as.symbol)

    if (input$college == 'Collegewide' & input$demo != 'None') {
      temp <- enroll %>%
        subset(term %in% input$term) %>%
        group_by_(.dots = dots) %>%
        summarise(avg = mean(success))
    }

    else {temp <- data.frame(a = 1:4, b = 5:8, c = 1:4)}

    names(temp)[2] <- "demo_col"
    temp <- data.frame(temp)
    temp})

    output$table <- renderTable({temp()})

})
