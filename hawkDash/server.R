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
  success <- reactive({
    demo <- input$demo

    # Determine the group by factors, if none is selected only put term
    dots <- list("term", demo)
    dots <- dots[!dots == 'None']
    dots <- lapply(dots, as.symbol)


    temp <- enroll %>%
      subset(term %in% input$term) %>%
      group_by_(.dots = dots) %>%
      summarise(avg = mean(success))

    if (input$college != 'Collegewide' & input$progType == 'Academic Programs')
      {
      temp <- enroll %>%
        subset(term %in% input$term & subject %in% input$acad) %>%
        group_by_(.dots = dots) %>%
        summarise(avg = mean(success))
    }

    if (input$college != 'Collegewide' & input$progType == 'Special Programs') {
      newNames <- names(enroll)
      newNames[newNames == input$special] <- 'filt'
      names(enroll) <- newNames

      temp <- enroll %>%
        subset(term %in% input$term & !is.na(filt)) %>%
        group_by_(.dots = dots) %>%
        summarise(avg = mean(success))

      newNames <- names(enroll)
      newNames[newNames == 'filt'] <- input$special
      names(enroll) <- newNames
    }


    temp <- data.frame(temp)
    temp})

    output$table <- renderTable({success()})

})
