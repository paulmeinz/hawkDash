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

# Define server logic
shinyServer(function(input, output) {
  success <- reactive({

    # Determine filter columns, subject by default.
    prog <- input$progType
    filtCol <- 'subject'
    filtCol[prog == 'Special Programs'] <- input$special
    oldNames <- names(enroll)
    names(enroll)[names(enroll) == filtCol] <- 'filt'
    
    # Determine the group by factors, if "None" is selected only put term
    demo <- input$demo
    dots <- c("term", demo)
    dots <- dots[!dots == 'None']

    # Calculate collegewide by default
    temp <- enroll %>%
      subset(term %in% input$term) %>%
      group_by_(.dots = dots) %>%
      summarise(avg = mean(success))

    # If Special Program is selected do this
    if (input$college != 'Collegewide') {

      # Do the disag
      temp <- enroll %>%
        subset(term %in% input$term & 
               (filt %in% input$acad  | !is.na(filt))) %>%
        group_by_(.dots = dots) %>%
        summarise(avg = mean(success))
    }

    names(enroll) <- oldNames
    temp <- data.frame(temp)
    temp})

    output$table <- renderTable({success()})

})
