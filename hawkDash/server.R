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
shinyServer(function(input, output, session) {
  
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

    # If program is selected...
    if (input$college != 'Collegewide') {

      # Do the disag
      temp <- enroll %>%
        subset(term %in% input$term & 
               (filt %in% input$acad  | input$special == filt)) %>%
        group_by_(.dots = dots) %>%
        summarise(avg = mean(success))
    }

    names(enroll) <- oldNames
    temp <- data.frame(temp)
    if (length(temp) >= 3) {names(temp)[2] <- 'demo_col'}
    temp})

    output$hist <- renderChart({
      if (length(success()) >= 3) {
        n1 <- nPlot(avg ~ demo_col, group = "term", 
                    data = success(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot1_width"]])
        n1$chart(showControls = F, reduceXTicks = F)
      }
      
      if (length(success()) <= 2) {
        n1 <- nPlot(avg ~ term, 
                    data = success(), 
                    type = "discreteBarChart",
                    width = session$clientData[["output_plot1_width"]])
      }
      
      n1$addParams(dom = 'hist')
      n1$chart(color = c('blue', 'orange', 'brown', 'green', 'red'))
      n1$chart(forceY = c(0,100))
      n1$chart(tooltipContent = "#! function(key, x, y, e){ 
      return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
      '<strong>' + y + '%' + '</strong>' +' in ' + x
      } !#")
      n1$yAxis(axisLabel='Course Success Rate (%)', width=50)
      return(n1)
      })

})
