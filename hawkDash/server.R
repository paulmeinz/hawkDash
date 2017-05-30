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
      summarise(Success = mean(success), num = sum(success), den = n()) %>%
      mutate(overallSuc = sum(num)/sum(den))

    # Store collegewide for comparisons
    college <- temp
    
    # If program is selected do this disag
    if (input$college != 'Collegewide') {
      temp <- enroll %>%
        subset(term %in% input$term & 
               (filt %in% input$acad  | input$special == filt)) %>%
        group_by_(.dots = dots) %>%
        summarise(Success = mean(success), num = sum(success), den = n()) %>%
        mutate(overallSuc = sum(num)/sum(den))
    }
    
    # Restore old names so things don't get wacky.
    names(enroll) <- oldNames
    temp <- data.frame(temp)
    college <- data.frame(college)
    
    
    # Final manipulateions based on input
    if (input$demo != 'None') {
      names(temp)[2] <- 'demo_col'
      names(college)[2] <- 'demo_col'
    }
    
    if (input$compare == 'Evaluate Equity') {
      if (input$demo != 'None') {
        temp$Success <- temp$Success/temp$overallSuc * 100
      }
      
      if (input$demo == 'None') {
        temp$Success <- temp$Success/temp$Success * 100
      }
      
    }
    
    if (input$compare == 'Compare to Collegewide') {
      if (input$demo == 'None') {
        temp <- temp %>% 
          left_join(college, by = c('term' = 'term')) %>%
          mutate(Success = Success.x - Success.y)
      }
      
      if (input$demo != 'None') {
        temp <- temp %>% 
          left_join(college, 
                    by = c('term' = 'term', 'demo_col' = 'demo_col')) %>%
          mutate(Success = Success.x - Success.y)
      } 
    }
    
    # Suppress small Ns
    if (length(temp[,1]) > 0) {
      temp[temp$den <= 20, 'Success'] <- -1
    }
    
    temp})

  
  
# output
    output$hist <- renderChart({
      if (input$demo != 'None') {
        n1 <- nPlot(Success ~ demo_col, group = "term", 
                    data = success(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot1_width"]])
        n1$chart(showControls = F, reduceXTicks = F)
      }
      
      if (input$demo == 'None') {
        n1 <- nPlot(Success ~ term, 
                    data = success(), 
                    type = "discreteBarChart",
                    width = session$clientData[["output_plot1_width"]])
      }
      
      n1$addParams(dom = 'hist')
      n1$chart(color = c('blue', 'orange', 'brown', 'green', 'red'))
      
      # Set axis based on comparison
      if (input$compare == 'None') {
        n1$chart(forceY = c(0,100))
        n1$yAxis(axisLabel='Course Success Rate (%)', width=50)
      }

      if (input$compare == 'Evaluate Equity') {
        n1$chart(forceY = c(0, max(success()$Success) + 20))
        n1$yAxis(axisLabel='Proportionality Index', width=50)
      }
      
      if (input$compare == 'Compare to Collegewide') {
        n1$chart(forceY = c(-20,20))
        n1$yAxis(axisLabel =
                   'Program Success Rate (%) - Collegewide Success Rate (%)', 
                 width=50)
      }
      
      n1$chart(tooltipContent = "#! function(key, x, y, e){ 
      if (y > -1) {
        return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
        x + ': ' + '<strong>' + y + '%' + '</strong>'
      }

      return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
        x + ': ' + '<strong>' + 'Sample size too small'+ '</strong>'
      } !#")
      return(n1)
      })

})
