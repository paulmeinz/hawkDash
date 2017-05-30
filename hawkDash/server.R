library(shiny)

load('enrollment.rdata')

# Define server logic
shinyServer(function(input, output, session) {
  
  success <- reactive({

    # Determine filter columns, subject by default.
    prog <- input$progTypeS
    filtCol <- 'subject'
    filtCol[prog == 'Special Programs'] <- input$specialS
    oldNames <- names(enroll)
    names(enroll)[names(enroll) == filtCol] <- 'filt'
    
    # Determine the group by factors, if "None" is selected only put term
    demo <- input$demoS
    dots <- c("term", demo)
    dots <- dots[!dots == 'None']

    # Calculate collegewide by default
    temp <- enroll %>%
      subset(term %in% input$termS) %>%
      group_by_(.dots = dots) %>%
      summarise(Success = mean(success), num = sum(success), den = n()) %>%
      mutate(overallSuc = sum(num)/sum(den))

    # Store collegewide for comparisons
    college <- temp
    
    # If program is selected do this disag
    if (input$collegeS != 'Collegewide') {
      temp <- enroll %>%
        subset(term %in% input$termS & 
               (filt %in% input$acadS  | input$specialS == filt)) %>%
        group_by_(.dots = dots) %>%
        summarise(Success = mean(success), num = sum(success), den = n()) %>%
        mutate(overallSuc = sum(num)/sum(den))
    }
    
    # Restore old names so things don't get wacky.
    names(enroll) <- oldNames
    temp <- data.frame(temp)
    college <- data.frame(college)
    
    
    # Final manipulateions based on input
    if (input$demoS != 'None') {
      names(temp)[2] <- 'demo_col'
      names(college)[2] <- 'demo_col'
    }
    
    if (input$compareS == 'Evaluate Equity') {
      if (input$demoS != 'None') {
        temp$Success <- temp$Success/temp$overallSuc * 100
      }
      
      if (input$demoS == 'None') {
        temp$Success <- temp$Success/temp$Success * 100
      }
      
    }
    
    if (input$compareS == 'Compare to Collegewide') {
      if (input$demoS == 'None') {
        temp <- temp %>% 
          left_join(college, by = c('term' = 'term')) %>%
          mutate(Success = Success.x - Success.y) %>%
          rename(den = den.x)
      }
      
      if (input$demoS != 'None') {
        temp <- temp %>% 
          left_join(college, 
                    by = c('term' = 'term', 'demo_col' = 'demo_col')) %>%
          mutate(Success = Success.x - Success.y) %>%
          rename(den = den.x)
      } 
    }
    
    print(temp)
    
    # Suppress small Ns
    if (length(temp[,1]) > 0) {
      temp <- temp[temp$den >= 20, ]
    }
    
    print(temp)
    temp})

  
  
# output
    output$histS <- renderChart({
      if (input$demoS != 'None') {
        n1 <- nPlot(Success ~ demo_col, group = "term", 
                    data = success(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot1_width"]])
        n1$chart(showControls = F, reduceXTicks = F)
      }
      
      if (input$demoS == 'None') {
        n1 <- nPlot(Success ~ term, 
                    data = success(), 
                    type = "discreteBarChart",
                    width = session$clientData[["output_plot1_width"]])
      }
      
      # Set axis based on comparison
      if (input$compareS == 'None') {
        n1$chart(forceY = c(0,100))
        n1$yAxis(axisLabel='Course Success Rate (%)', width=50)
      }

      if (input$compareS == 'Evaluate Equity') {
        n1$chart(forceY = c(0, max(success()$Success) + 20))
        n1$yAxis(axisLabel='Proportionality Index', width=50)
      }
      
      if (input$compareS == 'Compare to Collegewide') {
        n1$chart(forceY = c(-20,20))
        n1$yAxis(axisLabel =
                   'Program Success Rate (%) - Collegewide Success Rate (%)', 
                 width=50)
      }
      
      n1$addParams(dom = 'histS')
      n1$chart(color = c('blue', 'orange', 'brown', 'green', 'red'))
      
      n1$chart(tooltipContent = "#! function(key, x, y, e){ 
        return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
          x + ': ' + '<strong>' + y + '%' + '</strong>'
      } !#")
      
      return(n1)
      })

})
