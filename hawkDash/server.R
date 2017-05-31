library(shiny)

load('enrollment.rdata')

# Define server logic
shinyServer(function(input, output, session) {

  
#-----------------------ENROLLMENT DASH-----------------------------------------
  enrollment <- reactive({
    
    # Determine filter columns, subject by default.
    prog <- input$progTypeE
    filtCol <- 'subject'
    filtCol[prog == 'Special Programs'] <- input$specialE
    oldNames <- names(enroll)
    names(enroll)[names(enroll) == filtCol] <- 'filt'
    names(enroll)
    
    # Determine the group by factors, if "None" is selected only put term
    demo <- input$demoE
    dots <- c("term", demo)
    dots <- dots[!dots == 'None']
    
    # Determine terms and make a regex pattern
    terms <- input$termE
    terms[is.null(terms)] <- 'None'
    if (length(terms) > 1) {
      terms <- paste(terms[1], "|", terms[2], sep = '')
    }
    
    # Calculate collegewide by default
    temp <- enroll %>%
      subset(seq_along(term) %in% grep(terms, term)) %>%
      group_by_(.dots = dots) %>%
      summarise(Duplicated = n(), Unduplicated = n_distinct(emplid)) %>%
      mutate(Proportion = Unduplicated/sum(Unduplicated) * 100)
    
    # Save the collegewide
    college <- temp
    
    # If program is selected do this disag
    if (input$collegeE != 'Collegewide') {
      temp <- enroll %>%
        subset(seq_along(term) %in% grep(terms, term) &
                 (filt %in% input$acadE  | input$specialE == filt)) %>%
        group_by_(.dots = dots) %>%
        summarise(Duplicated = n(), Unduplicated = n_distinct(emplid)) %>%
        mutate(Proportion = Unduplicated/sum(Unduplicated) * 100)
    }
    
    if (input$demoE != 'None') {
      names(temp)[2] <- 'demo_col'
      names(college)[2] <- 'demo_col'
    }
    
    if (input$compareE == 'Yes' & input$demoE != 'None') {
      temp <- temp %>%
        left_join(college, 
                  by = c('term' = 'term', 'demo_col' = 'demo_col')) %>%
        mutate(Proportion = Proportion.x/Proportion.y * 100) %>%
        select(-c(Proportion.x, Duplicated.y, Unduplicated.y, Proportion.y)) %>%
        rename(Unduplicated = Unduplicated.x)
    }
    
    
    if (input$demoE =='None') {
      temp <- gather(temp, 'Type', 'Enrollment', 2:3) %>%
        select(-Proportion)
      temp$termCont <- as.numeric(temp$term)
    }
    
    # Suppress small Ns
    print(temp)
    
    if (length(temp[,1]) > 0 & input$demoE != 'None') {
      temp <- temp[temp$Unduplicated >= 10, ]
    }
    print(temp)
    
    temp
  })
  
  
  #output
  output$histE <- renderChart({
    if(input$demoE == 'None') {
      n1 <- nPlot(Enrollment ~ termCont, group = "Type", 
                  data = enrollment(), 
                  type = "lineChart",
                  width = session$clientData[["output_plot2_width"]])
      
      # Create javascript code to modify x ticks (Wacky)
      x <- unique(enroll$term)
      y <- ''
      for (i in x) {
        if (y == '') {
          y <- paste("'", i, "'")
        } else {
          y <- paste(y, ",'", i, "'")
        }
      }
      
      codeForm <- paste("#!function(x) {keys = [", y, "]","
                    return keys[x-1]}!#", sep = '')
      
      x <- unique(as.numeric(enrollment()$term))
      y <- ''
      for (i in x) {
        if (y == '') {
          y <- paste("'", i, "'")
        } else {
          y <- paste(y, ",'", i, "'")
        }
      }
      
      codeVal <- paste("#!function(x) {tickvalues = [", y, "]","
                    return tickvalues}!#", sep = '')
      
      # Execute code and set other features
      n1$xAxis(axisLabel = 'Term', tickFormat = codeForm, tickValues = codeVal, 
               width = 50)
      n1$chart(forceY = c(.9 * min(enrollment()$Enrollment),
                          1.1 * max(enrollment()$Enrollment)),
               margin = list(left = 63, bottom = 63, right = 63))
      n1$yAxis(axisLabel='Enrollment', width=50)
      n1$chart(color = c('blue', 'orange'), size = 5)
      n1$chart(tooltipContent = "#! function(key, x, y, e){ 
        return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
          x + ': ' + '<strong>' + y + '</strong>'
      } !#")
    }
    
    if (input$demoE != 'None') {
      n1 <- nPlot(Proportion ~ demo_col, group = "term", 
                  data = enrollment(), 
                  type = "multiBarChart",
                  width = session$clientData[["output_plot2_width"]])
      n1$chart(showControls = F, reduceXTicks = F)
      n1$chart(color = c('blue', 'orange', 'brown', 'green', 'red'),
               forceY = c(0,100))
      n1$chart(tooltipContent = "#! function(key, x, y){ 
        return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
          x + ': ' + '<strong>' + y + '%' + '</strong>'
      } !#")
      n1$yAxis(axisLabel = 'Proportion of Students (%)', width = 50)
    }
    
    if (input$demoE != 'None' & input$compareE == 'Yes') {
      n1 <- nPlot(Proportion ~ demo_col, group = "term", 
                  data = enrollment(), 
                  type = "multiBarChart",
                  width = session$clientData[["output_plot2_width"]])
      n1$chart(showControls = F, reduceXTicks = F)
      n1$chart(color = c('blue', 'orange', 'brown', 'green', 'red'),
               forceY = c(0,max(enrollment()$Proportion) + 10))
      n1$chart(tooltipContent = "#! function(key, x, y){ 
        return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
          x + ': ' + '<strong>' + y + '</strong>'
      } !#")
      n1$yAxis(axisLabel = 'Proportionality Index', width = 50)
    }
    
    n1$addParams(dom = 'histE')
    
    return(n1)
  })

    
#-----------------------SUCCESS DASH--------------------------------------------  
  success <- reactive({

    # Determine filter columns, subject by default.
    prog <- input$progTypeS
    filtCol <- 'subject'
    filtCol[prog == 'Special Programs'] <- input$specialS
    names(enroll)[names(enroll) == filtCol] <- 'filt'
    
    # Determine the group by factors, if "None" is selected only put term
    demo <- input$demoS
    dots <- c("term", demo)
    dots <- dots[!dots == 'None']
    
    # Determine terms and make a regex pattern
    terms <- input$termS
    terms[is.null(terms)] <- 'None'
    if (length(terms) > 1) {
      terms <- paste(terms[1], "|", terms[2], sep = '')
    }
    

    # Calculate collegewide by default
    temp <- enroll %>%
      subset(seq_along(term) %in% grep(terms, term)) %>%
      group_by_(.dots = dots) %>%
      summarise(Success = mean(success), num = sum(success), den = n()) %>%
      mutate(overallSuc = sum(num)/sum(den))

    # Store collegewide for comparisons
    college <- temp
    
    # If program is selected do this disag
    if (input$collegeS != 'Collegewide') {
      temp <- enroll %>%
        subset(seq_along(term) %in% grep(terms, term) & 
               (filt %in% input$acadS  | input$specialS == filt)) %>%
        group_by_(.dots = dots) %>%
        summarise(Success = mean(success), num = sum(success), den = n()) %>%
        mutate(overallSuc = sum(num)/sum(den))
    }
    
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
    
    # Suppress small Ns
    if (length(temp[,1]) > 0) {
      temp <- temp[temp$den >= 20, ]
    }
    
    temp$termcont <- as.numeric(temp$term)
    
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
        n1$chart(tooltipContent = "#! function(key, x, y){ 
        return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
          x + ': ' + '<strong>' + y + '%' + '</strong>'
        } !#")
      }

      if (input$compareS == 'Evaluate Equity') {
        n1$chart(forceY = c(0, max(success()$Success) + 10))
        n1$yAxis(axisLabel='Proportionality Index', width=50)
        n1$chart(tooltipContent = "#! function(key, x, y){ 
        return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
          x + ': ' + '<strong>' + y + '</strong>'
        } !#")
      }
      
      if (input$compareS == 'Compare to Collegewide') {
        n1$chart(forceY = c(-20,20))
        n1$yAxis(axisLabel =
                   'Program Success Rate (%) - Collegewide Success Rate (%)', 
                 width=50)
        n1$chart(tooltipContent = "#! function(key, x, y){ 
        return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
          x + ': ' + '<strong>' + y + '%' + '</strong>'
        } !#")
      }
      
      n1$addParams(dom = 'histS')
      n1$chart(color = c('blue', 'orange', 'brown', 'green', 'red'))
      

      
      return(n1)
      })

})
