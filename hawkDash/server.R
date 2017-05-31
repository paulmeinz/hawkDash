library(shiny)

load('enrollment.rdata')

# Define server logic
shinyServer(function(input, output, session) {

  
#-----------------------MATRICULATION DASH--------------------------------------  
  matriculation <- reactive({
  
    # Determine the group by factors, if "None" is selected only put term
    demo <- input$demoM
    dots <- c("term", demo)
    dots <- dots[!dots == 'None', drop = F]
    
    # Determine terms and make a regex pattern for filtering
    terms <- input$termM
    terms[is.null(terms)] <- 'None'
    if (length(terms) > 1) {
      terms <- paste(terms[1], "|", terms[2], sep = '')
    }
    
    # Remove columns of the data that cause duplication IN terms
    data <- enroll %>%
      select(-class_rec_key, -subject, -success)
    data <- unique(data)
    
    # Determine which sssp elements should be used and calculate appropriately
    if (length(input$sssp) > 1) {
      x <- data[,input$sssp]
      data$Proportion <- rowSums(x)/length(input$sssp)
      data$Proportion[data$Proportion < 100] <- 0
    } else {
      names(data)[names(data) == input$sssp] <- 'Proportion'
    }
    
    # Disaggregate
    temp <- data %>%
      subset(seq_along(term) %in% grep(terms, term)) %>%
      group_by_(.dots = dots) %>%
      summarise(Proportion = mean(Proportion))
    
    
    
    print(temp)
    'hello world'    
  })
  
    output$histM <- renderText({matriculation()})
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
    
    # Determine terms and make a regex pattern for filtering
    terms <- input$termE
    terms[is.null(terms)] <- 'None'
    if (length(terms) > 1) {
      terms <- paste(terms[1], "|", terms[2], sep = '')
    }
    
    # Calculate collegewide enrollment by default
    # Have to do some voodoo magic in the left join because numbers will be
    # duplicated otherwise
    temp <- enroll %>%
      subset(seq_along(term) %in% grep(terms, term)) %>%
      group_by_(.dots = dots) %>%
      summarise(Duplicated = n(), Unduplicated = n_distinct(emplid)) %>%
      left_join(undup <- enroll %>%
                  subset(seq_along(term) %in% grep(terms, term)) %>%
                  group_by(term) %>%
                  summarise(undup = n_distinct(emplid))) %>%
      mutate(Proportion = Unduplicated/undup * 100)
      
    
    # Save the collegewide
    college <- temp
    
    # If program is selected do this disag (same as above but with program filt)
    if (input$collegeE != 'Collegewide') {
      temp <- enroll %>%
        subset(seq_along(term) %in% grep(terms, term) &
                 (filt %in% input$acadE  | input$specialE == filt)) %>%
        group_by_(.dots = dots) %>%
        summarise(Duplicated = n(), Unduplicated = n_distinct(emplid)) %>%
        left_join(undup <- enroll %>%
                    subset(seq_along(term) %in% grep(terms, term) &
                             (filt %in% input$acadE  | 
                                input$specialE == filt)) %>%
                    group_by(term) %>%
                    summarise(undup = n_distinct(emplid))) %>%
        mutate(Proportion = Unduplicated/undup * 100)
    }
    
    # If a demo is selected label the appropriate column (for general plot)
    if (input$demoE != 'None') {
      names(temp)[2] <- 'demo_col'
      names(college)[2] <- 'demo_col'
    }
    
    # If compare is selected and demo is not none calculated a prop index
    if (input$compareE == 'Yes' & input$demoE != 'None') {
      temp <- temp %>%
        left_join(college, 
                  by = c('term' = 'term', 'demo_col' = 'demo_col')) %>%
        mutate(Proportion = Proportion.x/Proportion.y * 100) %>%
        select(-c(Proportion.x, Duplicated.y, Unduplicated.y, Proportion.y)) %>%
        rename(Unduplicated = Unduplicated.x)
    }
    
    # If no demo is selected restructure the data to plot dup/undup
    if (input$demoE =='None') {
      temp <- gather(temp, 'Type', 'Enrollment', 2:3) %>%
        select(-Proportion)
      temp$termCont <- as.numeric(temp$term)
    }
    
    # Suppress small Ns so that prop indexes arent outliers.
    if (length(temp[,1]) > 0 & input$demoE != 'None') {
      temp <- temp[temp$Unduplicated >= 10, ]
    }
    
    temp
  })
  
  
  
  # ENROLLMENT DASH OUTPUT
  
  output$histE <- renderChart({
    
    # General plot if no demo selected
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
      n1$chart(forceY = c(.9 * min(enrollment()$Enrollment),
                          1.1 * max(enrollment()$Enrollment)),
               margin = list(left = 63, bottom = 63, right = 63),
               color = c('blue', 'orange'), size = 5, 
               tooltipContent = "#! 
                 function(key, x, y, e){ 
                   return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
                   x + ': ' + '<strong>' + y + '</strong>'
                 } !#")
      
      n1$yAxis(axisLabel='Enrollment', width=50)
      n1$xAxis(axisLabel = 'Term', tickFormat = codeForm, 
               tickValues = codeVal, 
               width = 50)
    }
    
    # General plot if demo is selected
    if (input$demoE != 'None') {
      n1 <- nPlot(Proportion ~ demo_col, group = "term", 
                  data = enrollment(), 
                  type = "multiBarChart",
                  width = session$clientData[["output_plot2_width"]])
      
      n1$chart(showControls = F, reduceXTicks = F, 
               color = c('blue', 'orange', 'brown', 'green', 'red'),
               forceY = c(0,100), 
               tooltipContent = "#! 
               function(key, x, y){ 
                 return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
                 x + ': ' + '<strong>' + y + '%' + '</strong>'
               } !#")
      
      n1$yAxis(axisLabel = 'Proportion of UNDUPLICATED Students (%)', 
               width = 50)
    }
    
    # General plot if compare is 'Yes'
    if (input$demoE != 'None' & input$compareE == 'Yes') {
      n1 <- nPlot(Proportion ~ demo_col, group = "term", 
                  data = enrollment(), 
                  type = "multiBarChart",
                  width = session$clientData[["output_plot2_width"]])
      
      n1$chart(showControls = F, reduceXTicks = F,
               color = c('blue', 'orange', 'brown', 'green', 'red'),
               forceY = c(0,max(enrollment()$Proportion) + 10),
               tooltipContent = "#! 
               function(key, x, y){ 
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
    
    # Determine terms and make a regex pattern for filtering later
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
    
    # If equity comparison is slected divide by overall success
    if (input$compareS == 'Evaluate Equity') {
      if (input$demoS != 'None') {
        temp$Success <- temp$Success/temp$overallSuc * 100
      }
      
      if (input$demoS == 'None') {
        temp$Success <- temp$Success/temp$Success * 100
      }
      
    }
    
    # If compare to collegewide is selected subtract collegewide
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

  
  
# SUCCESS DASH OUTPUT
  
    output$histS <- renderChart({
      
      # General plot for when demos are selected
      if (input$demoS != 'None') {
        n1 <- nPlot(Success ~ demo_col, group = "term", 
                    data = success(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot1_width"]])
        
        n1$chart(showControls = F, reduceXTicks = F)
      }
      
      # General plot for when no demos selected
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
      
      # Make sure the plot displays
      n1$addParams(dom = 'histS')
      n1$chart(color = c('blue', 'orange', 'brown', 'green', 'red'))
      
      return(n1)
      })

})
