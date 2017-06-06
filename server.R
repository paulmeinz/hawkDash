library(shiny)

# Load enrollment and application data
load('enrollment.rdata')
load('access.rdata')

# INITIALIZE SOME THINGS--------------------------------------------------------


# Create matriculation data
matric <- enroll %>%
  filter(strm > 1143) # we dont have data in spring 14 or before

# Set Colors
colors <- c("#D55E00", "#0072B2", "#E69F00", "#009E73", "#999999", 
            "#F0E442", "#000000", "#56B4E9", "#CC79A7", "#999900")

# Define server logic
shinyServer(function(input, output, session) {

  
################################################################################
  
#                              ACCESS DASH
  
################################################################################
  
  
  acc <- reactive({
    
    # Determine the group by factors, if "None" is selected only use term
    demo <- input$demoA
    dots <- c("term", demo)
    dots <- dots[dots != 'None', drop = F]
    
    # Determine terms and make a regex pattern for filtering
    terms <- input$termA
    terms[is.null(terms)] <- 'None' # If non selected make terms 'None'
    if (length(terms) > 1) {
      terms <- paste(terms[1], "|", terms[2], sep = '')
    }
    
    # Filter Elk Grove only if yes selected to input
    if (input$egusd == 'Yes') {
      access <- access[access$egusd == 'egusd',]
    }
    
    # Disaggregate the access data  
    temp <- access %>%
      
      # Filter terms and group by selected factors and summarize
      subset(seq_along(term) %in% grep(terms, term)) %>%
      group_by_(.dots = dots) %>%
      summarise(headcount = n_distinct(emplid), # Headcount by grouping
                enrolled = mean(enroll), # Percent that enrolled by grouping
                tot = sum(enroll)) %>% # Overall number that enrolled
      
      # Create additional variables for Equity and custom tooltips
      mutate(repOut = tot/sum(tot) * 100, outTot = tot/100, # Rep in enrolled
             repAll = headcount/sum(headcount) * 100, # Rep at college
             headcountAll = sum(headcount)) %>% # Collegewide headcount
      mutate(equity = repOut/repAll * 100) %>% # Calc equity index
      select(-c(tot))
    
    # Remove small sample sizes (less than 10) if temp is populated
    if (length(temp[,1]) > 0 & input$demoA != 'None') {
      temp <- temp[temp$headcount >= 10, ]
    }
    
    # Rename the demographic column for general plot function
    if (input$demoA != 'None') {
      names(temp)[2] <- 'demoCol'
    }
    
    # Make term numeric for line plot
    temp$termCont <- as.numeric(temp$term) 
    
    # Round for custom tooltips
    temp$repAll <- round(temp$repAll, 2)
    temp$repOut <- round(temp$repOut, 2)
    
    temp
  })

    
#----------------------------------OUTPUT---------------------------------------

    
    output$histA <- renderChart({
      
      # FOr non demo plots..
      if (input$demoA == 'None') {
        
        # When applicant counts is selected
        if (input$outcome == 'Applicant Counts') {
          n1 <- nPlot(headcount ~ termCont,
                      data = acc(), 
                      type = "lineChart",
                      width = session$clientData[["output_plot4_width"]])
          
          # Create javascript code to modify x ticks (Ugly)
          # Could turn this into a helper function
          x <- unique(access$term)
          x <- x[order(x)]
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
          
          x <- unique(as.numeric(acc()$term))
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
          n1$xAxis(tickFormat = codeForm, tickValues = codeVal, 
                   width = 50, rotateLabels = -25)
          n1$yAxis(axisLabel='Number of Applicants', width=50)
          n1$chart(forceY = c(floor(.9 * min(acc()$headcount)),
                              floor(1.1 * max(acc()$headcount))),
                   margin = list(left = 63, bottom = 63, right = 63),
                   color = colors, size = 5,
                   showLegend = FALSE,
                   
                   # Tooltip function
                   tooltipContent = "#! 
                   function(key, x, y, e) { 
                     return  x + ': <strong>' + y + '</strong>' + ' applicants'
                   } !#")
          


        }
        
        # If percent applicants is selected
        if (input$outcome == '% of Applicants that Enroll') {
          n1 <- nPlot(enrolled ~ term,
                      data = acc(), 
                      type = "discreteBarChart",
                      width = session$clientData[["output_plot4_width"]])
          
          # Set plot features
          n1$yAxis(axisLabel='% of Applicants Who Enrolled', width=50)
          n1$xAxis(rotateLabels = -25)
          n1$chart(forceY = c(0,100), tooltipContent = 
          "#! 
          function(key, x, y, e) { 
            return '<p> <strong>' + x + '</strong> </p>' + 
              '<p> Enrollment Rate: <strong>' + y + '% </strong> </p>' +
              '<p>' +
                e.point.outTot + ' out of ' + e.point.headcount + '</br>' +
                'applicants enrolled after applying' +
              '</p>'
          } !#")
        }
      }
      
      # If demos are selected
      if (input$demoA != 'None') {
        
        # If applicant counts are selected
        if(input$outcome == 'Applicant Counts') {
          n1 <- nPlot(repAll ~ demoCol, group = "term", 
                      data = acc(), 
                      type = "multiBarChart",
                      width = session$clientData[["output_plot4_width"]])
        
          
          n1$yAxis(axisLabel = 'Proportion (%) of all applicants', 
                   width = 50)  
          n1$chart(showControls = F, reduceXTicks = F, 
                   color = colors,
                   forceY = c(0,100), 
                   tooltipContent = 
                   "#! 
                   function(key, x, y, e) { 
                     return '<p> <strong>' + key + '</strong> </p>' + 
                       '<p>' + x + ': <strong>' + y + '% </strong> </p>' +
                       '<p>' + 
                         e.point.headcount + ' out of ' + 
                         e.point.headcountAll + ' applicants </br>' +
                         'in this group.'
                       '</p>'
                   } !#")
        }
      
        # if outcome is percent of applicants then..
        if (input$outcome == '% of Applicants that Enroll') {
          
          # Dont calculate equity
          if (input$compareA == 'No') {
            n1 <- nPlot(enrolled ~ demoCol, group = "term", 
                        data = acc(), 
                        type = "multiBarChart",
                        width = session$clientData[["output_plot4_width"]])
        
            n1$yAxis(axisLabel = '% of Applicants Who Enrolled (%)', 
                     width = 50) 
            n1$chart(showControls = F, reduceXTicks = F, 
                     color = colors,
                     forceY = c(0,100), 
                     tooltipContent = 
                     "#! 
                     function(key, x, y, e) { 
                       return '<p> <strong>' + key + '</strong> </p>' + 
                         '<p>' + x + ': <strong>' + y + '% </strong> </p>' +
                         '<p>' + 
                            e.point.outTot + ' enrolled out of ' + 
                            e.point.headcount + '<br/>' +
                            'applicants in this group.'
                         '</p>'
                     } !#")
          }
        
          # Calculate equity
          if (input$compareA == 'Yes') {
            n1 <- nPlot(equity ~ demoCol, group = "term", 
                        data = acc(), 
                        type = "multiBarChart",
                        width = session$clientData[["output_plot4_width"]])
          
            n1$chart(showControls = F, reduceXTicks = F, 
                     color = colors,
                     forceY = c(floor(1.1 * max(acc()$equity)),
                                floor(.9 * min(acc()$equity))), 
                     tooltipContent = "#! 
                   function(key, x, y, e){ 
                   return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
                   '<p>' + 
                     x + ': ' + '<strong>' + y  + '</strong>' + 
                   '</p>' +
                   '<p>' + 
                     'This group constituted ' + e.point.repOut + '%' + 
                     '<br/>' +
                     'of the students that enrolled' +
                     '<br/>' +
                     'after applying' +
                     '<br/>' +
                     'and ' + e.point.repAll + '% of all applicants.' 
                   '</p>'
                   } !#")
          
            n1$yAxis(axisLabel = 'Proportionality Index', 
                     width = 50)  
          }
        }
      }  
      
      # Make sure the plot displays
      n1$addParams(dom = 'histA')
      return(n1)
      
      })
  
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
    
    # Filter out exempt students if they should not be included
    if (input$exempt == 'No') {
      matric <- matric[matric$exempt == 'not exempt',]  
    }
    
    # Determine which sssp elements should be used and calculate appropriately
    if (length(input$sssp) > 1) {
      x <- matric[,input$sssp]
      matric$Proportion <- rowSums(x)/length(input$sssp)
      matric$Proportion[matric$Proportion < 100] <- 0
    } else {
      names(matric)[names(matric) == input$sssp] <- 'Proportion'
    }
    
    if (is.null(input$sssp)) {
      matric$Proportion <- 0
    }
    
    # Disaggregate
    temp <- matric %>%
      subset(seq_along(term) %in% grep(terms, term) &
             Proportion == 100) %>%
      group_by_(.dots = dots) %>%
      summarise(outGrp = n_distinct(emplid)) %>%
      left_join(x <- matric %>%
                  group_by_(.dots = dots) %>%
                  summarise(hcGrp = n_distinct(emplid))) %>%
      left_join(y <- matric %>%
                  subset(seq_along(term) %in% grep(terms, term) &
                         Proportion == 100) %>%
                  group_by(term)%>%
                  summarise(outTot = n_distinct(emplid))) %>%
      left_join(z <- matric %>%
                  group_by(term) %>%
                  summarise(colTot = n_distinct(emplid))) %>%
      mutate(Prop = outGrp/hcGrp * 100, outRep = outGrp/outTot,
             colRep = hcGrp/colTot) %>%
      mutate(Equity = outRep/colRep * 100)
    
    
    if (input$demoM != 'None') {
      names(temp)[2] <- 'demo_col'
    }
    
    temp$outRep <- round(temp$outRep, 2)
    temp$colRep <- round(temp$colRep, 2)
    
    temp
  })
  
# MATRICULATION DASH OUTPUT   
    output$histM <- renderChart({
      if (input$compareM == 'No' | input$demoM == 'None') {
        n1 <- nPlot(Prop ~ term,  
                    data = matriculation(), 
                    type = "discreteBarChart",
                    width = session$clientData[["output_plot3_width"]])
        
        n1$chart(forceY = c(0,100), 
                 color = colors,
                 tooltipContent = "#! function(key, x, y, e){ 
                 return '<p>' + x + '</p>' +
                   '<p>' + 
                     'Proportion of students: ' + 
                     '<strong>' + y + '%' + '</strong>' +
                   '</p>' + 
                   '<P>' +
                     e.point.outGrp + ' out of ' + 
                     e.point.hcGrp + ' students.' +
                   '</p>'
                 } !#")
        
        n1$yAxis(axisLabel='% of Students Completing the Selected Outcomes',
                 width=50)
        n1$xAxis(rotateLabels = -25)
      }
      
      if (input$compareM == 'No' & input$demoM != 'None') {
        n1 <- nPlot(Prop ~ demo_col, group = "term", 
                    data = matriculation(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot3_width"]])
        
        n1$chart(forceY = c(0,100), 
                 color = colors,
                 showControls = F, reduceXTicks = F, 
                 tooltipContent = "#! function(key, x, y, e){ 
                 return '<p>' + key + '</p>' +
                 '<p>' + 
                   x + ': ' + '<strong>' + y + '%' + '</strong>' +
                 '</p>' + 
                 '<P>' +
                   e.point.outGrp + ' out of ' + 
                   e.point.hcGrp + ' students.' +
                 '</p>'
                 } !#")
      }
      
      if (input$compareM == 'Yes' & input$demoM != 'None') {
        n1 <- nPlot(Equity ~ demo_col, group = "term", 
                    data = matriculation(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot3_width"]])
        
        n1$chart(showControls = F, reduceXTicks = F,
                 color = colors,
                 forceY = c(0,max(matriculation()$Equity) + 10),
                 tooltipContent = "#! 
                 function(key, x, y, e){ 
                 return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
                   '<p>' + x + ': ' + '<strong>' + y + '</strong>' + '</p>' + 
                   '<p>' +
                     'This group constituted ' + e.point.outRep + '%' + 
                     '<br/>' + 
                     'of students completing the' +
                     '<br/>' +
                     'selected outcome(s) and' + 
                     '<br/>' +
                     e.point.colRep + '% of students collegewide.' + 
                     '<br/>'
                   '</p>'
                 } !#")
      }
      
      n1$addParams(dom = 'histM')
      return(n1)
    })
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
        select(-c(Duplicated.x, Duplicated.y, Unduplicated.y,
                  undup.x, undup.y)) %>%
        rename(Unduplicated = Unduplicated.x, progProp = Proportion.x,
               colProp = Proportion.y)
      temp$progProp <- round(temp$progProp, 2)
      temp$colProp <- round(temp$colProp, 2)
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
      x <- x[order(x)]
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
      n1$chart(forceY = c(floor(.9 * min(enrollment()$Enrollment)),
                          floor(1.1 * max(enrollment()$Enrollment))),
               margin = list(left = 63, bottom = 63, right = 63),
               color = colors, size = 5, 
               tooltipContent = "#! 
                 function(key, x, y, e){ 
                   return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
                   x + ': ' + '<strong>' + y + '</strong>'
                 } !#")
      
      n1$yAxis(axisLabel='Enrollment', width=50)
      n1$xAxis(axisLabel = 'Term', tickFormat = codeForm, tickValues = codeVal, 
               width = 50, rotateLabels = -25)
    }
    
    # General plot if demo is selected
    if (input$demoE != 'None') {
      n1 <- nPlot(Proportion ~ demo_col, group = "term", 
                  data = enrollment(), 
                  type = "multiBarChart",
                  width = session$clientData[["output_plot2_width"]])
      
      n1$chart(showControls = F, reduceXTicks = F, 
               color = colors,
               forceY = c(0,100), 
               tooltipContent = "#! 
               function(key, x, y, e){ 
                 return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
                   '<p>' + 
                   x + ': ' + '<strong>' + y + '%' + '</strong>' + 
                   '</p>' +
                   '<p>' + e.point.Unduplicated + ' out of ' + 
                   e.point.undup + ' unduplicated students' + '<br/>' +
                   'in the selected program(s).'
                   '</p>'
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
               color = colors,
               forceY = c(0,max(enrollment()$Proportion) + 10),
               tooltipContent = "#! 
               function(key, x, y, e){ 
                 return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
                   '<p>' + x + ': ' + '<strong>' + y + '</strong>' + '</p>' + 
                   '<p>' +
                     'This group constituted ' + e.point.progProp + '%' + 
                     '<br/>' + 
                     ' of students in the selected' + 
                     '<br/>' +
                     ' program(s) and ' + e.point.colProp + '% of students ' + 
                     '<br/>' +
                     'collegewide.' +
                   '</p>'
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
      mutate(overallSuc = sum(num)/sum(den), outProp = num/sum(num) * 100,
             progProp = den/sum(den) * 100)

    # Store collegewide for comparisons
    college <- temp
    
    # If program is selected do this disag
    if (input$collegeS != 'Collegewide') {
      temp <- enroll %>%
        subset(seq_along(term) %in% grep(terms, term) & 
               (filt %in% input$acadS  | input$specialS == filt)) %>%
        group_by_(.dots = dots) %>%
        summarise(Success = mean(success), num = sum(success), den = n()) %>%
        mutate(overallSuc = sum(num)/sum(den), outProp = num/sum(num) * 100,
               progProp = den/sum(den) * 100)
    }
    
    
    # Final manipulations based on input
    if (input$demoS != 'None') {
      names(temp)[2] <- 'demo_col'
      names(college)[2] <- 'demo_col'
    }
    
    # If equity comparison is slected divide by overall success
    if (input$compareDem == 'Yes' & input$demoS != 'None') {
      temp$Success <- temp$Success/temp$overallSuc * 100
      temp$outProp <- round(temp$outProp, 2)
      temp$progProp <- round(temp$progProp, 2)
      
    }
    
    # If compare to collegewide is selected subtract collegewide
    if (input$compareCol == 'Yes' & input$demoS == 'None') {
      temp <- temp %>% 
        left_join(college, by = c('term' = 'term')) %>%
        rename(Program = Success.x, Collegewide = Success.y, den = den.x) %>%
        select(-c(num.x, overallSuc.x, outProp.x, progProp.x,
                  num.y, den.y, overallSuc.y, outProp.y, progProp.y))
      temp <- gather(temp, 'Type', 'Success', c(2,4))
      temp$termCont <- as.numeric(temp$term)
      temp$Success <- round(temp$Success, 2)
    }
    
    # Suppress small Ns
    if (length(temp[,1]) > 0) {
      temp <- temp[temp$den >= 20, ]
    }
    
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
      if (input$demoS == 'None' & input$compareCol == 'No') {
        n1 <- nPlot(Success ~ term, 
                    data = success(), 
                    type = "discreteBarChart",
                    width = session$clientData[["output_plot1_width"]])
      }
      
      # When compare to college is selected
      if (input$demoS == 'None' & input$compareCol == 'Yes') {
      n1 <- nPlot(Success ~ termCont, group = "Type", 
                  data = success(), 
                  type = "lineChart",
                  width = session$clientData[["output_plot1_width"]])
      }
      
      # Set axis based on comparison
      if (input$compareDem == 'No' | input$demoS == 'None') {
        n1$chart(forceY = c(0,100))
        n1$yAxis(axisLabel='Course Success Rate (%)', width=50)
        n1$chart(tooltipContent = "#! function(key, x, y, e){ 
        return '<p>' + '<strong>' + x + '</strong>' + '</p>' + 
          '<p>' + 
            'Success Rate: ' + '<strong>' + y + '%' + '</strong>' + 
          '</p>' +
          '<p>' +
            e.point.num/100 + ' successful enrollments out of' + '</br>' +
            e.point.den + ' total enrollments.' +
          '</p>'
        } !#")
        n1$xAxis(rotateLabels = -25)
      }
      
      if (input$compareDem == 'No' & input$demoS != 'None') {
        n1$chart(forceY = c(0,100))
        n1$yAxis(axisLabel='Course Success Rate (%)', width=50)
        n1$chart(tooltipContent = "#! function(key, x, y, e){ 
                 return '<p>' + 
                   '<strong>' + key + ': ' + '</strong>' + x +
                 '</p>' + 
                 '<p>' + 
                   'Success Rate: ' + '<strong>' + y + '%' + '</strong>' + 
                 '</p>' +
                 '<p>' +
                   e.point.num/100 + ' successful enrollments out of' + 
                   '</br>' +
                   e.point.den + ' total enrollments.' +
                 '</p>'
      } !#")
      }

      if (input$compareDem == 'Yes' & input$demoS != 'None') {
        n1$chart(forceY = c(0, max(success()$Success) + 10))
        n1$yAxis(axisLabel='Proportionality Index', width=50)
        n1$chart(tooltipContent = "#! 
               function(key, x, y, e){ 
                 return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
                 '<p>' + x + ': ' + '<strong>' + y + '</strong>' + '</p>' + 
                 '<p>' +
                    'This group constituted ' + e.point.outProp + '%' + 
                   '<br/>' + 
                   ' of successful enrollments and ' + 
                   '<br/>' +
                   e.point.progProp + '% of enrollments ' + 
                   '<br/>' +
                   'in the selected program(s).' +
                 '</p>'
               } !#")
      }
      
      if (input$compareCol == 'Yes' & input$demoS == 'None') {
        n1$chart(forceY = c(-20,20))
        n1$yAxis(axisLabel =
                   'Program Success Rate (%)', 
                 width=50)
        
        # Create javascript code to modify x ticks (Wacky)
        x <- unique(enroll$term)
        x <- x[order(x)]
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
        
        x <- unique(as.numeric(success()$term))
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
        n1$chart(forceY = c(0, 100),
                 margin = list(left = 63, bottom = 63, right = 63),
                 color = colors, size = 5, 
                 tooltipContent = "#! 
                 function(key, x, y, e){ 
                   return '<p>' + '<strong>' + key + '</strong>' + '</p>' + 
                   x + ': ' + '<strong>' + y + '%' + '</strong>'
                 } !#")
        
        n1$xAxis(tickFormat = codeForm, tickValues = codeVal, 
                 rotateLabels = -25)
      }
      
      # Make sure the plot displays
      n1$addParams(dom = 'histS')
      n1$chart(color = colors)
      
      return(n1)
      })

})
