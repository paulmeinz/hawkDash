library(shiny)
library(shinyjs)

# Load enrollment and application data
load('enrollment.rdata')
load('access.rdata')

# INITIALIZE SOME THINGS--------------------------------------------------------


# Create matriculation data
matric <- enroll %>%
  filter(strm > 1143) # we dont have data in spring 14 or before

# Set Colors
colors <- c("#E87722", "#001A72", "#E69F00", "#009E73", "#999999", 
            "#F0E442", "#000000", "#56B4E9", "#CC79A7", "#999900")

# Compare options
compare <- c('No', 'Yes')

# Applicant Valid Demographics
demoA <- c('None', 'status', 'ethnicity', 'firstgen', 'foster', 'gender', 
           'calworks', 'dsps', 'eops', 'veteran')

# All Other Dash Valid Demographics
demo <- c(None = 'None', Age = 'age', 'Basic Skills' = 'basicskills',
          'Disability Status' = 'disability', Ethnicity = 'ethnicity',
          'Enrollment Status' = 'status', 'First Generation' = 'firstgen',
          'Foster Youth Status' = 'foster',
          'Gender' = 'gender','Online' = 'online', 'Veteran Status' = 'veteran'
)

# Define server logic
shinyServer(function(input, output, session) {


################################################################################
  
#                              URL PARAMETER SELECT
  
################################################################################

  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$tab)) {
      tab <- query$tab
      updateTabsetPanel(session, 'navbar', tab)
    }
  })  
  
################################################################################
  
#                              Access Dash
  
################################################################################

  
  # Reset comparisons if they are hidden
  observe({
    if (input$demoA == 'None') {
      reset('compareA')
    }
  })  
    
  # Create access dataset for plotting. I could do this inside a render* func
  # But this opens the door for other plots and/or an event reactive button
  # In the event of changes...
  acc <- reactive({
    
    # Determine the group by factors, if "None" is selected only use term
    demo <- input$demoA
    if (!(demo %in% demoA)) {
      demo <- 'None'
    }
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
             repAll = headcount/sum(headcount) * 100, # Rep in all applicants
             headcountAll = sum(headcount)) %>% # Collegewide headcount
      mutate(equity = repOut/repAll * 100) %>% # Calc equity index
      select(-c(tot))
    
    # Remove small sample sizes (less than 10) if temp is populated
    if (length(temp[, 1]) > 0) {
      temp <- temp[temp$headcount >= 10, ]
    }
    
    # Rename the demographic column for general plot function
    if (input$demoA != 'None') {
      names(temp)[2] <- 'demoCol'
    }
    
    # Remove small sample sizes (less than 10) if temp is populated
    if (length(temp[, 1]) > 0) {
      temp <- temp[temp$headcount >= 10, ]
    }
    
    # Make term numeric for line plot
    temp$termCont <- as.numeric(temp$term) 
    
    # Round for custom tooltips
    temp$repAll <- round(temp$repAll, 2)
    temp$repOut <- round(temp$repOut, 2)
    
    temp
  })

################################################################################    
#-----------------------------ACCESS-OUTPUT-------------------------------------
  
  output$defA <- renderUI({
    txt <- ''
    if (input$outcome == 'Applicant Counts') {
      if (input$demoA == 'None') {
        txt <- '<strong> Displaying data for applicant counts. MOUSE OVER 
          LINE TO VIEW EXACT COUNTS. </strong>'
      }
      if(input$demoA != 'None') {
        txt <- '<strong> Displaying the % representation of a demographic 
          among all applicants. MOUSE OVER BARS TO VIEW EXACT NUMBERS AND 
          HEADCOUNTS. 
          </strong>'
      }
    }
    if (input$outcome == '% of Applicants that Enroll') {
      txt <- '<strong> Displaying the percentage of applicants that enroll by
        term. Note that headcounts of enrollees will be highly correlated with
        but may not exactly match end of term enrollment counts (like those 
        displayed in the enrollment tab of this dashboard). This is because 
        additional data validation and cleaning happens after enrollment.
        MOUSE OVER BARS TO VIEW EXACT NUMBERS AND COUNTS. </strong>'

      if (input$compareA == 'Yes') {
        txt <- '<strong> Displaying proportionality indexes. In this case, the 
          proportionality index is calculated by taking a group&#39;s percent
          representation amongst enrolled applicants and dividing by the same 
          group&#39;s representation among all applicants.
          This ratio is multiplied by 100. A value below 100 means a 
          group is enrolling at lower rates than expected, perhaps indicating
          a barrier to access. MOUSE OVER BARS TO DISPLAY
          SPECIFIC VALUES. </strong>'
      }
    }
    
    HTML(txt)
  })
  
  output$helpA2 <- renderUI({
    term <- input$termA
    txt <- ''
    
    if (is.null(term)) {
      txt <- "<p class = 'help'> Select a term by checking the boxes
      above. Selecting both Fall and Spring will display data for
      fall and spring over five years.</p>"
    }
    
    HTML(txt)
  })
    
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
                 width = 50, rotateLabels = -70)
        n1$yAxis(axisLabel='Number of Applicants', width = 50)
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
        n1$yAxis(axisLabel='% of Applicants Who Enrolled', width = 50)
        n1$xAxis(rotateLabels = -25)
        n1$chart(forceY = c(0, 100), color = colors, tooltipContent = 
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
                 forceY = c(0, 100), 
                 tooltipContent = 
                 "#! 
                 function(key, x, y, e) { 
                   return '<p> <strong>' + key + '</strong> </p>' + 
                     '<p>' + x + ': <strong>' + y + '% </strong> </p>' +
                     '<p>' + 
                       e.point.headcount + ' out of </br>' + 
                       e.point.headcountAll + ' total applicants'
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
                   forceY = c(0, 100), 
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
                 function(key, x, y, e) { 
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
  
    
################################################################################
    
#                           Matriculation Dash
    
################################################################################ 
  
  
  # Reset comparisons if they are hidden
  observe({
    if (input$demoM == 'None') {
      reset('compareM')
    }
  })
  
  # Create matriculation dataset for plotting    
  matriculation <- reactive({
    
    # Determine the grouping factors, if "None" is selected only put term
    demo <- input$demoM
    dots <- c("term", demo)
    dots <- dots[dots != 'None', drop = F]
    
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
    
    # Determine which sssp elements should be used and calculate
    # which students have completed all the selected outcomes
    if (length(input$sssp) > 1) {
      x <- matric[,input$sssp]
      matric$Proportion <- rowSums(x)/length(input$sssp) # Won't work witn 1 col
      matric$Proportion[matric$Proportion < 100] <- 0
    } else {
      names(matric)[names(matric) == input$sssp] <- 'Proportion'
    }
    
    # If non selected than prop col with zero so the disag doesnt throw error
    if (is.null(input$sssp)) {
      matric$Proportion <- 0
    }
    
    # Disaggregate (Must do several left joins to eliminate duplication)
    temp <- matric %>%
      subset(seq_along(term) %in% grep(terms, term) &
             Proportion == 100) %>%
      group_by_(.dots = dots) %>%
      
      # N Completing outcomes by grp
      summarise(outGrp = n_distinct(emplid)) %>% 
      
      # The headcount by group (the denominator for the rate)
      left_join(x <- matric %>%
                  group_by_(.dots = dots) %>%
                  summarise(hcGrp = n_distinct(emplid))) %>%
      
      # Total completing the outcome collegewide (for equity calc)
      left_join(y <- matric %>%
                  subset(seq_along(term) %in% grep(terms, term) &
                         Proportion == 100) %>%
                  group_by(term)%>%
                  summarise(outTot = n_distinct(emplid))) %>%
      
      # Total headcount collegewide (for equity calc)
      left_join(z <- matric %>%
                  group_by(term) %>%
                  summarise(colTot = n_distinct(emplid))) %>%
      mutate(Prop = outGrp/hcGrp * 100, # Proportion completing
             outRep = outGrp/outTot * 100, # Outcome representation (equity num)
             colRep = hcGrp/colTot * 100) %>% # Colleg rep (equity den)
      mutate(Equity = outRep/colRep * 100)
    
    
    if (input$demoM != 'None') {
      names(temp)[2] <- 'demoCol'
    }
        
    # Round these numbers for equity plot tooltips
    temp$outRep <- round(temp$outRep, 2)
    temp$colRep <- round(temp$colRep, 2)
    
    temp
  })
  
################################################################################
#----------------------------Matric-Output--------------------------------------
  
  output$defM <- renderUI({
    txt <- ''
    txt <- '<strong>
      Displaying the percentage of enrolled students that completed ALL the 
      matriculation elements you selected (Assessment, Ed Plan, 
      Orientation, and/or Counseling) during or before a given term. For the
      purposes of this dashboard,
      Assessment, Ed Plan, and Orientation are counted districtwide, whereas
      counseling is counted only at CRC.
      MOUSE OVER BARS TO VIEW SPECIFIC NUMBERS AND COUNTS.
      </strong>'
    
    if(input$compareM == 'Yes') {
      txt <- ' <strong>
        Displaying proportionality indexes. In this case, the proportionality 
        index is calculated by taking a group&#39;s representation among the 
        students that completed the selected matriculation elements and 
        dividing by that group&#39;s representation on campus. This ratio is 
        multiplied by 100. A value below 100 means a particular group is
        completing matriculation elements at a lower rate than expected,
        perhapts indicating disproportionate impact. 
        MOUSE OVER BARS TO VIEW SPECIFIC NUMBERS. </strong>'
    }
    
    HTML(txt)
  })
  
  output$helpM2 <- renderUI({
    term <- input$termM
    txt <- ''
    
    if (is.null(term)) {
      txt <- "<p class = 'help'> Select a term by checking the boxes
        above. Selecting both Fall and Spring will display data for
        fall and spring over five years.</p>"
    }
    
    HTML(txt)
  })  
    
  output$histM <- renderChart({
    
    # Render this plot if no demos are selected
    if (input$demoM == 'None') {
      n1 <- nPlot(Prop ~ term,  
                  data = matriculation(), 
                  type = "discreteBarChart",
                  width = session$clientData[["output_plot3_width"]])
      
      n1$yAxis(axisLabel='% of Students Completing the Selected Outcomes',
               width = 50)
      n1$xAxis(rotateLabels = -25)
      n1$chart(forceY = c(0, 100), 
               color = colors,
                
               # Create custom tooltips
               tooltipContent = "#! 
               function(key, x, y, e) { 
                 return '<p>' + x + '</p>' +
                 '<p>' +
                   'Proportion of students: <strong>' + y + '% </strong>' +
                 '</p>' + 
                 '<P>' +
                   e.point.outGrp + ' out of ' + 
                   e.point.hcGrp + ' students.' +
                 '</p>'
               } !#")
    }
    
    # If demo is not None
    if (input$demoM != 'None') {
      
      # Make this plot with no comparisons
      if (input$compareE == 'No') {
        n1 <- nPlot(Prop ~ demoCol, group = "term", 
                    data = matriculation(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot3_width"]])
      
        n1$chart(forceY = c(0, 100), 
                 color = colors,
                 showControls = F, reduceXTicks = F, 
                 tooltipContent = "#! 
                 function(key, x, y, e) { 
                   return '<p>' + key + '</p>' +
                   '<p>' + 
                     x + ': <strong>' + y + '% </strong>' +
                   '</p>' + 
                   '<P>' +
                     e.point.outGrp + ' out of ' + 
                     e.point.hcGrp + ' students.' +
                   '</p>'
                 } !#")
      }
    
      # Make this plot with comparisons
      if (input$compareM == 'Yes') {
        n1 <- nPlot(Equity ~ demoCol, group = "term", 
                    data = matriculation(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot3_width"]])
      
        n1$chart(showControls = F, reduceXTicks = F,
                 color = colors,
                 forceY = c(0, max(matriculation()$Equity) + 10),
                 tooltipContent = "#! 
                 function(key, x, y, e) { 
                 return '<p> <strong>' + key + '</strong> </p>' + 
                   '<p>' + x + ': <strong>' + y + '</strong> </p>' + 
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
    }
    
    # Make the plot render
    n1$addParams(dom = 'histM')
    return(n1)
  })
  
    
################################################################################
    
#                           Enrollment Dash
  
################################################################################ 
  
  
  # Reset comparisons if they are hidden
  observe({
    if (input$demoE == 'None' | input$collegeE == 'Collegewide') {
      reset('compareE')
    }
  })  
  
  # Create enrollment dataset for plotting  
  enrollment <- reactive({
    
    # Determine filter columns, subject by default.
    prog <- input$collegeE
    filtCol <- 'subject'
    filtCol[prog == 'Student Support or Cohort Programs'] <- input$specialE
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
    temp <- enroll %>%
      subset(seq_along(term) %in% grep(terms, term)) %>%
      group_by_(.dots = dots) %>%
      summarise(duplicated = n(), unduplicated = n_distinct(emplid)) %>%
      
      # Left join collegwide unduplicated to use as the denom for rep
      # calculations. Otherwise sum(unduplicated) will be duplicated
      left_join(undup <- enroll %>%
                  subset(seq_along(term) %in% grep(terms, term)) %>%
                  group_by(term) %>%
                  summarise(undup = n_distinct(emplid))) %>%
      mutate(proportion = unduplicated/undup * 100)
      
    # Save the collegewide
    college <- temp
    
    # If program is selected do this disag (same as above but with program filt)
    if (input$collegeE != 'Collegewide') {
      temp <- enroll %>%
        subset(seq_along(term) %in% grep(terms, term) &
                 (filt %in% input$acadE  | input$specialE == filt)) %>%
        group_by_(.dots = dots) %>%
        summarise(duplicated = n(), unduplicated = n_distinct(emplid)) %>%
        
        # Left join again (see previous)
        left_join(undup <- enroll %>%
                    subset(seq_along(term) %in% grep(terms, term) &
                             (filt %in% input$acadE  | 
                                input$specialE == filt)) %>%
                    group_by(term) %>%
                    summarise(undup = n_distinct(emplid))) %>%
        mutate(proportion = unduplicated/undup * 100)
    }
    
    # If no demo is selected restructure the data to plot dup/undup
    if (input$demoE =='None') {
      temp <- gather(temp, 'type', 'enrollment', 2:3) %>%
        select(-proportion)

      # Rename levels for the purpose of tooltips
      if (nrow(temp) > 0) {
        temp[temp$type == 'unduplicated', 'type'] <- 'Unduplicated'
        temp[temp$type == 'duplicated', 'type'] <- 'Duplicated'
      }
      
      temp$termCont <- as.numeric(temp$term)
    }
    
    # If a demo is selected label the appropriate column (for general plot)
    if (input$demoE != 'None') {
      names(temp)[2] <- 'demoCol'
      names(college)[2] <- 'demoCol'
      
      # If compare is selected and demo calculated a prop index (equity)
      if (input$compareE == 'Yes') {
        temp <- temp %>%
          left_join(college, 
                    by = c('term' = 'term', 'demoCol' = 'demoCol')) %>%
          mutate(proportion = proportion.x/proportion.y * 100) %>%
          select(-c(duplicated.x, duplicated.y, unduplicated.y,
                    undup.x, undup.y)) %>%
          rename(unduplicated = unduplicated.x, progProp = proportion.x,
                 colProp = proportion.y)
        temp$progProp <- round(temp$progProp, 2)
        temp$colProp <- round(temp$colProp, 2)
      }
      
    }
    
    temp
  })
  
################################################################################  
#----------------------------Enrollment Output----------------------------------
 
  output$defE <- renderUI({
    txt <- '<strong> Displaying duplicated and unduplicated 
        headcounts. MOUSE OVER LINE TO VIEW SPECIFIC NUMBERS </strong>'
 
    if(input$demoE != 'None') {
      txt <- '<strong> Displaying the proportion of UNDUPLICATED headcount for
        the selected demographic collegwide. MOUSE OVER BARS TO VIEW RAW 
        NUMBERS. </strong>'
    }

    if(input$compareE == 'Yes') {
      txt <- '<strong> Displaying proportionality indexes for the selected 
        demographic in the selected program(s). The proportionality index
        is calculated by taking the representation of a demographic group
        in the selected program and dividing by that group&#39;s 
        representation collegewide. This ratio is multiplied by 100. A ratio 
        below 100 may indicate an access issue to the selected program 
        because a particular group is accessing the program at a lower
        rate than expected. MOUSE OVER BARS TO VIEW SPECIFIC VALUES.'
      }
    
    
    HTML(txt)
  })
  
  output$helpE1 <- renderUI({
    prog <- input$acadE
    col <- input$collegeE
    txt <- ''
    
    if (is.null(prog) & col == 'Academic Programs') {
      txt <- "<p class = 'help'> Select a program by clicking in the box above.
      You can type a subject prefix (e.g., MATH) or pick out of
      the menu. Picking multiple will combine results across programs.
      Delete selections with backspace. </p>"
    }
    
    HTML(txt)
  })
  
  
  output$helpE2 <- renderUI({
    term <- input$termE
    txt <- ''
    
    if (is.null(term)) {
      txt <- "<p class = 'help'> Select a term by checking the boxes
      above. Selecting both Fall and Spring will display data for
      fall and spring over five years.</p>"
    }
    
    HTML(txt)
  })
   
  output$histE <- renderChart({
    
    # General plot if no demo selected
    if(input$demoE == 'None') {
      n1 <- nPlot(enrollment ~ termCont, group = "type", 
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
      
      # Set min/max enrollment
      mn <- 0
      mx <- 0
      if (nrow(enrollment()) > 0) {
        mn <- floor(.9 * min(enrollment()$enrollment))
        mx <- floor(1.1 * max(enrollment()$enrollment))
      }
      
      # Execute code and set other features
      n1$xAxis(tickFormat = codeForm, tickValues = codeVal, 
               width = 50, rotateLabels = -70)
      n1$yAxis(axisLabel='Enrollment', width = 50)
      n1$chart(forceY = c(mn, mx), 
               margin = list(left = 63, bottom = 63, right = 63),
               color = colors, size = 5, 
               
               # Custom tooltip
               tooltipContent = "#! 
                 function(key, x, y, e) { 
                   return '<p> <strong>' + key + '</strong> </p>' + 
                     '<p>' + x + ': <strong>' + y + '</strong> </p>'
                 } !#")
    }
    
    # General plot if demo is selected
    if (input$demoE != 'None') {
      
      # If no comparison....
      if (input$compareE == 'No') {
        n1 <- nPlot(proportion ~ demoCol, group = "term", 
                    data = enrollment(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot2_width"]])
      
        n1$yAxis(axisLabel = 'Proportion of UNDUPLICATED Students (%)', 
                 width = 50)
        n1$chart(showControls = F, reduceXTicks = F, 
                 color = colors,
                 forceY = c(0, 100), 
                 tooltipContent = "#! 
                 function(key, x, y, e) { 
                   return '<p> <strong>' + key + '</strong> </p>' + 
                     '<p>' + 
                     x + ': <strong>' + y + '% </strong>' + 
                     '</p>' +
                     '<p>' + e.point.unduplicated + ' out of ' + 
                       e.point.undup + ' unduplicated students' + 
                       '<br/>' +
                       'in the selected program(s).'
                     '</p>'
                 } !#")
      }
      
      # General plot if compare is 'Yes'
      if (input$compareE == 'Yes') {
        n1 <- nPlot(proportion ~ demoCol, group = "term", 
                    data = enrollment(), 
                    type = "multiBarChart",
                    width = session$clientData[["output_plot2_width"]])
        
        n1$yAxis(axisLabel = 'Proportionality Index', width = 50)
        n1$chart(showControls = F, reduceXTicks = F,
                 color = colors,
                 forceY = c(0, max(enrollment()$proportion) + 10),
                 tooltipContent = "#! 
                 function(key, x, y, e) { 
                   return '<p> <strong>' + key + '</strong> </p>' + 
                   '<p>' + x + ': <strong>' + y + '</strong> </p>' + 
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
      }
    }
    
    # Make the plot render
    n1$addParams(dom = 'histE')
    return(n1)
  })

    
################################################################################
  
#                             Success Dash
  
################################################################################  
  
  
  # Reset comparisons if they are hidden
  observe({
    if (input$collegeS == 'Collegewide') {
      reset('compareCol')
    }
    
    if (input$demoS == 'None') {
      reset('compareDem')
    }
  })
  
  # Create success dataset
  success <- reactive({
    
    # Determine filter columns, subject by default.
    prog <- input$collegeS
    filtCol <- 'subject'
    filtCol[prog == 'Student Support or Cohort Programs'] <- input$specialS
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
      summarise(suc = mean(success), num = sum(success), den = n()) %>%
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
        summarise(suc = mean(success), num = sum(success), den = n()) %>%
        mutate(overallSuc = sum(num)/sum(den), outProp = num/sum(num) * 100,
               progProp = den/sum(den) * 100)
    }
    
    # Final manipulations based on input
    if (input$demoS != 'None') {
      names(temp)[2] <- 'demoCol'
      names(college)[2] <- 'demoCol'
      
      # If equity comparison is slected divide by overall success
      if (input$compareDem == 'Yes') {
        temp$suc <- temp$suc/temp$overallSuc * 100
        temp$outProp <- round(temp$outProp, 2)
        temp$progProp <- round(temp$progProp, 2)
        
      }
    }
    
    # If compare to collegewide is selected subtract collegewide
    if (input$compareCol == 'Yes' & input$demoS == 'None') {
      temp <- temp %>% 
        left_join(college, by = c('term' = 'term')) %>%
        rename(Program = suc.x, Collegewide = suc.y, den = den.x) %>%
        select(-c(num.x, overallSuc.x, outProp.x, progProp.x,
                  num.y, den.y, overallSuc.y, outProp.y, progProp.y))
      
      temp <- gather(temp, 'type', 'suc', c(2, 4))
      temp$termCont <- as.numeric(temp$term)
      temp$suc <- round(temp$suc, 2)
      temp[temp$type == 'Program', 'type'] <- 'Selected Program'
    }

    temp
  })

################################################################################  
#-----------------------------Success-Output------------------------------------  
  
  output$defS <- renderUI ({
  txt <- '<strong> Displaying success rates. A success rate is
    calculated by counting the total number of A, B, C, and P grades and 
    dividing by the total number of enrollments (including W&#39;s). 
    MOUSE OVER BARS TO VIEW SPECIFIC NUMBERS AND COUNTS. </strong>'
  
  if (input$compareDem == 'Yes') {
    txt <- '<strong> Displaying proportionality indexes. 
      For course success, the proportionality index is calculated by taking 
      the (%) representation of a given group among successful enrollments 
      (A, B, C, or P grades) and dividing by the (%) representation of that 
      same group among all enrollments. This ratio is multiplied by 100. A 
      value below 100 may indicate disproportionate impact such that a student
      group may be succeeding at a lower rate than expected.
      MOUSE OVER BARS TO VIEW SPECIFIC VALUES. </strong>'
  }
    
    HTML(txt)
  })
  
  output$helpS1 <- renderUI({
    prog <- input$acadS
    col <- input$collegeS
    txt <- ''
    
    if (is.null(prog) & col == 'Academic Programs') {
      txt <- "<p class = 'help'> Select a program by clicking in the box above.
        You can type a subject prefix (e.g., MATH) or pick out of
        the menu. Picking multiple will combine results across programs.
        Delete selections with backspace. </p>"
    }
    
    HTML(txt)
  })
  
  
  output$helpS2 <- renderUI({
    term <- input$termS
    txt <- ''
    
    if (is.null(term)) {
      txt <- "<p class = 'help'> Select a term by checking the boxes
        above. Selecting both Fall and Spring will display data for
        fall and spring over five years.</p>"
    }
    
    HTML(txt)
    })
  
  
  output$histS <- renderChart({
    
    # General plot for when no demos selected
    if (input$demoS == 'None') {
      
      # If no comparison to College
      if (input$compareCol == 'No' | input$collegeS == 'Collegewide') {
        n1 <- nPlot(suc ~ term, 
                    data = success(), 
                    type = "discreteBarChart",
                    width = session$clientData[["output_plot1_width"]])
        
        n1$yAxis(axisLabel='Course Success Rate (%)', width = 50)
        n1$xAxis(rotateLabels = -25)
        n1$chart(forceY = c(0, 100), tooltipContent = "#! 
                 function(key, x, y, e) { 
                   return '<p> <strong>' + x + '</strong> </p>' + 
                   '<p>' + 
                     'Success Rate: <strong>' + y + '% </strong>' + 
                   '</p>' +
                   '<p>' +
                     e.point.num/100 + ' successful enrollments out of' +
                     '</br>' +
                     e.point.den + ' total enrollments.' +
                   '</p>'
                 } !#")
        }
      
      # If comparison to college
      if (input$compareCol == 'Yes' & input$collegeS != 'Collegewide') {
        n1 <- nPlot(suc ~ termCont, group = "type", 
                    data = success(), 
                    type = "lineChart",
                    width = session$clientData[["output_plot1_width"]])
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
        n1$xAxis(tickFormat = codeForm, tickValues = codeVal, 
                 rotateLabels = -70)
        n1$yAxis(axisLabel = 'Course Success Rate (%)', 
                 width = 50)
        n1$chart(forceY = c(0, 100),
                 margin = list(left = 63, bottom = 63, right = 63),
                 color = colors, size = 5, 
                 tooltipContent = "#! 
                 function(key, x, y, e) { 
                   return '<p> <strong>' + key + '</strong> </p>' + 
                     '<p>' + x + ': <strong>' + y + '% </strong>' + '</p>'
                 } !#")
      }
    }
    

    # General plot for when demos are selected
    if (input$demoS != 'None') {
      n1 <- nPlot(suc ~ demoCol, group = "term", 
                  data = success(), 
                  type = "multiBarChart",
                  width = session$clientData[["output_plot1_width"]])
      
      n1$chart(showControls = F, reduceXTicks = F)
      
      if(input$compareDem == 'No') {
        n1$yAxis(axisLabel='Course Success Rate (%)', width = 50)
        n1$chart(forceY = c(0, 100), tooltipContent = "#! 
                 function(key, x, y, e) { 
                   return '<p> <strong>' + key + ': </strong>' + x + '</p>' + 
                   '<p>' + 
                     'Success Rate: <strong>' + y + '% </strong>' + 
                   '</p>' +
                   '<p>' +
                     e.point.num/100 + ' successful enrollments out of' + 
                     '</br>' +
                     e.point.den + ' total enrollments.' +
                   '</p>'
                } !#")
      }
      
      if(input$compareDem == 'Yes') {
        
        n1$yAxis(axisLabel='Proportionality Index', width = 50)
        n1$chart(forceY = c(0, max(success()$suc) + 10),
                 tooltipContent = "#! 
                 function(key, x, y, e){ 
                 return '<p> <strong>' + key + '</strong> </p>' + 
                 '<p>' + x + ': <strong>' + y + '</strong> </p>' + 
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
    }
    
    # Make sure the plot displays
    n1$addParams(dom = 'histS')
    n1$chart(color = colors)
    return(n1)
    })
})
