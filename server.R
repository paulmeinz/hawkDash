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
  dividing by the total number of enrollments (including W&#39;s). MOUSE OVER 
  BARS TO VIEW SPECIFIC NUMBERS AND COUNTS.
  <br>
  <br>
  Note: For academic programs, work experience enrollments are 
  counted under the "WEXP" subject code. 
  <br>
  <br>
  Note: Spring 2020 success rates may be an outlier for a given program because
  of increased EW and incomplete grades. As such, interpret this term with caution.
  </strong>'
  
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
