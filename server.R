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
demo <- c(None = 'None', Age = 'age', 
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

  #-----------Reactive UX ------------------------------------------------------
  
  # toggle program select
  observe({
    if(input$collegeS == 'Academic Programs')
    {show(id = 'acadSpop', anim = TRUE)}
    if(input$collegeS != 'Academic Programs')
    {hideElement(id = 'acadSpop', anim = TRUE)}
  })
  
  # toggle enrollment subject help
  observe({
    if(input$collegeS == 'Academic Programs' & is.null(input$acadS))
    {show(id = 'helpS1pop', anim = TRUE)}
    if(input$collegeS == 'Academic Programs' & !is.null(input$acadS)|
       input$collegeS != 'Academic Programs')
    {hideElement(id = 'helpS1pop', anim = TRUE)}
  })
  
  # toggle special programs select
  observe({
    if(input$collegeS == 'Student Support or Cohort Programs')
    {show(id = 'specialSpop', anim = TRUE)}
    if(input$collegeS != 'Student Support or Cohort Programs')
    {hideElement(id = 'specialSpop', anim = TRUE)}
  })
  
  # toggle term help
  observe({
    if(is.null(input$termS))
    {show(id = 'helpS2pop', anim = TRUE)}
    if(!is.null(input$termS))
    {hideElement(id = 'helpS2pop', anim = TRUE)}
  })
  
  # toggle equity eval
  observe({
    if(input$demoS != 'None')
    {show(id = 'compareDempop', anim = TRUE)}
    if(input$demoS == 'None')
    {hideElement(id = 'compareDempop', anim = TRUE)}
  })

  # toggle college compare
  observe({
    if((input$collegeS == 'Academic Programs' & !is.null(input$acadS) |
       input$collegeS == 'Student Support or Cohort Programs')
       & input$demoS == 'None')
    {show(id = 'compareColpop', anim = TRUE)}
    print(input$collegeS == 'Academic Programs' & !is.null(input$acadS) |
            input$collegeS == 'Student Support or Cohort Programs')
    if(input$demoS != 'None' | input$collegeS == 'Collegewide' |
       is.null(input$acadS))
    {hideElement(id = 'compareColpop', anim = TRUE)}
    print(input$demoS != 'None' | input$collegeS == 'Collegewide' |
            is.null(input$acadS))
  })
  
  # Reset comparisons if they are hidden
  observe({
    if (input$collegeS == 'Collegewide') {
      reset('compareCol')
    }

    if (input$demoS == 'None') {
      reset('compareDem')
    }
  })
  
  #-----------------------------------------------------------------------------

  # Create success dataset
  success <- reactive({

    # Determine filter columns, subject by default.
    prog <- input$collegeS

    if (is.null(input$acadS)) {
      filtCol = 'subject'

    } else if ('WEXP' %in% input$acadS) {
      filtCol <- 'wexp'

    } else {
      filtCol = 'subject'
    }

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
      group_by(.dots = dots) %>%
      summarise(suc = mean(success), num = sum(success), den = n()) %>%
      mutate(overallSuc = sum(num)/sum(den), outProp = suc,
             progProp = sum(num)/sum(den))

    # Store collegewide for comparisons
    college <- temp

    # If program is selected do this disag
    if (input$collegeS != 'Collegewide') {

      if (input$collegeS == 'Academic Programs' & is.null(input$acadS)) {
        temp <- college

      } else {
      temp <- enroll %>%
        subset(seq_along(term) %in% grep(terms, term) &
               (filt %in% input$acadS  | input$specialS == filt)) %>%
        group_by(.dots = dots) %>%
        summarise(suc = mean(success), num = sum(success), den = n()) %>%
        mutate(overallSuc = sum(num)/sum(den), outProp = suc,
               progProp = sum(num)/sum(den))
      }
    }

    # Final manipulations based on input
    if (input$demoS != 'None') {
      names(temp)[2] <- 'demoCol'
      names(college)[2] <- 'demoCol'

      # If equity comparison is slected divide by overall success
      if (input$compareDem == 'Yes') {
        temp$suc <- temp$suc - temp$overallSuc
        temp$outProp <- round(temp$outProp, 1)
        temp$progProp <- round(temp$progProp, 1)

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
    Note: As of V1.1, WEXP enrollments are now counted within each program. Success
    rates and enrollments may have changed slightly. All WEXP enrollments (198,
    298, 498) are still counted if you select the WEXP subject code.
    <br>
    <br>
    Note: Spring 2020 success rates may be an outlier for a given program because
    of increased EW and incomplete grades. As such, interpret this term with caution.
     </strong>'

  if (input$compareDem == 'Yes') {
    txt <- '<strong> Displaying percentage point gaps (PPGs).
      For course success, a ppg is calculated by taking
      the success rate for a given group and subtracting the overall success
      rate for all students.  A
      value below 0 may indicate disproportionate impact such that a student
      group has a below average success rate.
      MOUSE OVER BARS TO VIEW SPECIFIC VALUES. </strong>'
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

        n1$yAxis(axisLabel='Percentage Point Gap', width = 50)
        n1$chart(forceY = c(-1.0 * max(abs(success()$suc)), 
                            1.0 * max(abs(success()$suc))),
                 tooltipContent = "#!
                 function(key, x, y, e){
                 return '<p> <strong>' + key + '</strong> </p>' +
                 '<p>' + x + ': <strong>' + y + '</strong> </p>' +
                 '<p>' +
                   'This group had a ' + e.point.outProp + '%' +
                   '<br/>' +
                   ' success rate compared to ' +
                   '<br/>' +
                   e.point.progProp + '% for all students ' +
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
