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

#                           Enrollment Dash

################################################################################

  #-----------Reactive UX ------------------------------------------------------
  
  # toggle program select
  observe({
    if(input$collegeE == 'Academic Programs')
    {show(id = 'acadEpop', anim = TRUE)}
    if(input$collegeE != 'Academic Programs')
    {hideElement(id = 'acadEpop', anim = TRUE)}
  })
  
  # toggle enrollment subject help
  observe({
    if(input$collegeE == 'Academic Programs' & is.null(input$acadE))
    {show(id = 'helpE1pop', anim = TRUE)}
    if(input$collegeE == 'Academic Programs' & !is.null(input$acadE)|
       input$collegeE != 'Academic Programs')
    {hideElement(id = 'helpE1pop', anim = TRUE)}
  })
  
  # toggle special programs select
  observe({
    if(input$collegeE == 'Student Support or Cohort Programs')
    {show(id = 'specialEpop', anim = TRUE)}
    if(input$collegeE != 'Student Support or Cohort Programs')
    {hideElement(id = 'specialEpop', anim = TRUE)}
  })
  
  # toggle term help
  observe({
    if(is.null(input$termE))
    {show(id = 'helpE2pop', anim = TRUE)}
    if(!is.null(input$termE))
    {hideElement(id = 'helpE2pop', anim = TRUE)}
  })
  
  # toggle equity question
  observe({
    if(input$demoE != 'None' & (
       input$collegeE == 'Student Support or Cohort Programs' |
       input$collegeE == 'Academic Programs' & !is.null(input$acadE)))
    {show(id = 'demoEpop', anim = TRUE)}
    if(input$demoE == 'None' | input$collegeE == 'Collegewide'|
       input$collegeE == 'Academic Programs' & is.null(input$acadE))
    {hideElement(id = 'demoEpop', anim = TRUE)}
  })

  # Reset comparisons if they are hidden
  observe({
    if (input$demoE == 'None' | input$collegeE == 'Collegewide') {
      reset('compareE')
    }
  })
  
  #-----------------------------------------------------------------------------

  # Create enrollment dataset for plotting
  enrollment <- reactive({

    # Determine filter columns, subject by default.
    prog <- input$collegeE
    print(input$acadE)

    # If work experience is selected, combine all WEXP by using wexp column
    if (is.null(input$acadE)) {
      filtCol = 'subject'

    } else if ('WEXP' %in% input$acadE) {
      filtCol <- 'wexp'

    } else {
      filtCol = 'subject'
    }

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
      group_by(.dots = dots) %>%
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

      # Keep collegewide unless something is selected
      if (input$collegeE == 'Academic Programs' & is.null(input$acadE)) {
        temp <- college

      } else {
        temp <- enroll %>%
          subset(seq_along(term) %in% grep(terms, term) &
                   (filt %in% input$acadE  | input$specialE == filt)) %>%
          group_by(.dots = dots) %>%
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
          mutate(proportion = proportion.x - proportion.y) %>%
          select(-c(duplicated.x, duplicated.y, unduplicated.y,
                    undup.x, undup.y)) %>%
          rename(unduplicated = unduplicated.x, progProp = proportion.x,
                 colProp = proportion.y)
        temp$progProp <- round(temp$progProp, 1)
        temp$colProp <- round(temp$colProp, 1)
      }

    }

    temp
  })

################################################################################
#----------------------------Enrollment Output----------------------------------

  output$defE <- renderUI({
    txt <- '<strong> Displaying duplicated and unduplicated
        headcounts. MOUSE OVER LINE TO VIEW SPECIFIC NUMBERS
        <br>
        <br>
        Note: As of version 1.1, WEXP enrollments are now counted within programs.
        As a result, success rates/enrollments may have changed very slightly since
        your last visit. Additionally, all WEXP enrollments will still be counted
        under the WEXP subject code when selected. </strong>'

    if(input$demoE != 'None') {
      txt <- '<strong> Displaying the proportion of UNDUPLICATED headcount for
        the selected demographic collegwide. MOUSE OVER BARS TO VIEW RAW
        NUMBERS. </strong>'
    }

    if(input$compareE == 'Yes') {
      txt <- '<strong> Percentage point gaps (PPGs) for the selected
        demographic in the selected program(s). A PPG is calculated by
        taking the representation of a demographic group
        in the selected program and subtracting that group&#39;s
        representation collegewide.  A value
        below 0 may indicate an access issue to the selected program
        because a particular group is accessing the program at a lower
        rate than expected. MOUSE OVER BARS TO VIEW SPECIFIC VALUES.'
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

        n1$yAxis(axisLabel = 'Percentage Point Gap', width = 50)
        n1$chart(showControls = F, reduceXTicks = F,
                 color = colors,
                 forceY = c(-1.0 * max(abs(enrollment()$proportion)), 
                            1.0 * max(abs(enrollment()$proportion))),
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
})
