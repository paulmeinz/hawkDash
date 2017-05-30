library(shiny)
library(dplyr)
library(rCharts)

# Load standard data
load('enrollment.rdata')

################################################################################

#                         DEFINE BUTTON OPTIONS

################################################################################

# COURSE SUCCESS DASHBOARD OPTIONS
programType <- c('Choose One', 'Academic Programs', 'Special Programs')

# Type of trend evaluation
trends <- c('Collegewide','Program')

# Academic programs
acad <- unique(enroll$subject)
acad <- acad[order(acad)]

# Special (non academic) programs
special <- c(CalWORKS = 'calwork', CARE = 'care', EOPS = 'eops',
             Diop = 'diop', DSPS = 'dsps', Puente = 'puente')

# Compare options
compare <- c('None','Compare to Collegewide',
             'Evaluate Equity')

# Term selections
term <- unique(enroll$term)
term <- term[order(term)]

# Demographic options
demo <- c(None = 'None', Age = 'age', 'Basic Skills' = 'basicskills',
          'Disability Status' = 'disability', Ethnicity = 'ethnicity',
          'Enrollment Status' = 'status', 'First Generation' = 'firstgen',
          'Gender' = 'gender','Online' = 'online', 'Veteran Status' = 'veteran',
          'Foster Youth Status' = 'foster')


################################################################################

#                                Define UI

################################################################################

shinyUI(fluidPage(

  # Application title
  titlePanel("The CRC Hawkdash"),

  # Inputs
  sidebarLayout(
    sidebarPanel(
      selectInput('college', 'Would you like to see Collegewide data
                  or data in a program?', trends),

      conditionalPanel(condition = "input.college == 'Program'",
        selectInput('progType', 'Select a program type', programType),

      conditionalPanel(
        condition = "input.progType == 'Academic Programs'",
        selectInput('acad', 'Select program(s) by clicking in the box below.'
                    , acad, multiple = TRUE)),

      conditionalPanel(
        condition = "input.progType == 'Special Programs'",
        selectInput('special', 'Select a program', special))),

      selectInput('term', 'What terms would you like to see? Click in
                  the box below.', term, multiple = TRUE),

      selectInput('demo', 'Would you like to view trends for
                  a particular demographic group?', demo),
      
      selectInput('compare','Perform a Comparison', compare)
    ),

    # Show a plot of the generated distribution
    mainPanel(
       chartOutput('hist', lib = 'nvd3'),
       plotOutput("plot1")
    )
  )
))
