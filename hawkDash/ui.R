#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(rCharts)

# Load standard data
load('enrollment.rdata')

# Create selection options
programType <- c('Choose One', 'Academic Programs', 'Special Programs')

acad <- unique(enroll$subject)
acad <- acad[order(acad)]

compare <- c('None','Compare to Collegewide',
             'Evaluate Equity')

term <- unique(enroll$term)

trends <- c('Collegewide','Program')

special <- c(CalWORKS = 'calwork', CARE = 'care', EOPS = 'eops',
             Diop = 'diop', DSPS = 'dsps', Puente = 'puente')

demo <- c(None = 'None', Age = 'age', 'Basic Skills' = 'basicskills',
          'Disability Status' = 'disability', Ethnicity = 'ethnicity',
          'Enrollment Status' = 'status', 'First Generation' = 'firstgen',
          'Online' = 'online', 'Veteran Status' = 'veteran',
          'Foster Youth Status' = 'foster')


# Define UI
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
