library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)

# Load standard data
load('enrollment.rdata')

################################################################################

#                         DEFINE BUTTON OPTIONS

################################################################################


#------------------ENROLLMENT/COURSE SUCCESS DASHBOARD OPTIONS------------------
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
compareE <- c('No','Yes')

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

  navbarPage(title = 'The CRC Hawkdash',
#---------------------------ACCESS TAB------------------------------------------
    tabPanel(title = 'Applications (Access)'),

#---------------------------MATRICULATION TAB-----------------------------------
    tabPanel(title = 'Matriculation (SSSP)'),

#---------------------------ENROLLMENT TAB--------------------------------------
    tabPanel(title = 'Enrollment',
      sidebarLayout(
        sidebarPanel(
          selectInput('collegeE', 'Would you like to see Collegewide data
                      or data in a program?', trends),
                 
          conditionalPanel(condition = "input.collegeE == 'Program'",
            selectInput('progTypeE', 'Select a program type', programType),
                                  
          conditionalPanel(condition = "input.progTypeE == 'Academic Programs'",
            selectInput('acadE', 
                        'Select program(s) by clicking in the box below.',
                        acad, multiple = TRUE)),
                                  
          conditionalPanel(condition = "input.progTypeE == 'Special Programs'",
            selectInput('specialE', 'Select a program', special))),
                 
          selectInput('termE', 'What terms would you like to see? Click in
                      the box below.', term, multiple = TRUE),
                 
          selectInput('demoE', 'Would you like to view trends for
                      a particular demographic group?', demo),
          
          conditionalPanel(condition = "input.demoE != 'None'
                           & input.collegeE == 'Program'",       
            selectInput('compareE','Evaluate Equity?', compareE))
        ),
               
               # Show a plot of the generated distribution
        mainPanel(
          chartOutput('histE', lib = 'nvd3'),
          plotOutput("plot2")
        )
      )           
    ),

#---------------------------COURSE SUCCESS TAB----------------------------------
    tabPanel(title = 'Course Success', 
      sidebarLayout(
        sidebarPanel(
          selectInput('collegeS', 'Would you like to see Collegewide data
                  or data in a program?', trends),

          conditionalPanel(condition = "input.collegeS == 'Program'",
            selectInput('progTypeS', 'Select a program type', programType),

          conditionalPanel(condition = "input.progTypeS == 'Academic Programs'",
            selectInput('acadS', 
                        'Select program(s) by clicking in the box below.',
                         acad, multiple = TRUE)),

          conditionalPanel(condition = "input.progTypeS == 'Special Programs'",
            selectInput('specialS', 'Select a program', special))),

          selectInput('termS', 'What terms would you like to see? Click in
                      the box below.', term, multiple = TRUE),

          selectInput('demoS', 'Would you like to view trends for
                      a particular demographic group?', demo),
      
          selectInput('compareS','Perform a Comparison', compare)
        ),

    # Show a plot of the generated distribution
        mainPanel(
          chartOutput('histS', lib = 'nvd3'),
          plotOutput("plot1")
        )
      )
    )
  )
))
