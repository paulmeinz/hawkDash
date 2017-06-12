library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)
library(shinyjs)
library(shinyBS)
source('utilFuncs.R')

# Load standard data
load('enrollment.rdata')

################################################################################

#                         DEFINE BUTTON OPTIONS

################################################################################


#--------------------------ACCESS DASHBOARD OPTIONS-----------------------------

# Outcome variable
outcome <- c('Applicant Counts', '% of Applicants that Enroll')

# Population
egusd <- c('No','Yes')

# Demographic selections for disaggregation
demoA <- c(None = 'None', 'Enrollment Status' = 'status', 
           Ethnicity = 'ethnicity', 'First Generation' = 'firstgen', 
           'Foster youth' = 'foster', Gender = 'gender', 
           'Interested in CalWORKS' = 'calworks', 'Interested in DSPS' = 'dsps',
           'Interested in EOPS' = 'eops', 'Veteran Status' = 'veteran')


#--------------ENROLLMENT/SUCCESS/MATRICULATION DASHBOARD OPTIONS---------------

programType <- c('Choose One', 'Academic Programs', 'Special Programs')

# Type of trend evaluation
trends <- c('Collegewide', 'Academic Programs', 'Special Programs')

# Academic programs
acad <- unique(enroll$subject)
acad <- acad[order(acad)]

# Special (non academic) programs
special <- c(CalWORKS = 'calwork', CARE = 'care', EOPS = 'eops',
             Diop = 'diop', DSPS = 'dsps', Puente = 'puente')

# Compare options
compare <- c('No','Yes')

# Term selections
term <- c('Fall','Spring')

# Demographic options
demo <- c(None = 'None', Age = 'age', 'Basic Skills' = 'basicskills',
          'Disability Status' = 'disability', Ethnicity = 'ethnicity',
          'Enrollment Status' = 'status', 'First Generation' = 'firstgen',
          'Gender' = 'gender','Online' = 'online', 'Veteran Status' = 'veteran',
          'Foster Youth Status' = 'foster')

# Exemption status
exempt <- c('No','Yes')

# Output Options
sssp <- c(Assessment = 'assess', 'Ed Plan' = 'edPlan', 
          Orientation = 'orientation')

#---------------------------POPOVER MESSAGES------------------------------------

toolValues <- 'Mouse over points/bars to view specific values and raw numbers'
toolValues <- subNew(toolValues)

popTerm <- 'Select the terms you would like to see. Picking both <strong> Fall
        </strong> and <strong> Spring </strong> will show fall and spring data 
        for five years.'
popTerm <- subNew(popTerm)

popA1 <- 'Selecting <strong> Applicant Counts </strong> will provide a breakdown 
         of the raw number of applicants, and selecting <strong> % of Applicants 
         that Enroll </strong> will provide data on the applicants that enroll 
         after applying.'
popA1 <- subNew(popA1)

popA2 <- 'Selecting <strong> Yes </strong> will display data only for applicants
         from....[fill in]'
popA2 <- subNew(popA2)

popA3 <- "Selecting <strong> Yes </strong> will calculate a proportionality
         index by taking the percent representation of a group in student
         enrollees and dividing by the representation of that group in all 
         applicants. The resulting ratio is multiplied by 100. A ratio below 
         100 may indicate disproportionate impact."
popA3 <- subNew(popA3)

popM1 <- 'Checking multiple boxes will display data for enrolled students who 
         have completed all the selected outcomes. For example, checking 
         <strong> Assessment </strong> and <strong> Ed Plan </strong> will 
         display data on students who completed assessment AND an educational 
         plan.'
popM1 <- subNew(popM1)

popM2 <- 'Selecting <strong> Yes </strong> will calculate a proportionality
         index by taking the percent representation of a group among students
         that completed the selected outcomes (Assessment, Ed Plan, and/or 
         Orientation) and dividing by the representation of that group amongst
         all enrolled students. The resulting ratio is multiplied by 100.
         A ratio below 1 may indicate disproportionate impact.'
popM2 <- subNew(popM2)


################################################################################

#                                Define UI

################################################################################


shinyUI(fluidPage(
  useShinyjs(),

  navbarPage(title = 'The CRC Hawkdash',
             
             
#---------------------------ACCESS TAB------------------------------------------


    tabPanel(title = 'Applications (Access)',
      sidebarLayout(
        sidebarPanel(
          radioButtons('outcome', 'What applicant data would you like to see?',
                        outcome),
          
          bsPopover('outcome', 
                    "<strong> What data would you like to see? </strong>",
                     popA1, 'right', options = list(container = 'body')),
          
          radioButtons('egusd', 'Look at EGUSD applicants only?', egusd,
                       inline = TRUE),
          
          bsPopover('egusd', 
                    '<strong> Look at EGUSD applicants only? </strong>',
                    popA2, 'right', options = list(container = 'body')),
          
          checkboxGroupInput('termA','What terms would you like to see?', term,
                             selected = 'Fall', inline = TRUE),
          
          bsPopover('termA', 
                    '<strong> What terms would you like to see? </strong>',
                    popTerm, 'right', options = list(container = 'body')),
          
          conditionalPanel(condition = "input.termA == ''",
                           htmlOutput('helpA2')),
          
          selectInput('demoA', 'Look at a particular demographic group?', 
                      demoA),
          
          conditionalPanel(condition = "input.demoA != 'None' &
                           input.outcome == '% of Applicants that Enroll'",       
                           radioButtons('compareA','Evaluate Equity?', 
                                         compare, inline = TRUE)),
          
          bsPopover('compareA', '<strong> Evaluate Equity? </strong>',
                    popA3, 'right', 
                    options = list(container = 'body'))
        ),
        
        # Show plot
        mainPanel(
          chartOutput('histA', lib = 'nvd3'),
          bsTooltip('histA', toolValues, 'bottom'),
          plotOutput("plot4")
        )
       
      )
    ),


#---------------------------MATRICULATION TAB-----------------------------------
    

    tabPanel(title = 'Matriculation (SSSP)',
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput('sssp', 'View data on students
                             that completed (check all that apply):', sssp,
                             selected = c('assess','edPlan', 'orientation')),
          
          bsPopover('sssp','<strong> Select one or many </strong>', popM1,
                    'right', options = list(container = 'body')),
          
          selectInput('exempt', 'Include students who were exempted, 
                      grandfathered, or new transfer matriculated?', exempt),
                 
          checkboxGroupInput('termM', 'What terms would you like to see?', 
                             term, inline = TRUE, selected = 'Fall'),
          
          bsPopover('termM', 
                    '<strong> What terms would you like to see? </strong>',
                    popTerm, 'right', options = list(container = 'body')),
          
          conditionalPanel(condition = "input.termM == ''",
                           htmlOutput('helpM2')),
                 
          selectInput('demoM', 'Look at a particular demographic group?', demo),
                 
          conditionalPanel(condition = "input.demoM != 'None'",       
                           radioButtons('compareM','Evaluate Equity?', compare, 
                                        inline = TRUE)),
          
          bsPopover('compareM', '<strong> Evaluate Equity? </strong>', popM2,
                    'right', options = list(container = 'body'))
        ),
               
               # Show a plot of the generated distribution
        mainPanel(
          chartOutput('histM', lib = 'nvd3'),
          bsTooltip('histM', toolValues, 'bottom'),
          plotOutput("plot3")
        )
      )           
    ),


#---------------------------ENROLLMENT TAB--------------------------------------
    

    tabPanel(title = 'Enrollment',
      sidebarLayout(
        sidebarPanel(
          selectInput('collegeE', 'Would you like to see Collegewide data
                      or data in a program?', trends),
                                  
          conditionalPanel(condition = "input.collegeE == 'Academic Programs'",
                           selectInput('acadE', 'Select program(s) by clicking 
                                       in the box below.',acad, 
                                       multiple = TRUE)),
          
          conditionalPanel(condition = "
                           input.collegeE == 'Academic Programs' &&
                           input.acadE == null",
                           htmlOutput('helpE1')),
                                  
          conditionalPanel(condition = "input.collegeE == 'Special Programs'",
                           selectInput('specialE', 'Select a program', 
                                       special)),
                 
          checkboxGroupInput('termE', 'What terms would you like to see?', 
                             term, inline = TRUE, selected = 'Fall'),
          
          conditionalPanel(condition = "input.termE == ''",
                           htmlOutput('helpE2')),
                 
          selectInput('demoE', 'Would you like to view trends for
                      a particular demographic group?', demo),
          
          conditionalPanel(condition = "input.demoE != 'None'
                           && input.collegeE != 'Collegewide'",       
                           selectInput('compareE','Evaluate Equity?', compare))
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

          conditionalPanel(condition = "input.collegeS == 'Academic Programs'",
                           selectInput('acadS', 
                                       'Select program(s) by clicking in the box 
                                       below.',
                                       acad, multiple = TRUE)),
          
          conditionalPanel(condition = "
                           input.collegeS == 'Academic Programs' &&
                           input.acadS == null",
                           htmlOutput('helpS1')),

          conditionalPanel(condition = "input.collegeS == 'Special Programs'",
                           selectInput('specialS', 'Select a program', 
                                       special)),

          checkboxGroupInput('termS', 'What terms would you like to see?', 
                             term, inline = TRUE, selected = 'Fall'),
          
          conditionalPanel(condition = "input.termS == ''",
                           htmlOutput('helpS2')),
          
          selectInput('demoS', 'Would you like to view trends for
                      a particular demographic group?', demo),
          
          conditionalPanel(condition = "input.demoS != 'None'",
                           selectInput('compareDem','Would you like to evaluate
                                       equity?', compare)),
          
          conditionalPanel(condition = "
                           (input.collegeS == 'Academic Programs' &&
                           input.acadS != null ||
                           input.collegeS == 'Special Programs') &&
                           input.demoS == 'None'
                           ",
                           selectInput('compareCol','Compare to collegewide?', 
                                       compare))
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
