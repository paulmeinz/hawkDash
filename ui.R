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


# Type of trend evaluation
trends <- c('Collegewide', 'Academic Programs', 
            'Student Support or Cohort Programs')

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
  </strong> and <strong> Spring </strong> will show fall and spring data.'
popTerm <- subNew(popTerm)

popA1 <- 'Selecting <strong> Applicant Counts </strong> will provide a breakdown 
  of the raw number of applicants, and selecting <strong> % of Applicants 
  that Enroll </strong> will provide data on the applicants that enroll 
  after applying.'
popA1 <- subNew(popA1)

popA2 <- 'Selecting <strong> Yes </strong> will display data only for applicants
  from Cosumnes Oaks, Elk Grove High, Elk Grove Charter, Florin, 
  Franklin, Laguna Creek, Monterey Trail, Pleasant Grove, Sheldon,
  Valley, Calvine, Rio Cazadero, and Transition High.'
popA2 <- subNew(popA2)

popA3 <- "Selecting <strong> Yes </strong> will calculate a proportionality
  index by taking the percent representation of a group in student
  enrollees and dividing by the representation of that group in all 
  applicants. The resulting ratio is multiplied by 100. A ratio below 
  100 may indicate an access issue."
popA3 <- subNew(popA3)

popM1 <- 'Checking multiple boxes will display data for enrolled students who 
  have completed all the selected outcomes during or before a given term. 
  For example, checking <strong> Assessment </strong> and <strong> 
  Ed Plan </strong> will display data on students who completed 
  assessment AND an educational plan.'
popM1 <- subNew(popM1)

popM2 <- 'Selecting <strong> Yes </strong> will calculate a proportionality
  index by taking the percent representation of a group among students
  that completed the selected outcomes (Assessment, Ed Plan, and/or 
  Orientation) and dividing by the representation of that group amongst
  all enrolled students. The resulting ratio is multiplied by 100.
  A ratio below 100 may indicate disproportionate impact.'
popM2 <- subNew(popM2)

popE2 <- 'Selecting <strong> Yes </strong> will calculate a proportionality
  index by taking the percent representation of a group in the selected
  program(s) and dividing by the representation of that group 
  collegewide. The resulting ratio is multiplied by 100. A ratio below
  100 may indicate an access issue.'
popE2 <- subNew(popE2)

popS1 <- 'Selecting <strong> Yes </strong> will display a trend graph with
  a line representing the selected program success rates and a line
  representing the collegwide success rates.'
popS1 <- subNew(popS1)

popS2 <- "Selecting <strong> Yes </strong> will calculate a proportionality
  index. This index is calculated by taking the percent representation 
  among successful enrollments for a given group and dividing by the
  representation of that group in collegwide enrollments. The resulting 
  ratio is multiplied by 100. A value below 100 may indicate 
  disproportionate impact."
popS2 <- subNew(popS2)


################################################################################

#                                Define UI

################################################################################


shinyUI(fluidPage(
  theme = 'style.css',
  useShinyjs(),
  
  navbarPage(title = 'CRC HawkDash',
             
#---------------------------INTRO PAGE------------------------------------------             

    
    tabPanel('Welcome!',
      fluidRow(id = 'welcome',
               column(6,
                      p(class = 'welcome-text',
                        "Welcome to the CRC HawkDash!",
                      br(),
                      br(),
                      "Click the tabs above to view data on",
                      br(),
                      "applications, matriculation, enrollment,",
                      br(),
                      "and course success at Cosumnes River College.")
                      
               ),
               column(6)
      )
    ),             
#---------------------------ACCESS TAB------------------------------------------


    tabPanel(title = 'Application Data (Access)',
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
          
          selectInput('demoA', 
                      'View trends for a particular demographic group?', 
                      demoA),
          
          conditionalPanel(condition = "input.demoA != 'None' &
                           input.outcome == '% of Applicants that Enroll'",       
                           radioButtons('compareA','Evaluate Equity?', 
                                         compare, inline = TRUE)),
          
          bsPopover('compareA', '<strong> Evaluate equity? </strong>',
                    popA3, 'right', 
                    options = list(container = 'body'))
        ),
        
        # Show plot
        mainPanel(
          chartOutput('histA', lib = 'nvd3'),
          #bsTooltip('histA', toolValues, 'bottom'),
          htmlOutput('defA'),
          plotOutput('plot4')
        )
      )
    ),


#---------------------------MATRICULATION TAB-----------------------------------
    

    tabPanel(title = 'Matriculation Data (SSSP)',
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
                 
          selectInput('demoM', 'View trends for a particular demographic group?'
                      , demo),
                 
          conditionalPanel(condition = "input.demoM != 'None'",       
                           radioButtons('compareM','Evaluate Equity?', compare, 
                                        inline = TRUE)),
          
          bsPopover('compareM', '<strong> Evaluate equity? </strong>', popM2,
                    'right', options = list(container = 'body'))
        ),
               
               # Show a plot of the generated distribution
        mainPanel(
          chartOutput('histM', lib = 'nvd3'),
          #bsTooltip('histM', toolValues, 'bottom'),
          htmlOutput('defM'),
          plotOutput("plot3")
        )
      )           
    ),


#---------------------------ENROLLMENT TAB--------------------------------------
    

    tabPanel(title = 'Enrollment Data',
      sidebarLayout(
        sidebarPanel(
          selectInput('collegeE', 'View collegewide data or data in a program?', 
                      trends),
                                  
          conditionalPanel(condition = "input.collegeE == 'Academic Programs'",
                           selectInput('acadE', 'Select program(s)',acad, 
                                       multiple = TRUE)),
          
          conditionalPanel(condition = "
                           input.collegeE == 'Academic Programs' &&
                           input.acadE == null",
                           htmlOutput('helpE1')),
                                  
          conditionalPanel(condition = 
                           "input.collegeE == 
                           'Student Support or Cohort Programs'",
                           selectInput('specialE', 'Select a program', 
                                       special)),
                 
          checkboxGroupInput('termE', 'What terms would you like to see?', 
                             term, inline = TRUE, selected = 'Fall'),
          
          bsPopover('termE', 
                    '<strong> What terms would you like to see? </strong>',
                    popTerm, 'right', options = list(container = 'body')),
          
          conditionalPanel(condition = "input.termE == ''",
                           htmlOutput('helpE2')),
                 
          selectInput('demoE', 'View trends for a particular demographic group?'
                      , demo),
          
          conditionalPanel(condition = "input.demoE != 'None'
                           && input.collegeE != 'Collegewide'",       
                           radioButtons('compareE','Evaluate equity?', 
                                        compare, inline = TRUE)),
          
          bsPopover('compareE', '<strong> Evaluate Equity? </strong>', popE2,
                    'right', options = list(container = 'body'))
        ),
               
               # Show a plot of the generated distribution
        mainPanel(
          chartOutput('histE', lib = 'nvd3'),
          #bsTooltip('histE', toolValues, 'bottom'),
          htmlOutput('defE'),
          plotOutput("plot2")
        )
      )           
    ),


#---------------------------COURSE SUCCESS TAB----------------------------------


    tabPanel(title = 'Course Success Data', 
      sidebarLayout(
        sidebarPanel(
          selectInput('collegeS', 'View collegewide data or data in a program?', 
                      trends),

          conditionalPanel(condition = "input.collegeS == 'Academic Programs'",
                           selectInput('acadS', 'Select program(s)', acad, 
                                       multiple = TRUE)),
          
          conditionalPanel(condition = "
                           input.collegeS == 'Academic Programs' &&
                           input.acadS == null",
                           htmlOutput('helpS1')),

          conditionalPanel(condition = 
                           "input.collegeS == 
                           'Student Support or Cohort Programs'",
                           selectInput('specialS', 'Select a program', 
                                       special)),

          checkboxGroupInput('termS', 'What terms would you like to see?', 
                             term, inline = TRUE, selected = 'Fall'),
          
          bsPopover('termS', 
                    '<strong> What terms would you like to see? </strong>',
                    popTerm, 'right', options = list(container = 'body')),
          
          conditionalPanel(condition = "input.termS == ''",
                           htmlOutput('helpS2')),
          
          selectInput('demoS', 'Would you like to view trends for
                      a particular demographic group?', demo),
          
          conditionalPanel(condition = "input.demoS != 'None'",
                           radioButtons('compareDem','Evaluate equity?', 
                                       compare, inline = TRUE)),
          
          bsPopover('compareDem', 
                    '<strong> Evaluate equity? </strong>',
                    popS2, 'right', options = list(container = 'body')),
          
          conditionalPanel(condition = "
                           (input.collegeS == 'Academic Programs' &&
                           input.acadS != null ||
                           input.collegeS == 
                           'Student Support or Cohort Programs') &&
                           input.demoS == 'None'
                           ",
                           radioButtons('compareCol','Compare to collegewide?', 
                                       compare, inline = TRUE)),
          bsPopover('compareCol', 
                    '<strong> Compare to collegwide? </strong>',
                    popS1, 'right', options = list(container = 'body'))
        ),

    # Show a plot of the generated distribution
        mainPanel(
          chartOutput('histS', lib = 'nvd3'),
          #bsTooltip('histS', toolValues, 'bottom'),
          htmlOutput('defS'),
          plotOutput("plot1")
        )
      )
    )
  )

))
