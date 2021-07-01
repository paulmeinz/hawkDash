library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)
library(shinyjs)
library(shinyBS)
source('utilFuncs.R')

# Load standard data
load('enrollment.rdata')

crcLink <- "https://crclosrios.co1.qualtrics.com/jfe/form/SV_86cJqok0Qj1Lrdr"


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
trends <- c('The entire college' = 'Collegewide',
            'An academic program' = 'Academic Programs',
            'A student support or cohort program' =
              'Student Support or Cohort Programs')

# Academic programs
acad <- unique(enroll$subject)
acad <- acad[order(acad)]

# Special (non academic) programs
special <- c(CalWORKS = 'calwork', CARE = 'care', EOPS = 'eops',
             Diop = 'diop', DSPS = 'dsps', Puente = 'puente', MESA = 'mesa',
             Honors = 'honors')

# Compare options
compare <- c('No','Yes')

# Term selections
term <- c('Fall','Spring')

# Demographic options
demo <- c(None = 'None', Age = 'age', 'Basic Skills' = 'basicskills',
          'Disability Status' = 'disability', Ethnicity = 'ethnicity',
          'Enrollment Status' = 'status', 'First Generation' = 'firstgen',
          'Foster Youth Status' = 'foster',
          'Gender' = 'gender','Online' = 'online', 'Veteran Status' = 'veteran'
          )

# Exemption status
exempt <- c('No','Yes')

# Output Options
sssp <- c(Assessment = 'assess', 'Ed Plan' = 'edPlan',
          Orientation = 'orientation', 'Counseling/Follow-up' = 'counsel')


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
  who attended (at any time) Cosumnes Oaks, Elk Grove High, Elk Grove Charter,
  Florin,
  Franklin, Laguna Creek, Monterey Trail, Pleasant Grove, Sheldon,
  Valley, Calvine, Rio Cazadero, and Transition High.'
popA2 <- subNew(popA2)

popA3 <- "Selecting <strong> Yes </strong> will calculate a percentage
  point gap by subtracting the enrollment rate for a group from the enrollment
  rate for all students. A value below zero may indicate an access issue."
popA3 <- subNew(popA3)

popM1 <- 'Checking multiple boxes will display data for enrolled students who
  have completed all the selected outcomes during or before a given term.
  For example, checking <strong> Assessment </strong> and <strong>
  Ed Plan </strong> will display data on students who completed
  assessment AND an educational plan.'
popM1 <- subNew(popM1)

popM2 <- 'Checking <strong> Yes </strong> will include students who have been
  exempted, new transfer matriculated, or grandfathered.'
popM2 <- subNew(popM2)

popM3 <- 'Selecting <strong> Yes </strong> will calculate a percentage point gap
  by taking the percent of students that completed the selected outcomes
  (Assessment, Ed Plan, and/or Orientation) and subtracting the percentage of 
  all students that completed the same outcomes. A value below zero may indicate 
  disproportionate impact.'
popM3 <- subNew(popM3)

popE2 <- 'Selecting <strong> Yes </strong> will calculate a percentage point
  gap by taking the percent representation of a group in the selected
  program(s) and dsubtracting the representation of that group
  collegewide. A value below 0 may indicate an access issue.'
popE2 <- subNew(popE2)

popS1 <- 'Selecting <strong> Yes </strong> will display a trend graph with
  a line representing the selected program success rates and a line
  representing the collegwide success rates.'
popS1 <- subNew(popS1)

popS2 <- "Selecting <strong> Yes </strong> will calculate a percentage point
  gap. The percentage point gap is calculated by subtracting the success rate
  for all students from the success rate for a given group. 
  A value below 0 may indicate disproportionate impact."
popS2 <- subNew(popS2)


################################################################################

#                                Define UI

################################################################################


shinyUI(fluidPage(
  theme = 'style.css',
  
  useShinyjs(),

  navbarPage(title = 'CRC HawkDash', id = 'navbar',

#---------------------------INTRO PAGE------------------------------------------


    tabPanel('Welcome!',
      fluidRow(id = 'welcome-top',
               column(12,
                      br(),
                      br(),
                      br(),
                      h1(id = 'welcome-header',
                         "Welcome to the CRC HawkDash!"),
                      p(class = 'welcome-text', id = 'specific',
                        "Click the tabs above to view data on",
                        "applications, matriculation, enrollment,",
                        "and course success at Cosumnes River College.")

               )
      ),
      fluidRow(id = 'welcome-mid',
               br(),
               br(),
               br(),
               column(6,
                      p(class = 'welcome-text', id = 'specific',
                        "If you have questions, please contact:",
                        br(),
                        a(href = 'mailto:CRC-Research@crc.losrios.edu',
                          style = 'color: #ffffff',
                          "CRC-Research@crc.losrios.edu"))

               ),
               column(6,
                      p(class = 'welcome-text', id = 'specific',
                        "If you have a research question or want",
                        br(),
                        "additional data ",
                        a(href = crcLink, style = 'color:#ffffff',
                          target= '_blank',
                          "CLICK HERE"))
                      )
      ),
      fluidRow(id = 'copyright',
               column(12,
                      p(id = 'info',
                        'Product of the CRC Office of',
                        'Research and Equity v1.1')
               )
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
          
          hidden(div(id = 'helpA2pop',
                 p(class = 'help', 'Select a term by checking the boxes
                   above. Selecting both Fall and Spring will display data for
                   fall and spring over five years.')
                 )),

          selectInput('demoA',
                      'View trends for a particular demographic group?',
                      demoA),
          
          hidden(div(id = 'compareApop',
                     radioButtons('compareA','Evaluate Equity?',
                                  compare, inline = TRUE))),

          bsPopover('compareA', '<strong> Evaluate equity? </strong>',
                    popA3, 'right',
                    options = list(container = 'body'))
        ),

        # Show plot
        mainPanel(
          chartOutput('histA', lib = 'nvd3'),
          #bsTooltip('histA', toolValues, 'bottom'),
          htmlOutput('defA'),
          plotOutput('plot4', height = '0px')
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

          radioButtons('exempt', 'Include students who were exempt, new
                        transfer matriculated, and/or grandfathered?', exempt,
                       inline = TRUE),

          bsPopover('exempt','<strong> Select Yes or No </strong>',
                    popM2,
                    'right', options = list(container = 'body')),

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

          bsPopover('compareM', '<strong> Evaluate equity? </strong>', popM3,
                    'right', options = list(container = 'body'))
        ),

               # Show a plot of the generated distribution
        mainPanel(
          chartOutput('histM', lib = 'nvd3'),
          #bsTooltip('histM', toolValues, 'bottom'),
          htmlOutput('defM'),
          plotOutput("plot3", height = '0px')
        )
      )
    ),


#---------------------------ENROLLMENT TAB--------------------------------------


    tabPanel(title = 'Enrollment Data',
      sidebarLayout(
        sidebarPanel(
          radioButtons('collegeE',
                       'Would you like to view enrollment data for:',
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
          plotOutput("plot2", height = '0px')
        )
      )
    ),


#---------------------------COURSE SUCCESS TAB----------------------------------


    tabPanel(title = 'Course Success Data',
      sidebarLayout(
        sidebarPanel(
          radioButtons('collegeS', 'Would you like to view course success data
                       for:',
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
          plotOutput("plot1", height = '0px')
        )
      )
    )
  )

))
