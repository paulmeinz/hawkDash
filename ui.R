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
demo <- c(None = 'None', Age = 'age',
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

  

#---------------------------COURSE SUCCESS TAB----------------------------------
      sidebarLayout(
        sidebarPanel(
          radioButtons('collegeS', 'Would you like to view course success data
                       for:',
                       trends),
          
          hidden(div(id = 'acadSpop',
                     selectInput('acadS', 'Select program(s)', acad,
                                 multiple = TRUE))),
          
          hidden(div(id = 'helpS1pop',
                     p(class = 'help', 'Select a program by clicking in the box above.
                       You can type a subject prefix (e.g., MATH) or pick out of
                       the menu. Picking multiple will combine results across programs.
                       Delete selections with backspace. Collegewide
                       data displays when no program is selected.'))),
          
          hidden(div(id = 'specialSpop',
                     selectInput('specialS', 'Select a program',
                                 special))),

          checkboxGroupInput('termS', 'What terms would you like to see?',
                             term, inline = TRUE, selected = 'Fall'),

          bsPopover('termS',
                    '<strong> What terms would you like to see? </strong>',
                    popTerm, 'right', options = list(container = 'body')),
          
          hidden(div(id = 'helpS2pop',
                     p(class = 'help', 'Select a term by checking the boxes
                       above. Selecting both Fall and Spring will display data 
                       for fall and spring over five years.'))),

          selectInput('demoS', 'Would you like to view trends for
                      a particular demographic group?', demo),
          
          hidden(div(id = 'compareDempop',
                     radioButtons('compareDem','Evaluate equity?',
                                  compare, inline = TRUE))),

          bsPopover('compareDem',
                    '<strong> Evaluate equity? </strong>',
                    popS2, 'right', options = list(container = 'body')),
          
          hidden(div(id = 'compareColpop',
                     radioButtons('compareCol','Compare to collegewide?',
                                  compare, inline = TRUE))),

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


