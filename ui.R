library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)

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
          
          radioButtons('egusd', 'Look at EGUSD applicants only?', egusd,
                       inline = TRUE),
          
          checkboxGroupInput('termA','What terms would you like to see?', term,
                             selected = 'Fall', inline = TRUE),
          
          selectInput('demoA', 'Would you like to see data for a particular
                      demographic group?', demoA),
          
          conditionalPanel(condition = "input.demoA != 'None' &
                           input.outcome == '% of Applicants that Enroll'",       
                           selectInput('compareA','Evaluate Equity?', compare))
          
        ),
        
        # Show plot
        mainPanel(
          chartOutput('histA', lib = 'nvd3'),
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
          
          selectInput('exempt', 'Include students who were exempted, 
                      grandfathered, or new transfer matriculated?', exempt),
                 
          checkboxGroupInput('termM', 'What terms would you like to see?', 
                             term, inline = TRUE, selected = 'Fall'),
                 
          selectInput('demoM', 'Would you like to view trends for
                      a particular demographic group?', demo),
                 
          conditionalPanel(condition = "input.demoM != 'None'",       
                           selectInput('compareM','Evaluate Equity?', compare))
        ),
               
               # Show a plot of the generated distribution
        mainPanel(
          chartOutput('histM', lib = 'nvd3'),
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
                                  
          conditionalPanel(condition = "input.collegeE == 'Special Programs'",
                           selectInput('specialE', 'Select a program', 
                                       special)),
                 
          checkboxGroupInput('termE', 'What terms would you like to see?', 
                             term, inline = TRUE, selected = 'Fall'),
                 
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

          conditionalPanel(condition = "input.collegeS == 'Special Programs'",
                           selectInput('specialS', 'Select a program', 
                                       special)),

          checkboxGroupInput('termS', 'What terms would you like to see?', 
                             term, inline = TRUE, selected = 'Fall'),

          selectInput('demoS', 'Would you like to view trends for
                      a particular demographic group?', demo),
          
          conditionalPanel(condition = "input.demoS != 'None'",
                           selectInput('compareDem','Would you like to evaluate
                                       equity?', compare)),
          
          conditionalPanel(condition = "
                           input.collegeS == 'Academic Programs' &&
                           input.acadS != null ||
                           input.collegeS == 'Special Programs'
                           ",
                           selectInput('compareCol','Compare to collegewide?', 
                                       compare))
          
        ),

    # Show a plot of the generated distribution
        mainPanel(
          chartOutput('histS', lib = 'nvd3'),
          plotOutput("plot1"))
        
      )
    )
  )
))
