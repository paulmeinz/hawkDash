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

load('enrollment.rdata')
programtype <- c('Academic Programs', 'Special Programs')
acad <- unique(enroll$subject)
special <- c('CalWORKS', 'CARE', 'EOPS', 'Diop', 'DSPS')
lookup <- c(calwork = 'CalWORKS',
             care = 'CARE',
             eops = 'EOPS',
             diop = 'Diop',
             dsps = 'DSPS')


# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("The CRC Hawkdash"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
       selectInput('progType', 'Select a program type', programtype)
    ),

  conditionalPanel(
    condition = "input.progType == 'Academic Programs'",
    selectInput('acad', 'Select program(s)', acad)
  ),

  conditionalPanel(
    condition = "input.progType == 'Special Programs'",
    selectInput('special', 'Select Program', special)
  ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
