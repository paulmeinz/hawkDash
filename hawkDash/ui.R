#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

enroll <- load('enrollment.rdata')
programtype <- c('Academic Program', 'Student Services Program')

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("The CRC Hawkdash"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
       selectInput('progType', 'Select a program type', programtype)
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
