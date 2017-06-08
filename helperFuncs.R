################################################################################
#
# This file contains helper functions for HakDash.
#
# There are currently many code redundencies that could be turned into helper
# functions. I will work on this as I get the time.
#

createHelp <- function(output = '', acadProg = '', term = '') {
  
  if (output != '' & is.null(output)) {
    out <- "<strong> Select the outcomes you want to see </br>
            (Assessment, Ed Plan, or Orientation). Checking </br>
            multiple will display data for students who have earned </br>
            all those outcomes (e.g., an Ed Plan AND Assessment) </strong>"
  }

  if (acadProg != '' & is.null(acadProg)) {
    prog <- "<strong> Select a program by clicking in the box labelled </br>
             'Select Program(s)'. You can either type out a program prefix </br>
             '(e.g., PSYC) or select from the dropdown menu. Press </br>
             'backspace to unselect a program </strong>"
  }
}