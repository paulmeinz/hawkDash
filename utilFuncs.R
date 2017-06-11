################################################################################
# This file contains some helper functions to reduce code redundency
# Much of the code needs to be replaced with helper functions and I will
# do this when I get the time.

subNew <- function(txt) {
  txt <- gsub(pattern = '\n', replacement = ' ', txt)
  txt
}