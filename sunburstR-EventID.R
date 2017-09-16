# SunburstR example for DFIR Redefined: Deeper Functionality for Investigators with R
# Built from  https://gist.github.com/kerryrodden example
# read in eventID-sequences.csv data from the path where you stored it

library(sunburstR)

sequence_data <- read.csv("C:/coding/R/data/sequences/eventID-sequences.csv",
  header=F, stringsAsFactors = FALSE)

sunburst(sequence_data)