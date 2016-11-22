###############################################################################
# Filename:         wk2assignment.R
# Version:          0.1
# Author:           Robert Muwanga
# License:          Apache License 2.0
# Purpose:          Code for Assignment 1
# Dependencies:     None
###############################################################################
# This outlines all the functions that are used for the PA1_templated.Rmd 
# as part of Assignment 1 in John Hopkins' Reproducible Research course.
###############################################################################

# Required packages
packages <- c('lubridate', 'dplyr', 'ggplot2')
dataURL <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
dataFolder <- file.path(getwd(), 'data/')
filename <- 'activity.zip'
extractedFile <- 'activity.csv'

# Download and load packages
checkPackages <- function() {
  missingPackages <- packages[(!(packages %in% installed.packages()))]
  if(length(missingPackages) > 0)
    install.packages(missingPackages)
  
  if(!all(unlist(lapply(packages, require, character.only = TRUE))))
    stop('Cannot install packages. Quitting...')
}

# Download and load dataset
loadData <- function() {
  fullFileName <- file.path(dataFolder, filename)
  
  dir.create(dataFolder, showWarnings = FALSE)
  download.file(dataURL, fullFileName)
  unzip(zipfile = fullFileName, exdir = dataFolder, files = extractedFile)
  
  # Return dataset
  tbl_df(read.csv(file.path(dataFolder, extractedFile)))
}

cleanData <- function(data_file) {
  data_file$date <- ymd(data_file$date)
  
}

# Executing all functions for 'testing'
main <- function() {
  # Load dependencies
  checkPackages()
  
  # Loading and preprocessing the data
  # dataFile <- loadData()
  # cleanData(dataFile)
  
  # What is mean  number of steps taken per day?
  totalsteps <- dataFile %>% group_by(date) %>% 
    summarise(total = sum(steps, na.rm = TRUE))
  
  totalsteps %>% ggplot(aes(total)) + geom_histogram(binwidth = 350) + 
    ggtitle('Histogram of total number of steps')
  
  dataFile %>% group_by(date) %>% 
    summarise(total = sum(steps, na.rm = TRUE), 
              mean = mean(steps, na.rm = TRUE), 
              median = median(steps, na.rm = TRUE))
  
  # What is the average daily activity pattern?
  
  
  
}