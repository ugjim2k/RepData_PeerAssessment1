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
  #  download.file(dataURL, fullFileName)
  unzip(zipfile = fullFileName, exdir = dataFolder, files = extractedFile)
  
  # Return dataset
  tbl_df(read.csv(file.path(dataFolder, extractedFile)))
}

cleanData <- function(data_file, valueOfNASteps) {
  # Clean up the dates usling lubridate and replace NA in steps field
  # with value in valueOfNASteps argument
  
  data_file$date <- ymd(data_file$date)
  if(!missing(valueOfNASteps))
    data_file$steps <- replace(
      data_file$steps, which(is.na(data_file$steps)), 
      valueOfNASteps)
  
  data_file
}

# Executing all functions for 'testing'
main <- function() {
  # Load dependencies
  checkPackages()
  
  # Loading the original data set into the dataSet variable
  dataSet <- loadData()
  
  # Cleaning steps in original data set by replacing NA with 0
  dataFile <- cleanData(dataSet, 0)
  
  # What is mean  number of steps taken per day?
  # -------------------------------------------------
  totalsteps <- dataFile %>% group_by(date) %>% 
    summarise(total = sum(steps, na.rm = TRUE))
  
  totalsteps %>% ggplot(aes(total)) + geom_histogram(binwidth = 350) + 
    ggtitle('Histogram of total number of steps')
  
  summaryData <- dataFile %>% group_by(date) %>% 
    summarise(total = sum(steps), 
              mean = mean(steps), 
              median = median(steps))
  
  # What is the average daily activity pattern?
  # ------------------------------------------------
  ggplot(summaryData, aes(date, mean)) + 
    geom_line() + 
    labs(title = 'Time Series Plot', x = 'Date', y = 'Average Step Count')
  
  summaryData[which.max(summaryData$mean), ]$date
  
  ## Imputing missing values
  ## -----------------------------------
  NATotals <- sum(is.na(dataFile)) # Total number of NAs
  
  #Let's merge the summaryData object with the dataFile object to create one.
  mrg <- merge(cleanData(dataSet), summaryData, by = 'date')
  NADataSet <- cleanData(dataSet)
  indices <- which(is.na(NADataSet$steps))
  NADataSet$steps <- replace(NADataSet$steps, indices, mrg$mean[indices])
  
  NADataSet %>% group_by(date) %>% 
    summarise(
      totalsteps = sum(steps),
      mean = mean(steps), 
      median = median(steps))
  
  ggplot(NADataSet, aes(steps)) + geom_histogram()
  
  ## Are there differences in activity patterns between weekdays and weekends?
  #---------------------------------------------------------------------------
  weekend <- c('Sat', 'Sun')
  title <- c('Weekend', 'Weekday')
  NADataSet %>% 
    merge(summaryData, by = 'date') %>% 
    mutate(weekday = title[weekdays(date, TRUE) %in% weekend + 1]) %>% 
    group_by(weekday, interval) %>% 
    summarise(AverageSteps = mean(steps)) %>% 
    ggplot(aes(interval, AverageSteps)) + 
      geom_line() + 
      facet_grid(weekday ~ .) +
      ggtitle('Weekday and Weekend Comparison') +
      xlab('Interval') + 
      ylab('Number of Average Steps')
}