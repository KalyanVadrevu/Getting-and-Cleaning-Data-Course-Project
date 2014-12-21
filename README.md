Getting-and-Cleaning-Data-Course-Project
========================================

This repo has the following files:

## Codebook.md
This file details the information about the variables, data, and summary choices used to get the tidy data.

## Run_analysis.R 
This file contains the script that does the following:
1. Assuming that the data set is in the working directory, reads the measurement files and merges the training and test datasets
2. Further, it extracts the relevant data set (only the measurements on the mean and standard deviation for each measurement), adds detail to increase comprehension.
3. Provides a data file with a tidy data set with the average of each variable for each activity and each subject.
* Note: The run_analysis script uses data.table package functionality to perform some summary operations on the intermediate datasets.


