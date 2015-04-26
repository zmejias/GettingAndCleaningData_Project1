# GettingAndCleaningData_Project1
Repo for the Getting and Cleaning Data Project 1 Submission

run_analysis.R needs some preliminary set up before running the code:

Please load the Packages with these two lines:
```
packages <- c("data.table", "reshape2")

sapply(packages, require, character.only=TRUE, quietly=TRUE)
```
After the packages are load, and the run_analysis.R is saved on the working directory, then:
```
source("run_analysis.R")
```

The Codes does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



