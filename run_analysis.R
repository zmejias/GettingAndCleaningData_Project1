#Setting directory where the file should be
path <- getwd()
fPath <- file.path(path, "UCI HAR Dataset")

#Reading the  Subject & Activity ("Label") Files
dataTable_SubjectTest <- fread(file.path(fPath, "test" , "subject_test.txt" ))
dataTable_SubjectTrain <- fread(file.path(fPath, "train", "subject_train.txt"))
dataTable_ActivityTest <- fread(file.path(fPath, "test" , "Y_test.txt" ))
dataTable_ActivityTrain <- fread(file.path(fPath, "train", "Y_train.txt"))


#function to Read the data files, Convert data frame to data table & Return it
ConvertToDataTable <- function (f) {
  df <- read.table(f)
  dataTable <- data.table(df)
}

#applying ConvertToDataTable function to the training and test sets
dataTable_Train <- ConvertToDataTable(file.path(fPath, "train", "X_train.txt"))
dataTable_Test  <- ConvertToDataTable(file.path(fPath, "test" , "X_test.txt" ))
##


### 1- Merges the training and the test sets to create one data set.

#Concatenate the data tables
dataTable_Subject <- rbind(dataTable_SubjectTrain, dataTable_SubjectTest)
setnames(dataTable_Subject, "V1", "subject")

dataTable_Activity <- rbind(dataTable_ActivityTrain, dataTable_ActivityTest)
setnames(dataTable_Activity, "V1", "activityNum")

dataTable <- rbind(dataTable_Train, dataTable_Test)

#Merge columns
dataTable_Subject <- cbind(dataTable_Subject, dataTable_Activity)
dataTable <- cbind(dataTable_Subject, dataTable)

#setting key for merging
setkey(dataTable, subject, activityNum)

###

#### 2- Extracts only the measurements on the mean and standard deviation for each measurement. 

#Reading the features.txt file
dataTable_Features <- fread(file.path(fPath, "features.txt"))
setnames(dataTable_Features, names(dataTable_Features), c("featureNum", "featureName"))

#Subsetting measurements that only  have the mean and standard deviation.

dataTable_Features <- dataTable_Features[grepl("mean\\(\\)|std\\(\\)", featureName)]

#Converting column numbers to a vector of names that match columns from dataTable.

dataTable_Features$featureCode <- dataTable_Features[, paste0("V", featureNum)]


#Subsetting dataTable to these variable names

select <- c(key(dataTable), dataTable_Features$featureCode)
dataTable <- dataTable[, select, with=FALSE]

####

##### 3- Uses descriptive activity names to name the activities in the data set

#Reading activity_labels.txt file to add descriptive names to the activitie needed.

dataTable_ActivityNames <- fread(file.path(fPath, "activity_labels.txt"))
setnames(dataTable_ActivityNames, names(dataTable_ActivityNames), c("activityNum", "activityName"))

###### 4- Appropriately labels the data set with descriptive variable names. 

#Merging the labels of the activities

dataTable <- merge(dataTable, dataTable_ActivityNames, by="activityNum", all.x=TRUE)

#setting activityName as the key for the merging.

setkey(dataTable, subject, activityNum, activityName)

#using the Melt function to Convert the object into a molten data frame.

dataTable <- data.table(melt(dataTable, key(dataTable), variable.name="featureCode"))

#Merging the activity name

dataTable <- merge(dataTable, dataTable_Features[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

#Creating the new variables activity & feature as a factor class. 

dataTable$activity <- factor(dataTable$activityName)
dataTable$feature <- factor(dataTable$featureName)

#Seperate features from featureName using grepit.

grepit <- function (Ex) {
  grepl(Ex, dataTable$feature)
}

## Case 1: When Features have 3 Categories
Num <- 3
y <- matrix(seq(1, Num), nrow=Num)
x <- matrix(c(grepit("-X"), grepit("-Y"), grepit("-Z")), ncol=nrow(y))
dataTable$feature_Axis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

## Case 2: When Features have 2 Categories
Num <- 2
y <- matrix(seq(1, Num), nrow=Num)
x <- matrix(c(grepit("^t"), grepit("^f")), ncol=nrow(y))
dataTable$feature_Domain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepit("Acc"), grepit("Gyro")), ncol=nrow(y))
dataTable$feature_Instrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepit("BodyAcc"), grepit("GravityAcc")), ncol=nrow(y))
dataTable$feature_Acceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepit("mean()"), grepit("std()")), ncol=nrow(y))
dataTable$feature_Variable <- factor(x %*% y, labels=c("Mean", "SD"))

## Case 3: When Features have 1 category
dataTable$feature_Jerk <- factor(grepit("Jerk"), labels=c(NA, "Jerk"))
dataTable$feature_Magnitude <- factor(grepit("Mag"), labels=c(NA, "Magnitude"))


####### 5- From the data set in step 4, creates a second, independent tidy data set 
#######    with the average of each variable for each activity and each subject.

#Creating Tidy data set

setkey(dataTable, subject, activity, feature_Domain, feature_Acceleration, feature_Instrument, feature_Jerk, feature_Magnitude, feature_Variable, feature_Axis)
dataTable_TidyData <- dataTable[, list(count = .N, average = mean(value)), by=key(dataTable)]
dataTable_TidyData
