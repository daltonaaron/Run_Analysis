##Corsera Getting and Cleaning Data
## Aaron Dalton
##6/20/2015

##You should create one R script called run_analysis.R that does the following. 



##Check Path and Make sure the Data is in one folder##

getwd()
setwd("C:/Users/VHAPALDALTOA/My Documents/UCI HAR Dataset")


##1.Merges the training and the test sets to create one data set.

    subjecttrain <- read.table("subject_train.txt")
    subjecttest <- read.table("subject_test.txt")
    
    Xtrain <- read.table("X_train.txt")
    Xtest <- read.table("X_test.txt")
    
    ytrain <- read.table("y_train.txt")
    ytest <- read.table("y_test.txt")

# add column name for subject files
  names(subjecttrain) <- "subjectID"
  names(subjecttest) <- "subjectID"

# add column names for measurement files
  featureNames <- read.table("features.txt")
    names(Xtrain) <- featureNames$V2
    names(Xtest) <- featureNames$V2

# add column name for label files
  names(ytrain) <- "activity"
  names(ytest) <- "activity"

# combine files into one dataset
  RealTrains <- cbind(subjecttrain, ytrain, Xtrain)
  TestTrains <- cbind(subjecttest, ytest, Xtest)
    AllTrains <- rbind.data.frame(RealTrains, TestTrains)


View(Alltrains)



###Preliminary Check of the Data###
View(AllTrains)
summary(AllTrains)

##2.Extracts only the measurements on the mean 
##and standard deviation for each measurement. 


#Note column names are "mean" and "std"


STD_Mean <- grepl("mean\\(\\)", names(AllTrains)) |
            grepl("std\\(\\)", names(AllTrains))
    STD_Mean[1:2] <- TRUE
  TrainSDMean <- AllTrains[, STD_Mean]
head(TrainSDMean)

##Check the new Dataset##
colnames(TrainSDMean)
summary(TrainSDMean)
View(TestTrains)


##3.Uses descriptive activity names to name the activities in the data set.

#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING

TrainSDMean$activity <- factor(TrainSDMean$activity,
    levels = c(1,2,3,4,5,6),
    labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING","LAYING")
)

head(TrainSDMean)



##4.Appropriately labels the data set with descriptive variable names. 
#Make variable names human readable ie. AgeAtDiagnosis instead of AgeDx

names(TrainSDMean) <- gsub("Acc", "Accelerator", names(TrainSDMean))
names(TrainSDMean) <- gsub("Mag", "Magnitude", names(TrainSDMean))
names(TrainSDMean) <- gsub("Gyro", "Gyroscope", names(TrainSDMean))
names(TrainSDMean) <- gsub("^t", "time", names(TrainSDMean))
names(TrainSDMean) <- gsub("^f", "frequency", names(TrainSDMean))

head(TrainSDMean)

##5.From the data set in step 4, creates a second, 
##independent tidy data set with the average of each variable 
##for each activity and each subject.
library(plyr)
library(reshape2)
library(tidyr)

SummaryData<-aggregate(. ~subjectID + activity, TrainSDMean, mean)

write.table(SummaryData, file = "tidydata.txt",row.name=FALSE)

#One last Check#
head(SummaryData)


