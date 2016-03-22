##############################################################################################
##############################################################################################
##############################################################################################


## Getting and Cleaning Data Course Project
## Tara Meyer
## 2016

## File Description - runAnalysis.r 
#  Merges training and test sets into one dataset. 
#  Extract only the mean and standard deviation for each measurement. 
#  Uses descriptive activity names for the activities in the dataset. 
#  Labels the data set with descriptive activity names. 
#  Creates a new tidy dataset with the average of each variable, for each activity and subject. 

## Utilizes the UCI HAR dataset
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 


##############################################################################################
##############################################################################################
##############################################################################################


## PREP WORKSPACE - clear previous analysis and set the current work directory.  This is 
#  where the .zip file is unzipped. 

rm(list=ls())
getwd()
setwd("/Users/taraxmeyer/Desktop/data_science/") 
library(reshape2)


##############################################################################################
##############################################################################################

# DOWNLOAD AND UNZIP THE DATA SET

filename <- "get-data.zip"

## Download and unzip the dataset:
if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method="curl")
} 

if (!file.exists("UCI HAR Dataset")) { 
    unzip(filename) 
}

##############################################################################################
##############################################################################################

# LOAD ACTIVITY LABELS AND FEATURES

activityLabels      <- read.table("UCI HAR Dataset/activity_labels.txt") 
activityLabels[,2]  <- as.character(activityLabels[,2])
features            <- read.table("UCI HAR Dataset/features.txt") 
features[,2]        <- as.character(features[,2])

##############################################################################################
##############################################################################################

# GET ONLY MEAN AND STANDARD DEVIATION FROM DATA SET

want        <-  grep(".*mean.*|.*std.*" , features [,2])
want.names  <-  features[want, 2] 
want.names  =   gsub('-mean', 'Mean', want.names) 
want.names  =   gsub('-std', 'Std', want.names) 
want.names  <-  gsub('[-()]' , ' ', want.names) 

##############################################################################################
##############################################################################################

# LOAD DATA SET

train           <-  read.table("UCI HAR Dataset/train/X_train.txt")[want]
trainActivities <-  read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects   <-  read.table("UCI HAR Dataset/train/subject_train.txt") 
train           <-  cbind(trainSubjects, trainActivities, train) 

test            <- read.table("UCI HAR Dataset/test/X_test.txt")[want] 
testActivities  <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects    <- read.table("UCI HAR Dataset/test/subject_test.txt")
test            <- cbind (testSubjects, testActivities, test) 
                                                            
##############################################################################################
##############################################################################################
                                                            
# MERGE DATA SETS, + LABELS
                                                            
totalData           <- rbind(train , test) 
colnames(totalData) <- c("subject" , "activity" , want.names ) 
                                                            
                                                            
##############################################################################################
##############################################################################################
                                                            
# CONVERT ACTIVITIES / SUBJECTS INTO FACTORS
                                                            
totalData$activity  <-  factor(totalData$activity, levels = activityLabels[ ,1], 
                        labels = activityLabels[ , 2])
totalData$subject   <-  as.factor(totalData$subject)
totalData.melted    <-  melt(totalData , id = c("subject", "activity"))
totalData.mean      <-  dcast(totalData.melted , subject + activity ~ variable, mean)
                                                            
                                                            
##############################################################################################
##############################################################################################
                                                            
# WRITE NEW TIDY DATA TXT FILE
                                                            
write.table(totalData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
                                                            