The tidy data set a set of variables for each activity and each subject. 
10299 instances are split into 180 groups (30 subjects and 6 activities), 
66 mean and standard deviation features are averaged for each group.  

The resulting data table has 180 rows and 69 columns - 33 Mean variables + 33 Standard deviation variables + 1 Subject ( 1 of the 30 test subjects) + ActivityName + ActivityNum

The tidy data set’s first row is the header containing the names for each column.



ID Variables

"Subject"
"Activity"
Measurement Variables

"tBodyAcc Mean X"
"tBodyAcc Mean Y"
"tBodyAcc Mean Z"
"tBodyAcc StanDev X"
"tBodyAcc StanDev Y"
"tBodyAcc StanDev Z"
"tGravityAcc Mean X"
"tGravityAcc Mean Y"
"tGravityAcc Mean Z"
"tGravityAcc StanDev X"
"tGravityAcc StanDev Y"
"tGravityAcc StanDev Z"
"tBodyAccJerk Mean X"
"tBodyAccJerk Mean Y"
"tBodyAccJerk Mean Z"
"tBodyAccJerk StanDev X"
"tBodyAccJerk StanDev Y"
"tBodyAccJerk StanDev Z"
"tBodyGyro Mean X"
"tBodyGyro Mean Y"
"tBodyGyro Mean Z"
"tBodyGyro StanDev X"
"tBodyGyro StanDev Y"
"tBodyGyro StanDev Z"
"tBodyGyroJerk Mean X"
"tBodyGyroJerk Mean Y"
"tBodyGyroJerk Mean Z"
"tBodyGyroJerk StanDev X"
"tBodyGyroJerk StanDev Y"
"tBodyGyroJerk StanDev Z"
"tBodyAccMag Mean "
"tBodyAccMag StanDev "
"tGravityAccMag Mean "
"tGravityAccMag StanDev "
"tBodyAccJerkMag Mean "
"tBodyAccJerkMag StanDev "
"tBodyGyroMag Mean "
"tBodyGyroMag StanDev "
"tBodyGyroJerkMag Mean "
"tBodyGyroJerkMag StanDev "
"fBodyAcc Mean X"
"fBodyAcc Mean Y"
"fBodyAcc Mean Z"
"fBodyAcc StanDev X"
"fBodyAcc StanDev Y"
"fBodyAcc StanDev Z"
"fBodyAcc Mean Freq X"
"fBodyAcc Mean Freq Y"
"fBodyAcc Mean Freq Z"
"fBodyAccJerk Mean X"
"fBodyAccJerk Mean Y"
"fBodyAccJerk Mean Z"
"fBodyAccJerk StanDev X"
"fBodyAccJerk StanDev Y"
"fBodyAccJerk StanDev Z"
"fBodyAccJerk Mean Freq X"
"fBodyAccJerk Mean Freq Y"
"fBodyAccJerk Mean Freq Z"
"fBodyGyro Mean X"
"fBodyGyro Mean Y"
"fBodyGyro Mean Z"
"fBodyGyro StanDev X"
"fBodyGyro StanDev Y"
"fBodyGyro StanDev Z"
"fBodyGyro Mean Freq X"
"fBodyGyro Mean Freq Y"
"fBodyGyro Mean Freq Z"
"fBodyAccMag Mean "
"fBodyAccMag StanDev "
"fBodyAccMag Mean Freq"
"fBodyBodyAccJerkMag Mean "
"fBodyBodyAccJerkMag StanDev "
"fBodyBodyAccJerkMag Mean Freq"
"fBodyBodyGyroMag Mean "
"fBodyBodyGyroMag StanDev "
"fBodyBodyGyroMag Mean Freq"
"fBodyBodyGyroJerkMag Mean "
"fBodyBodyGyroJerkMag StanDev "
"fBodyBodyGyroJerkMag Mean Freq"

The Activity variable describes the activity being measured as proformed by each subject and takes values of:

LAYING
SITTING
STANDING
WALKING
WALKING_DOWNSTAIRS
WALKING_UPSTAIRS
