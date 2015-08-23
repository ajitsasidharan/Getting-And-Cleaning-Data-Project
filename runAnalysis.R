# Get the requisite libraries
library(plyr)
library(dplyr)
setwd("C:\\Coursera\\GettingAndCleaningData\\Week 3")
# Get the data file into the temp directory
temp <- tempfile()
# Download the DataSet file into a temp directory
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
unzip(temp, list = TRUE)
# Get the activity labels
activity_labels <-read.table(unzip(temp, "UCI HAR Dataset/activity_labels.txt"))
colnames(activity_labels)<-c("Activity ID","Activity Name")
# Features 
features <- read.table(unzip(temp, "UCI HAR Dataset/features.txt"))
# Get the test data
Y_test <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))
X_test <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))
subject_test <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))
# Get the train data
Y_train <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))
X_train <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))
subject_train <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt"))
# Temp is no longer needed
# unlink(temp)
# Setting column names of the training and test datasets
colnames(X_test)<-features[,2]
colnames(Y_test)<-c("Activity ID")
colnames(subject_test)<-c("Subject ID")

colnames(X_train)<-features[,2]
colnames(Y_train)<-c("Activity ID")
colnames(subject_train)<-c("Subject ID")

# Merge the test data together
testData<-cbind(Y_test,subject_test,X_test)
# Merge the train data together
trainData<-cbind(Y_train,subject_train,X_train)
####### Requirement 1: Merge test and train data set
# Merge the test and train data together
completeData<-rbind(testData,trainData)
####### Requirement 2: Extracts only the measurements on the mean and standard deviation for each measurement
meanColIds<-grep("mean",colnames(completeData),ignore.case = TRUE)
stdColIds<-grep("std",colnames(completeData),ignore.case=TRUE)
requiredColIDs<-c(1,2,meanColIds,stdColIds)
# Get a data set with only columns providing mean and std values
dataSetWithOnlyMeanAndStdCols<-completeData[,requiredColIDs]
####### Requirement 3: Uses descriptive activity names to name the activities in the data set
dataSetWithNames <- merge(activity_labels,dataSetWithOnlyMeanAndStdCols,by.x="Activity ID",by.y="Activity ID",all=TRUE)
####### Requirement 4:Appropriately labels the data set with descriptive variable names. 
# Cleaning up the variable names
columnNames<-colnames(dataSetWithNames)
for (i in 1:length(columnNames)) 
{
    columnNames[i] = gsub("\\()","",columnNames[i])
    columnNames[i] = gsub("-std$","StdDev",columnNames[i])
    columnNames[i] = gsub("-mean","Mean",columnNames[i])
    columnNames[i] = gsub("^(t)","time",columnNames[i])
    columnNames[i] = gsub("^(f)","freq",columnNames[i])
    columnNames[i] = gsub("([Gg]ravity)","Gravity",columnNames[i])
    columnNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columnNames[i])
    columnNames[i] = gsub("[Gg]yro","Gyro",columnNames[i])
    columnNames[i] = gsub("AccMag","AccMagnitude",columnNames[i])
    columnNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columnNames[i])
    columnNames[i] = gsub("JerkMag","JerkMagnitude",columnNames[i])
    columnNames[i] = gsub("GyroMag","GyroMagnitude",columnNames[i])
};
colnames(dataSetWithNames)<-columnNames
####### Requirement 5: From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
##Cast the melted dataset according to  the average of each variable 
##for each activity and each subjec
finalData  = dataSetWithNames[,names(dataSetWithNames) != 'Activity Name'];
tidyData    = aggregate(finalData[,names(finalData) != c('Activity ID','Subject ID')],by=list('Activity ID'=finalData$'Activity ID','Subject ID' = finalData$'Subject ID'),mean);## Create a file with the new tidy dataset
finalTidyData <- merge(activity_labels,tidyData,by.x="Activity ID",by.y="Activity ID",all=TRUE)
# Export the tidyData set 
write.table(finalTidyData, './tidyData.txt',row.names=TRUE,sep='\t');
