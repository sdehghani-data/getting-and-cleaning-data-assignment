#
#Reading activity and feature labels
#
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt") 
features <- read.table("./UCI HAR Dataset/features.txt")
#
#Reading test data
#
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
#
#Reading train data
#
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
#
#Merging the training and the test sets to create one data set
#
test  <- cbind(subject_test, y_test, X_test)
train <- cbind(subject_train, y_train, X_train)
merged <- rbind(test, train)
#
#Extracting only the measurements on the mean and standard deviation for each measurement
#
allNames <- c("subject", "activity", as.character(features$V2))
meanStdColumns <- grep("subject|activity|[Mm]ean|std", allNames, value = FALSE)
extracted <- merged[ ,meanStdColumns]
#
#Using descriptive activity names to name the activities in the data set
#
names(activity_labels) <- c("activityNumber", "activityName")
extracted$V1.1 <- activity_labels$activityName[extracted$V1.1]
#
#labeling the data set with descriptive variable names
#
Names <- allNames[meanStdColumns]    
Names <- gsub("mean", "Mean", Names)
Names <- gsub("std", "Std", Names)
Names <- gsub("gravity", "Gravity", Names)
Names <- gsub("[[:punct:]]", "", Names)
Names <- gsub("^t", "time", Names)
Names <- gsub("^f", "frequency", Names)
Names <- gsub("^anglet", "angleTime", Names)
names(extracted) <- Names
#
#Creating a second, independent tidy data set with the average of each variable for each activity and each subject
#
library(dplyr)
tidyDataset <- extracted %>% 
  group_by(activity, subject) %>%
  summarise_all(funs(mean))

write.table(tidyDataset, file = "tidyDataset.txt", row.names = FALSE)
