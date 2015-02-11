library("reshape2")
library("data.table")

setwd("C:\\Raphael\\estudos\\datasciencecoursera\\Clearing Data\\Week3")

dir_train = ".\\UCI HAR Dataset\\train"
dir_test = ".\\UCI HAR Dataset\\test"
dir_feature = ".\\UCI HAR Dataset"

#1 Merges the training and the test sets to create one data set


x_train = read.table(paste(dir_train, "\\", "X_train.txt", sep=""), sep="", header=FALSE)
subject_train = read.table(paste(dir_train, "\\", "subject_train.txt", sep=""), col.names=c("subject"))
y_train = read.table(paste(dir_train, "\\", "y_train.txt", sep="") , col.names=c("activity"))
data_train = cbind(x_train, subject_train, y_train)


x_test = read.table(paste(dir_test, "\\", "X_test.txt", sep=""), comment.char="")
subject_test = read.table(paste(dir_test, "\\", "subject_test.txt", sep=""), col.names=c("subject"))
y_test = read.table(paste(dir_test, "\\", "y_test.txt", sep=""), col.names=c("activity"))
data_test = cbind(x_test, subject_test, y_test)


data_set = rbind(data_train, data_test)


#2 Extracts only the measurements on the mean and standard deviation for each measurement
# include column names at data_set using "features.txt"
# values are in the "filtered_data_set"

feature_list = read.table(paste(dir_feature, "\\", "features.txt", sep=""), col.names = c("id", "name"))
feature_v = c(as.vector(feature_list[, "name"]), "subject", "activity")

# Loop at features to assign column names at dataset

for (i in 1:561){
        name_col = feature_v[i]
        names(data_set)[i] = name_col
}


# search for columns wich names contains "mean", "Mean" and "std"
# plus Activity and Subject colimns

filter_col_mean = data_set[ , grepl( "mean" , names( data_set ) ) ]
filter_col_Mean = data_set[ ,grepl ("Mean", names(data_set))]
filter_col_std = data_set[ , grepl( "std" , names( data_set ) ) ]
filter_col_subject = data_set[ , grepl( "subject" , names( data_set ) ) ]
filter_col_activity = data_set[ , grepl( "activity" , names( data_set ) ) ]

filtered_data_set = cbind(filter_col_mean, filter_col_Mean, filter_col_std, 
                          filter_col_subject, filter_col_activity)


#3 Uses descriptive activity names to name the activities in the data set

activities = read.table(paste(dir_feature, "\\", "activity_labels.txt", sep=""), col.names=c("id", "name"))

m = merge(filtered_data_set, activities, by.x = "filter_col_activity", by.y = "id")


#4 Appropriately labels the data set with descriptive variable names

names(m) = gsub("filter_col_activity", "Activity_id", names(m))
names(m) = gsub("mean()", "Mean", names(m), fixed = TRUE)
names(m) = gsub("std()", "Standard Deviation", names(m), fixed = TRUE)
names(m) = gsub("-", "_", names(m), fixed = TRUE)
names(m) = gsub("name", "Activity Name", names(m))
names(m) = gsub("filter_col_subject", "Subject", names(m))
names(m) = gsub(" ", "_", names(m), fixed = TRUE)


#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

dt = melt(m, id.vars = c("Subject","Activity_Name"))
tidym = dcast(dt, Subject + Activity_Name ~ variable, mean)

write.table(tidym, "tidyData.txt", quote = FALSE, row.name=FALSE)

