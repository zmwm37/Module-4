# MODULE 4 _ WEEK 4 ASSIGNMENT
library(dplyr)
# 1 Merges the training and the test sets to create one data set.

activity_desc<- read.table("C:\\Users\\zmeitus\\Documents\\Coursera\\Getting and Cleaning Data\\UCI HAR Dataset\\activity_labels.txt")

colnames(activity_desc) <- c("activity","activityDesc")

features<-read.table("C:\\Users\\zmeitus\\Documents\\Coursera\\Getting and Cleaning Data\\UCI HAR Dataset\\features.txt")

test_labels<- read.table("C:\\Users\\zmeitus\\Documents\\Coursera\\Getting and Cleaning Data\\UCI HAR Dataset\\test\\y_test.txt")

colnames(test_labels) <- c("activity")
test <- read.table("C:\\Users\\zmeitus\\Documents\\Coursera\\Getting and Cleaning Data\\UCI HAR Dataset\\test\\X_test.txt", col.names = features$V2)


train_labels <- read.table("C:\\Users\\zmeitus\\Documents\\Coursera\\Getting and Cleaning Data\\UCI HAR Dataset\\train\\y_train.txt")
colnames(train_labels) <- c("activity")

train <- read.table("C:\\Users\\zmeitus\\Documents\\Coursera\\Getting and Cleaning Data\\UCI HAR Dataset\\train\\X_train.txt", col.names = features$V2)


full_labels<- rbind(test_labels,train_labels)

full <- rbind(test,train)


# 2 Extracts only the measurements on the mean and standard deviation for each measurement.

sub_1<-full[,c(grep("mean",features$V2),grep("std",features$V2))] 
fullMeanStd <- sub_1[,grep("meanFreq",colnames(sub_1),invert = T)] %>% cbind(full_labels)# grep also pulls meanFreq, excluding here


# 3 Uses descriptive activity names to name the activities in the data set
fullMeanStd2 <- left_join(fullMeanStd,activity_desc, by = c("activity"))


# 4 Appropriately labels the data set with descriptive variable names.
colnames(fullMeanStd2) <-
        c(grep(
                "meanFreq",
                c(
                        grep("mean", features$V2, value = T),
                        grep("std", features$V2, value = T)
                ),
                invert = T,
                value = T
        ),
        "activity",
        "activity_desc") #names seemed to truncate from earlier, so I redo them here

# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#load the subject labels
subject_test <- read.table("C:\\Users\\zmeitus\\Documents\\Coursera\\Getting and Cleaning Data\\UCI HAR Dataset\\test\\subject_test.txt")

subject_train <- read.table("C:\\Users\\zmeitus\\Documents\\Coursera\\Getting and Cleaning Data\\UCI HAR Dataset\\train\\subject_train.txt")

subject_full <- rbind(subject_test, subject_train)
colnames(subject_full) <- c("subject")

#join on subject labels to main dataset
fullMeanStd3 <- cbind(fullMeanStd2,subject_full)

#create tidy dataset of average by subjet and activity
fullMeanStd3_avg <- fullMeanStd3 %>% group_by(activity,activity_desc,subject) %>% summarise_all(mean)

write.table(fullMeanStd3_avg, file = "C:\\Users\\zmeitus\\Documents\\Coursera\\Getting and Cleaning Data\\module 4 assignment - tidy.txt")