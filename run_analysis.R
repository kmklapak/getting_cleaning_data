library(dplyr)
library(tidyr)
library(reshape2)

#Import Activity and Feature Labels
activity_labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")[,2]

#Import Test Data and place in tbl_df structure
x_test <- read.table("test\\X_test.txt")
x_test <- tbl_df(x_test)

#Replace column names with correct feature name
names(x_test) <- features

#Import Test Activity Data and place in tbl_df structure
y_test <- read.table("test\\Y_test.txt")
y_test <- tbl_df(y_test)
names(y_test) <- c("activity")

#Import Test Subject Data and place in tbl_df structure
sub_test <- read.table("test\\subject_test.txt")
sub_test <- tbl_df(sub_test)
names(sub_test) <- c("subject")

#Add datatype column as 'test'
data_type <- rep("test",2947)

#Combine all test data columns
test_data <- cbind(x_test,y_test,sub_test,data_type)

#Import Train Data and place in tbl_df structure
x_train <- read.table("train\\X_train.txt")
x_train <- tbl_df(x_train)

#Replace column names with correct feature name
names(x_train) <- features

#Import Train Activity Data and place in tbl_df structure
y_train <- read.table("train\\Y_train.txt")
y_train <- tbl_df(y_train)
names(y_train) <- c("activity")

#Import Train Subject Data and place in tbl_df structure
sub_train <- read.table("train\\subject_train.txt")
sub_train <- tbl_df(sub_train)
names(sub_train) <- c("subject")

#Add datatype column as 'train'
data_type <- rep("train",7352)

#Combine all train data columns
train_data <- cbind(x_train,y_train,sub_train,data_type)

#Comine test and train data into a single dataset
dataset <- rbind(test_data,train_data)

#Select data_type,activity,subject, and mean/std for each measurement
mean_std_data <- dataset[,grepl("activity|subject|mean\\(|std",features)]

# Replace activity number with corresponding label
mean_std_data$activity <- activity_labels[,2][match(mean_std_data$activity,activity_labels[,1])]

#Reorder Data
dataset <- select(mean_std_data,subject,activity,data_type,1:66)

#Produce Tidy Data Set of averages for each subject/activity
ids <- c("subject","activity","data_type")
data_labels <- setdiff(names(dataset),ids)
melted_data <- melt(dataset, id=ids, measure.vars = data_labels)
tidy_data   = dcast(melted_data, subject + activity ~ variable, mean)

#Clean Names
names(tidy_data) <- sub("\\(\\)","", names(tidy_data))

write.table(tidy_data, file = "./tidy_data_part5.txt")

