
##run_analysis.R
addLabels <- function(the_dataframe,col_involved) {
levels(the_dataframe[[col_involved]])[levels(the_dataframe[[col_involved]])==
"1"] <- "walking"
levels(the_dataframe[[col_involved]])[levels(the_dataframe[[col_involved]])==
"2"] <- "walking_upstairs"
levels(the_dataframe[[col_involved]])[levels(the_dataframe[[col_involved]])==
"3"] <- "walking_downstairs"
levels(the_dataframe[[col_involved]])[levels(the_dataframe[[col_involved]])==
"4"] <- "sitting"
levels(the_dataframe[[col_involved]])[levels(the_dataframe[[col_involved]])==
"5"] <- "standing"
levels(the_dataframe[[col_involved]])[levels(the_dataframe[[col_involved]])==
"6"] <- "laying"
the_dataframe # to return the entire frame with changes done on column involved
}
## 1. Merging the training and the test sets to create one data set.
test_set_subject_ids_as_table <- read.table("test/subject_test.txt")
test_set_activities_as_table <- read.table("test/y_test.txt")
test_set_measurements <- read.table ("test/X_test.txt", sep="") # Since
test_table <- cbind (test_set_activities_as_table , test_set_measurements)
colnames(test_table)[1] <- "activity" #In order to keep things straight I am
test_table <- cbind (test_set_subject_ids_as_table , test_table)
colnames(test_table)[1] <- "subjectid" #In order to keep things clear I am
train_set_subject_ids_as_table <- read.table("train/subject_train.txt")
train_set_activities_as_table <- read.table("train/y_train.txt")
train_set_measurements <- read.table ("train/X_train.txt", sep="")
train_table <- cbind (train_set_activities_as_table , train_set_measurements)
colnames(train_table)[1] <- "activity"
train_table[1] <- factor(train_table[[1]])
train_table <- cbind (train_set_subject_ids_as_table , train_table)
colnames(train_table)[1] <- "subjectid"
merged_data <- rbind (train_table, test_table)
merged_data <- merged_data[order(merged_data$subjectid, merged_data$activity), ]
row.names(merged_data) <- NULL
merged_data[1] <- factor(merged_data[[1]]) # thanks for the question
merged_data[2] <- factor(merged_data[[2]])
missing_values <- sum(is.na(merged_data))
print(paste("There are ", missing_values," missing values in the merged data.", collapse=""))
1] "There are  0  missing values in the merged data."
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
extracted_values_vector <- c((1 + 2), (2 + 2), (3 + 2), (4 + 2), (5 + 2), (6 +
2), (41 + 2), (42 + 2), (43 + 2), (44 + 2), (45 + 2), (46 + 2), (81 +
2), (82 + 2), (83 + 2), (84 + 2), (85 + 2), (86 + 2), (121 + 2), (122 +
2), (123 + 2), (124 + 2), (125 + 2), (126 + 2), (161 + 2), (162 + 2), (163 +
2), (164 + 2), (165 + 2), (166 + 2), (201 + 2), (202 + 2), (214 + 2), (215 +
2), (227 + 2), (228 + 2), (240 + 2), (241 + 2), (253 + 2), (254 + 2), (266 +
2), (267 + 2), (268 + 2), (269 + 2), (270 + 2), (271 + 2), (345 + 2), (346 +
2), (347 + 2), (348 + 2), (349 + 2), (350 + 2), (424 + 2), (425 + 2), (426 +
2), (427 + 2), (428 + 2), (429 + 2), (503 + 2), (504 + 2), (516 + 2), (517 +
2), (529 + 2), (530 + 2), (542 + 2), (543 + 2) )
first_two_columns_vector <-c(1,2) # want to keep because I put subject ids and activities there
columns_to_keep_vector <- append (first_two_columns_vector, extracted_values_vector )
mean_std_data <- merged_data[ , columns_to_keep_vector]
## 3. Uses descriptive activity names to name the activities in the data set.
mean_std_data <- addLabels(mean_std_data, 2)
## 4. Appropriately labels the data set with descriptive variable names.
colnames(mean_std_data)[3:5] <- c(
"mean acceleration of the body in x-axis of the phone",
"mean acceleration of the body in y-axis of the phone",
"mean acceleration of the body in z-axis of the phone")
colnames(mean_std_data)[6:8] <- c(
"std deviation of acceleration of the body in x-axis of the phone",
"std deviation of acceleration of the body in y-axis of the phone",
"std deviation of acceleration of the body in z-axis of the phone")
colnames(mean_std_data)[9:11] <- c(
"mean acceleration of gravity in x-axis of the phone",
"mean acceleration of gravity in y-axis of the phone",
"mean acceleration of gravity in z-axis of the phone")
colnames(mean_std_data)[12:14] <- c(
"std deviation of acceleration of gravity in x-axis of the phone",
"std deviation of acceleration of gravity in y-axis of the phone",
"std deviation of acceleration of gravity in z-axis of the phone") #46
colnames(mean_std_data)[15:17] <- c(
"mean acceleration during jerk signals of the body in x-axis of the phone",
"mean acceleration during jerk signals of the body in y-axis of the phone",
"mean acceleration during jerk signals of the body in z-axis of the phone")
colnames(mean_std_data)[18:20] <- c(
"std deviation of acceleration during jerk signals of body
in x-axis of the phone",
"std deviation of acceleration during jerk signals of body
in y-axis of the phone",
"std deviation of acceleration during jerk signals of body
in z-axis of the phone") #86
colnames(mean_std_data)[21:23] <- c(
"mean angular velocity of the body in x-axis of the phone",
"mean angular velocity of the body in y-axis of the phone",
"mean angular velocity of the body in z-axis of the phone")
colnames(mean_std_data)[24:26] <- c(
"std deviation of angular velocity of the body in x-axis of the phone",
"std deviation of angular velocity of the body in y-axis of the phone",
"std deviation of angular velocity of the body in z-axis of the phone") #126
colnames(mean_std_data)[27:29] <- c(
"mean angular velocity during jerk signals of the body in x-axis of the phone",
"mean angular velocity during jerk signals of the body in y-axis of the phone",
"mean angular velocity during jerk signals of the body in z-axis of the phone")
colnames(mean_std_data)[30:32] <- c(
"std deviation of angular velocity during jerk signals of body
in x-axis of the phone",
"std deviation of angular velocity during jerk signals of body
in y-axis of the phone",
"std deviation of angular velocity during jerk signals of body
in z-axis of the phone") #166
colnames(mean_std_data)[33:34] <- c(
"mean magnitude of the acceleration of the body",
"std deviation of the magnitude of the acceleration of the body") #202
colnames(mean_std_data)[35:36] <- c(
"mean magnitude of the acceleration of gravity",
"std deviation of the magnitude of the acceleration of gravity") #215
colnames(mean_std_data)[37:38] <- c(
"mean magnitude of the acceleration of the body during jerk signals",
"std deviation of the magnitude of the acceleration of the body during
jerk signals") #228
colnames(mean_std_data)[39:40] <- c(
"mean magnitude of the angular velocity of the body",
"std deviation of the magnitude of the angular velocity of the body") #241
colnames(mean_std_data)[41:42] <- c(
"mean magnitude of the angular velocity of the body during jerk signals",
"std deviation of the magnitude of the angular velocity of the body
during jerk signals") #254
colnames(mean_std_data)[43:45] <- c(
"mean frequency domain signals of the acceleration of the body in x-axis
of the phone",
"mean frequency domain signals of acceleration of the body in y-axis of
the phone",
"mean frequency domain signals acceleration of the body in z-axis of the
phone")
colnames(mean_std_data)[46:48] <- c(
"std deviation of the frequency domain signals of acceleration of the
body in x-axis of the phone",
"std deviation of the frequency domain signals of acceleration of the
body in y-axis of the phone",
"std deviation of the frequency domain signals of acceleration of the
body in z-axis of the phone") #271
colnames(mean_std_data)[49:51] <- c(
"mean frequency domain signals during jerk signals for the acceleration
of the body in x-axis of the phone",
"mean frequency domain signals during jerk signals for the acceleration
of the body in y-axis of the phone",
"mean frequency domain signals during jerk signals for the acceleration
phone") #347
colnames(mean_std_data)[52:54] <- c(
"std deviation of the frequency domain signals during jerk signals for
the acceleration of the body in x-axis of the phone",
"std deviation of the frequency domain signals during jerk signals for
the acceleration of the body in y-axis of the phone",
"std deviation of the frequency domain signals during jerk signals for
the acceleration of the body in z-axis of the phone") #424
colnames(mean_std_data)[55:57] <- c(
"mean frequency domain signals of the angular velocity of the body in x-axis
of the phone",
"mean frequency domain signals of angular velocity of the body in y-axis of
the phone",
"mean frequency domain signals angular velocity of the body in z-axis of the
phone")
colnames(mean_std_data)[58:60] <- c(
"std deviation of the frequency domain signals of angular velocity of the
body in x-axis of the phone",
"std deviation of the frequency domain signals of angular velocity of the
body in y-axis of the phone",
"std deviation of the frequency domain signals of angular velocity of the
body in z-axis of the phone") #429
colnames(mean_std_data)[61:62] <- c(
"mean magnitude of the frequency domain signals of the acceleration of
the body",
"std deviation of the frequency domain signals of the magnitude of the
acceleration of the body") #504
colnames(mean_std_data)[63:64] <- c(
"mean magnitude of the frequency domain signals of the maganitude of
acceleration of the body during jerk signals",
"std deviation of the frequency domain signals of the magnitude of the
acceleration of the body during jerk signals") #517
colnames(mean_std_data)[65:66] <- c(
"mean magnitude of the frequency domain signals of the angular velocity
of the body",
"std deviation of the frequency domain signals of the magnitude of the
angular velocity of the body") #530
colnames(mean_std_data)[67:68] <- c(
"mean magnitude of the frequency domain signals of the angular velocity
of the body during jerk signals",
"std deviation of the frequency domain signals of the magnitude of the
angular velocity of the body during jerk signals") #543
## 5. Creates a second, independent tidy data set with the average of each
library(reshape2)
readied_data <- melt(mean_std_data, id.vars=c("subjectid", "activity"))
tidy_data_of_means <- dcast(readied_data, subjectid + activity ~ variable, mean)
write.table(tidy_data_of_means, "tidy_data_of_means.txt")
