
#Merges the training and the test sets to create one data set.

#Extracts only the measurements on the mean and standard deviation for each measurement.

#ses descriptive activity names to name the activities in the data set

#Appropriately labels the data set with descriptive variable names.

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#Good luck!

require(dplyr)
require(stringr)
require(tidyr)


setwd(dir = "/Users/jdelarosa/Documents/Coursera/datasciencecoursera/getting_cleaning_data_week4/")

#read in the data from test

subject_test<-read_table(file = "./UCI HAR Dataset/test/subject_test.txt", col_names = FALSE)

X_test<-read_table(file = "./UCI HAR Dataset/test/X_test.txt", col_names = FALSE)

Y_test<- read_table(file = "./UCI HAR Dataset/test/y_test.txt", col_names = FALSE)

features<- read_table(file = "./UCI HAR Dataset/features.txt", col_names = FALSE)

#renames X_test columns with descriptive names in features
colnames(X_test) <- features$X1

#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING


test_combined<-X_test %>%
  #seledts only columns with mean or std
  select(grep(x = colnames(X_test), pattern = "mean|std")) %>%
  #combines subject_test into data frame
  mutate(subject = subject_test$X1)%>%
  #combines Y_label into data frame
  mutate(Y_label = Y_test$X1)%>%
  #Creates descriptive activity label based on Y_label using activity labels in activity_labels.txt
  mutate(Y_label_activity_desc = case_when(Y_label == "6"~"LAYING",
                                  Y_label == "5"~"STANDING",
                                  Y_label == "4"~"SITTING",
                                  Y_label == "3"~"WALKING_DOWNSTAIRS",
                                  Y_label == "2"~"WALKING_UPSTAIRS",
                                  Y_label == "1"~"WALKING"))%>%
  mutate(source = "test")%>%
  #reorders columns to bring subject to front
  select(subject, Y_label, Y_label_activity_desc, source, everything())



  #read in the data from train

subject_train<-read_table(file = "/Users/jdelarosa/Downloads/UCI HAR Dataset/train/subject_train.txt", col_names = FALSE)

X_train<-read_table(file = "/Users/jdelarosa/Downloads/UCI HAR Dataset/train/X_train.txt", col_names = FALSE)

Y_train<- read_table(file = "/Users/jdelarosa/Downloads/UCI HAR Dataset/train/y_train.txt", col_names = FALSE)

#renames X_train columns with descriptive names in features
colnames(X_train) <- features$X1


#Creates train set
train_combined<-X_train %>%
  #seledts only columns with mean or std
  select(grep(x = colnames(X_train), pattern = "mean|std")) %>%
  #combines subject_test into data frame
  mutate(subject = subject_train$X1)%>%
  #combines Y_label into data frame
  mutate(Y_label = Y_train$X1)%>%
  #Creates descriptive activity label based on Y_label using activity labels in activity_labels.txt
  mutate(Y_label_activity_desc = case_when(Y_label == "6"~"LAYING",
                                           Y_label == "5"~"STANDING",
                                           Y_label == "4"~"SITTING",
                                           Y_label == "3"~"WALKING_DOWNSTAIRS",
                                           Y_label == "2"~"WALKING_UPSTAIRS",
                                           Y_label == "1"~"WALKING"))%>%
  #creats source column
  mutate(source = "train")%>%
  #reorders columns to bring subject to front
  select(subject, Y_label, Y_label_activity_desc, source, everything())

#Combines train and test data 
combined_tidy_data <- full_join(test_combined, train_combined)%>%arrange(subject)

#Creates Summarised Tidy Data set from combined_tidy_data set of mean of all values grouped by subject and activity (activity_label)
average_combined_tidy_data <-combined_tidy_data %>% group_by(subject, Y_label)%>%
  summarise_all(funs(mean))%>%
  mutate(Y_label_activity_desc = case_when(Y_label == "6"~"LAYING", 
                                           Y_label == "5"~"STANDING",
                                          Y_label == "4"~"SITTING",
                                          Y_label == "3"~"WALKING_DOWNSTAIRS",
                                           Y_label == "2"~"WALKING_UPSTAIRS",
                                           Y_label == "1"~"WALKING"))

write.table(file = "./submitted_Tidy_Data_Step5.txt", x = average_combined_tidy_data,row.names = FALSE)                                                                                                                                               


