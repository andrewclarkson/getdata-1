##
## Getting and Cleaning Data Class Project
## Andrew Clarkson 
## May 22nd, 2016
##

library(dplyr)

# Did 4 first because it was easier that way

##
## 4. Appropriately labels the data set with descriptive variable names:
##

# Read feature labels
features <- read.table("features.txt", header = FALSE, col.names = c("column", "variable"))

# Read the test variables using the feature names
x_test <- read.table("test/X_test.txt", col.names = features$variable)

# Read the activity class
y_test <- read.table("test/y_test.txt", col.names = c("activity"))

# Read the subject label
subject_test <- read.table("test/subject_test.txt", col.names = c("subject"))

# Combine the data frames horizontally
test <- cbind(y_test, subject_test, x_test)

# Do the same for train
x_train <- read.table("train/X_train.txt", col.names = features$variable)
y_train <- read.table("train/y_train.txt", col.names = c("activity"))
subject_train <- read.table("train/subject_train.txt", col.names = c("subject"))
train <- cbind(y_train, subject_train, x_train)

##
## 1. Merges the training and the test sets to create one data set
##

# Combine the data frames vertically
activity <- rbind(train, test)

##
## 2. Select only the averages and std
##

activity_subset <- activity %>% 
  # Select only the averages and std using regex
  select(activity, subject, matches("mean|std", ignore.case = TRUE))

##
## 3. Uses descriptive activity names to name the activities in the data set
##

# Get the labels
labels <- read.table("activity_labels.txt", col.names = c("activity", "label"))

activity_labeled <- activity_subset %>%
  # Join with the labels
  inner_join(labels, by="activity") %>% 
  # Make the label the new activity variable
  mutate(activity=label) %>%
  select(-label)

##
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##

activity_labeled %>% 
  group_by(activity, subject) %>%
  # Take the mean of all non-group variables
  summarise_each(funs(mean)) %>%
  # Write out the tidy file
  write.table(file = "activity_tidy.txt", row.names = FALSE)

