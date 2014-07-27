**Introduction**

The purpose of this project is to collect, work with, and clean a data set obtained from Human Activity Recognition Using Smartphones. Finally this project creates a tiny data set which contains average of measures by subject and activity.

This project contains following files:

1. Raw dataset at the link https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

2. run\_analysis.R

R script that cleans and transforms raw data set.

3. codebook.MD

A code book that describes the variables, the data, and transformations done on the raw data.

4. tidy\_data\_set.txt

This data set is output of running run\_analysis.R

Run\_analysis.R that creates tidy\_data\_set.txt contains following steps: 

# Step 1: Merges the training and the test sets to create one data set.

Reads training, activity, subject data sets and combines them using cbind.

Reads test, activity, subject data sets and combines them using cbind.

Joins training and test using rbind.

# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

Features\_info.txt contains definition of columns containing mean and standard deviation which says "mean(): Mean value, std(): Standard deviation". This step considers only those columns which contains text with either mean() or std().

# Step 3: Uses descriptive activity names to name the activities in the data set

Descriptive activity names from "UCI HAR Dataset/activity\_labels.txt" file are used to update activities in the data set.

# Step 4: Appropriately labels the data set with descriptive variable names.

Dashes and brackets are removed from variable names.

# Step 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Average of all measurable variables for each activity and each subject is calculated using aggregate function.

# Step 6: Writes tidy data set to tidy\_data\_set file

An output file with tidy\_data\_set.txt file is created.

**Getting Started**

1. Unzip raw data set obtained from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip so that "UCI HAR Dataset" folder is in the same folder where run\_analysis.R script is.

2. Open R Studio. And set the working directory to the directory where run\_analysis.R is

3. Run run\_analysis.R script.
