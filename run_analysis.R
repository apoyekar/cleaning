# run_analysis.R script performs following steps:
# Step 1:  Merges the training and the test sets to create one data set.
# Step 2:  Extracts only the measurements on the mean and standard deviation for each measurement. 
# Step 3:  Uses descriptive activity names to name the activities in the data set
# Step 4:  Appropriately labels the data set with descriptive variable names. 
# Step 5:  Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Step 6:  Writes tidy data set to tidy_data_set file


#read features.txt
features <- read.table("./UCI HAR Dataset/features.txt", strip.white = TRUE );

#Step 1: Merges training and test data sets to create one data set

#Step 1 (A): Training Data set
#read x_train.txt
train <- read.table("./UCI HAR Dataset/train/X_train.txt", strip.white = TRUE );

#set column names of train data set
colnames(train) <- features[,2];

#read y_train.txt for activity 
trainActivity <- read.table("./UCI HAR Dataset/train/y_train.txt", strip.white = TRUE );
colnames(trainActivity) <- "activity";

#read subject_train.txt for subject
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt", strip.white = TRUE );
colnames(trainSubject) <- "subject";

#combine x_train, y_train and subject_train using cbind
train <- cbind(trainActivity,train);
train <- cbind(trainSubject,train);


#Step 1 (B): Test Data set
#read x_test.txt
test <- read.table("./UCI HAR Dataset/test/X_test.txt", strip.white = TRUE );

#set column names of test data set
colnames(test) <- features[,2];

#read y_test.txt for activity 
testActivity <- read.table("./UCI HAR Dataset/test/y_test.txt", strip.white = TRUE );
colnames(testActivity) <- "activity";

#read subject_test.txt for subject
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt", strip.white = TRUE );
colnames(testSubject) <- "subject";

#combine x_test, y_test and subject_test using cbind
test <- cbind(testActivity,test);
test <- cbind(testSubject,test);



#combine train and test data sets using rbind
data <- rbind(train,test)
train <-NULL;
test <-NULL;
trainActivity <- NULL;
trainSubject <- NULL;
testActivity <- NULL;
testSubject <- NULL;
#=======================

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

#find columns that match mean() or std() in their column names
searchMeanStd <- grepl("std\\(\\)|mean\\(\\)", colnames(data));
#add first 2 columns for activity and subject
searchMeanStd[1:2]=TRUE;
#subset required columns from data set
data <-data[,searchMeanStd]


#Step 3: Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", strip.white = TRUE );
data$activity <- factor(data$activity,labels=activity_labels[,2]);


#step 4: Appropriately labels the data set with descriptive variable names
#Remove "-" and "()" from variable names
names <- gsub("-","",names(data))
names <- gsub("\\(\\)","",names)
names <- gsub("mean","Mean",names )
names <- gsub("std","Std",names )
names <- gsub("Acc","Accelerometer",names )
names <- gsub("Gyro","Gyroscoper",names )

#set decriptive variable names
colnames(data) <- names;


#step 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#average each variable by subject and activity
dataAvg <- aggregate(data[3:68],by=list(data$subject,data$activity),FUN=mean,na.rm=TRUE);
tidyVar <- names(dataAvg);

#change column names to descriptive ones 
tidyVar[1] <- "subject";
tidyVar[2] <- "activity";
tidyVar[3:68] <- paste0(tidyVar[3:68],"Avg");
colnames(dataAvg) <-tidyVar;

#write tidy data set to file
write.table(dataAvg, file="tidy_data_set.txt");

