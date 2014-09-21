# Author: Siamak Keyvani
# Source of data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# This R script does the following:
# 1. Merges the training and the test sets to create one data set.

xTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
xTest <- read.table("UCI HAR Dataset/test/X_test.txt")
xMerged <- rbind(xTrain, xTest)

yTrain <- read.table("UCI HAR Dataset/train/y_train.txt")
yTest <- read.table("UCI HAR Dataset/test/y_test.txt")
yMerged <- rbind(yTrain, yTest)

subTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
subTest <- read.table("UCI HAR Dataset/test/subject_test.txt")
subMerged <- rbind(subTrain, subTest)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("UCI HAR Dataset/features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
xMerged <- xMerged[, indices_of_good_features]
names(xMerged) <- features[indices_of_good_features, 2]
names(xMerged) <- gsub("\\(|\\)", "", names(xMerged))
names(xMerged) <- tolower(names(xMerged))  # see last slide of the lecture Editing Text Variables (week 4)

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
yMerged[,1] = activities[yMerged[,1], 2]
names(yMerged) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(subMerged) <- "subject"
cleaned <- cbind(subMerged, yMerged, xMerged)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(subMerged)[,1]
numSubjects = length(unique(subMerged)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "tidy_data.txt")