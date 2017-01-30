
# This in fulfilment to the Getting and Cleaning Data practical assignment 
# The url from which data was downloaded is
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# The following stores the url in the file 'url'
setwd(getwd())

# loading packages that may be required
library(dplyr)
library(plyr)

# The following creates the directory to store the files in case it is not already created
if(!file.exists("./data")){dir.create("./data")}

# Stores the url to download the data from 
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# downloads the data from the given url and stores it in the directory created earlier
download.file(Url,destfile="./data/Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")


#  Merging the training and the test sets to create one data set:

#  The data set is two folders namely "training" and "test"
### There is also a dataset containing features and that of activity labels
### These will be loaded as well

## Training data
Xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

## Test data
Xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# Features
features <- read.table('./data/UCI HAR Dataset/features.txt')

# Activity labels:
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

# The activity labels and features represent column names for the training and test data
# We use this to assign column names

colnames(Xtrain) <- features$V2 
colnames(Xtest)  <- features$V2

names1 <- c("Activity", "VolunteerID")
colnames(ytrain) <- names1[1]
colnames(subject_train) <- names1[2]
colnames(ytest) <- names1[1]
colnames(subject_test) <- names1[2]
colnames(activityLabels) <- c(names1[1],names1[2])

# This binds all the training columns together and all the tests in another dataframe
train <- cbind(ytrain,subject_train,Xtrain)
test <- cbind(ytest, subject_test, Xtest)

# We now combine the training and test datasets together
test.train <- rbind(train, test)
test.train$VolunteerID <-paste("participant", test.train$VolunteerID, sep = "")
# 2. Extracting only the measurements on the mean and standard deviation for each measurement
Cols <- colnames(test.train)
x <- c("Activity","VolunteerID","mean..","std..")

# We know work on extracting columns containing only "Activity", 'VolunteerID", "mean" and "std"

y <- grepl(paste(x, collapse = "|"), Cols) # This will produce a vector of logicals

# We are interested in only the "TRUE" response
tidy <- test.train[, y ==TRUE]


# 3. Using descriptive activity names to name the activities in the data set:
names(tidy) <- noquote(names(tidy))

#
# Changing levels in tidy$ActivityID with meaningful names
tidy$Activity <- as.character(tidy$Activity)
tidy$Activity[tidy$Activity == 1] <- "Walking"
tidy$Activity[tidy$Activity == 2] <- "Walking Upstairs"
tidy$Activity[tidy$Activity == 3] <- "Walking Downstairs"
tidy$Activity[tidy$Activity == 4] <- "Sitting"
tidy$Activity[tidy$Activity == 5] <- "Standing"
tidy$Activity[tidy$Activity == 6] <- "Laying"
tidy$Activity <- as.factor(tidy$Activity)

# Now we need to group the data and find the mean
final <-aggregate(.~VolunteerID + Activity, tidy, mean)

final <- final[order(final$VolunteerID, final$Activity),]
final <- as.data.frame(sapply(final, function(x) gsub("\"", "", x)))

# Finally, we write(export) the result to a text file
write.table(final, "finaldata.txt", row.name=FALSE)

