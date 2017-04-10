## Title: run_analysis.R
## Author: Emilie H. Wolf
## Date: April 9, 2017
## Description: This R script reads in several datasets, combines them into a 
## single dataset, extracts only the mean and standard deviation columns, and
## edits the variable names and values so they are descriptive and tidy.

## Please make sure this script is placed inside the unzipped folder 'UCI HAR Dataset'
## Please also set the working directory to the 'UCI HAR Dataset' folder

## Check for plyr library and install
if (!require("plyr")) {
        install.packages("plyr")
}
library(plyr)

## Read in the key files
train <- read.table("./train/X_train.txt",header = FALSE)
test <- read.table("./test/X_test.txt",header = FALSE)
trainlabels <- read.table("./train/y_train.txt",header = FALSE)
testlabels <- read.table("./test/y_test.txt",header = FALSE)
trainsubjects <- read.table("./train/subject_train.txt",header = FALSE)
testsubjects <- read.table("./test/subject_test.txt",header=FALSE)
activitylabels <- read.table("./activity_labels.txt",header = FALSE)
features <- read.table("./features.txt",header = FALSE)

## Create column names
names(test) <- features$V2
names(train) <- features$V2
names(testsubjects) <- "Subject"
names(trainsubjects) <- "Subject"
names(testlabels) <- "Activity"
names(trainlabels) <- "Activity"

## Merge all te data
TRAIN <- cbind(trainsubjects,trainlabels,train)
TEST <- cbind(testsubjects,testlabels,test)
data <- rbind(TRAIN,TEST)

## Remove objects no longer needed
rm(test,testlabels,testsubjects,train,trainlabels,trainsubjects)
rm(TEST,TRAIN,features)

## Extract only the columns with "mean" or "std" in their name
tidy <- data[,grepl("[Mm]ean|[Ss]td|Subject|Activity",names(data))]

rm(data)

## Change the values from numbers to descriptive strings
for (i in 1:6) { 
        tidy$Activity[tidy$Activity == i] <- as.character(activitylabels[i,2])
}
rm(i,activitylabels)

## Factorize the Activity and Subject columns
tidy$Activity <- factor(tidy$Activity)
tidy$Subject <- factor(tidy$Subject)

## Tidy up the column names a little
names(tidy)<-gsub("BodyBody", "Body", names(tidy))
names(tidy)<-gsub("-mean()", "Mean", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("-std()", "STD", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("-freq()", "Freq", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("angle", "Angle", names(tidy))
names(tidy)<-gsub("gravity", "Grav", names(tidy))
names(tidy)<-gsub("\\(","",names(tidy))
names(tidy)<-gsub(")","",names(tidy))
names(tidy)<-gsub(",","",names(tidy))
names(tidy)<-gsub("Gravity","Grav",names(tidy))

## Create another dataset with the average of each variable, grouped by subject and activity
tidymean <- ddply(tidy, c("Subject","Activity"), numcolwise(mean))
write.table(tidymean, file = "./tidymean.txt", row.names = FALSE)

print("Created datasets 'tidy' and 'tidymean' and created tidymean.txt")