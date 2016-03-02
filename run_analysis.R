library(reshape2)

extractedFolderName <- "UCI HAR Dataset"

ensureDirectory <- function() {
  downloadUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  downloadedZipName <- "getdata-projectfiles-UCI HAR Dataset.zip"
  
  if (!file.exists(downloadedZipName)){
    download.file(downloadUrl, destfile = downloadedZipName)
  }
  
  if (!dir.exists(extractedFolderName)) {
    unzip(downloadedZipName)
  }
}

getFullFilePath <- function(folderPath, prefix, postfix) {
  fileName <- paste(prefix, postfix ,".txt", sep = "")
  fullFilePath <- file.path(folderPath, fileName)
}

getData <- function(folderName, postfix) {
  folderPath = file.path(folderName, postfix)
  
  x_file_path <- getFullFilePath(folderPath, "X_", postfix)
  activity_file_path <- getFullFilePath(folderPath, "y_", postfix)
  subject_file_path <- getFullFilePath(folderPath, "subject_", postfix)
  
  x <- read.table(x_file_path)[featuresWanted]
  activities <- read.table(activity_file_path)
  subjects <- read.table(subject_file_path)
  
  cbind(subjects, activities, x)
}

getAllData <- function() {
  train <- getData(extractedFolderName, "train")
  test <- getData(extractedFolderName, "test")
  
  allData <- rbind(train, test)
  colnames(allData) <- c("subject", "activity", featureNames)
  
  allData$activity <- factor(allData$activity, levels = activities[,1], labels = activities[,2])
  allData$subject <- as.factor(allData$subject)
  
  allData
}

ensureDirectory()

features <- read.table(file.path(extractedFolderName, "features.txt"))
activities <- read.table(file.path(extractedFolderName, "activity_labels.txt"))
featuresWanted <- grep("-(mean|std)\\(\\)", features[,2])

featureNames <- features[featuresWanted,2]
featureNames = gsub('-mean', 'Mean', featureNames)
featureNames = gsub('-std', 'Std', featureNames)
featureNames <- gsub('[-()]', '', featureNames)

allData <- getAllData()

allData.melted <- melt(allData, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)

