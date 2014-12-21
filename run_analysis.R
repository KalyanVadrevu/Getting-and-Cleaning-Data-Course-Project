run_analysis <- function() {
  
  #Reading the 'Test' data set
  xtestfilepath <- file.path("C:","Users","kvadrevu", "Documents", "cp","UCI HAR Dataset","test","X_test.txt")
  
  xtestfilepath <- normalizePath(xtestfilepath)
  
  #Reading the X_test.txt into a data frame
  xtest <- read.table(xtestfilepath,header=F)
  
  #Reading the features.txt into a data frame
  features <- read.table("C:/Users/kvadrevu/Documents/cp/UCI HAR Dataset/features.txt",header=F, as.is=T)
  
  #Capturing the feature list
  flist <- features$V2
  
  # Setting the column names for the test data to the row entries obtained from the features.txt
  colnames(xtest) <- flist
  
  #Reading the y_test.txt into a data frame
  ytest <- read.table("C:/Users/kvadrevu/Documents/cp/UCI HAR Dataset/test/y_test.txt",header=F)
  
  colnames(ytest) <- "activity"
  
  ##Adding descriptive labels to activities in the test data set
  walk_index <- ytest[,1] == 1
  ytest[walk_index,] <- "Walking"
  
  walk_up_index <- ytest[,1] == 2
  ytest[walk_up_index,] <- "Walking_upstairs"
  
  walk_down_index <- ytest[,1] == 3
  ytest[walk_down_index,] <- "Walking_downstairs"
  
  sit_index <- ytest[,1] == 4
  ytest[sit_index,] <- "Sitting"
  
  stand_index <- ytest[,1] == 5
  ytest[stand_index,] <- "Standing"
    
  lay_index <- ytest[,1] == 6
  ytest[lay_index,] <- "Laying"
  
  #Combining the test results and the activities data sets
  testAndactivity <- cbind(xtest,ytest)
  
  #Reading the subject_test.txt into a data frame
  subject_test <- read.table("C:/Users/kvadrevu/Documents/cp/UCI HAR Dataset/test/subject_test.txt",header=F)
  
  colnames(subject_test) <- "subject"
  
  #allTestData will have the test data
  allTestData <- cbind(subject_test,testAndactivity)
  
  #Building the 'Training' data set
  xtrainfilepath <- file.path("C:","Users","kvadrevu", "Documents", "cp","UCI HAR Dataset","train","X_train.txt")
  
  xtrainfilepath <- normalizePath(xtrainfilepath)
  
  xtrain <- read.table(xtrainfilepath,header=F)
  
  # Setting the column names for the train data to the row entries obtained from the features.txt
  colnames(xtrain) <- flist
  
  #Reading the y_train.txt into a data frame
  ytrain <- read.table("C:/Users/kvadrevu/Documents/cp/UCI HAR Dataset/train/y_train.txt",header=F)
  
  #Assigning column name
  colnames(ytrain) <- "activity"
  
  #Adding descriptive labels to activities in the train data set
  walk_index <- ytrain[,1] == 1
  ytrain[walk_index,] <- "Walking"
  
  walk_up_index <- ytrain[,1] == 2
  ytrain[walk_up_index,] <- "Walking_upstairs"
  
  walk_down_index <- ytrain[,1] == 3
  ytrain[walk_down_index,] <- "Walking_downstairs"
  
  sit_index <- ytrain[,1] == 4
  ytrain[sit_index,] <- "Sitting"
  
  stand_index <- ytrain[,1] == 5
  ytrain[stand_index,] <- "Standing"
  
  lay_index <- ytrain[,1] == 6
  ytrain[lay_index,] <- "Laying"
  
  #Temporary variable to hold the training data without the subject data
  cc1 <- cbind(xtrain,ytrain)
  
  #Reading training subject data in to a data frame
  subject_train <- read.table("C:/Users/kvadrevu/Documents/cp/UCI HAR Dataset/train/subject_train.txt",header=F)
  
  #Adding descriptive column name
  colnames(subject_train) <- "subject"
  
  #allTrainData will now have the training data and the subject train data
  allTrainData <- cbind(subject_train,cc1)
  
  #Merging the training and the test sets to create one data set.
  TestandTrainData <- rbind(allTestData,allTrainData)
  
  #Extracting only the measurements on the mean and standard deviation for each measurement.
  measureContainsMean <- TestandTrainData[,grep("mean()",names(TestandTrainData), fixed=T)]
  measureContainsSD <- TestandTrainData[,grep("std()",names(TestandTrainData),fixed=T)]
  
  #Padding the subject and activity data to the extracted data
  fin <- cbind(TestandTrainData$subject,measureContainsMean,measureContainsSD, TestandTrainData$activity)
  
  #Labeling the columns appropriately
  colnames(fin)[1] <- "Subject"
  colnames(fin)[68] <- "Activity"
  
  #Loading data.table library to use its functions
  library(data.table)
  
  #Creating a data table using the data table with the consolidated data
  dtProjResult <- data.table(fin)
  
  #Removing temporary variables from memory
  rm(flist,xtest,xtrain,ytest,ytrain,subject_test,subject_train,lay_index,sit_index,walk_index,walk_down_index,walk_up_index,xtestfilepath,xtrainfilepath)
  rm(cc1, fin,features,stand_index,allTrainData,TestandTrainData,measureContainsMean,measureContainsSD,testAndactivity,allTestData)
  
  #Calculating the average of each variable for each activity and each subject.
  dtProjResult <- dtProjResult[, lapply(.SD,mean), by=list(Subject, Activity)]
  
  #Creating a temporary variable to prefix 'Average of' to the column names
  AddAvgToColumn <- paste("Average of", colnames(dtProjResult), sep = " ")
  
  # Update the names of the columns starting from the third column (Column 1 and 2 are exempt from this as they don't need a prefix)
  setnames(dtProjResult,3:68, AddAvgToColumn[3:68])
  
  #removing temporary variable
  rm(AddAvgToColumn)
  
  #Order the dataset first by 'Subject' and then by 'Activity'
  setorder(dtProjResult,Subject,Activity)
  
  #Creating a second, independent tidy data set with the average of each variable for each activity and each subject.
  write.table(dtProjResult,"ProjResult.txt", row.names=FALSE)
  
  
}

