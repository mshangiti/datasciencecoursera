# This script solves the course project requirement for coursera course.

run_analysis <- function()
{
  #loading required packages
  library(dplyr)
  
  ## step1: reading the files
  
        #reading the training data
        train_y <- read.table("./train/y_train.txt", header=FALSE);
        train_x <- read.table("./train/X_train.txt", header=FALSE);
        train_sub <- read.table("./train/subject_train.txt", header=FALSE);
        
        #reading the test data
        test_y <- read.table("./test/y_test.txt", header=FALSE);
        test_x <- read.table("./test/X_test.txt", header=FALSE);
        test_sub <- read.table("./test/subject_test.txt", header=FALSE);
      
        #reading the activiy names (activity_labels.txt)
        activity_labels <- read.table("./activity_labels.txt", header=FALSE);
        colnames(activity_labels) <- c("activity_id", "activity_desc"); # better names
        
        #reading the variable names (features.txt)
        features <- read.table("./features.txt", header=FALSE, stringsAsFactors = FALSE);
        #colnames(features) <- c("id", "feature"); # better names
        
        #changing the column name for the Y to activity (better variable name)
        colnames(train_y) <- c("activity"); # the training data
        colnames(test_y) <- c("activity"); # the test data
        
        #changing the column name for the subject to subject (better variable name)
        colnames(train_sub) <- c("subject"); # the training data
        colnames(test_sub) <- c("subject"); # the test data
    
        
        
        
  ## step2: merging the training and the testing datasets
  ## merging the rows FIRST, and then the columns to maintain the same order
  ## fulfilling requirement 1 (Merges the training and the test sets to create one data set).
  
        #merging the x's (ROWS)
        x <- rbind(train_x, test_x); 
      
        #merging the y's (ROWS)
        y <- rbind(train_y, test_y) 
      
        #merging the subject's (ROWS)
        subject <- rbind(train_sub, test_sub) 
      
        #merging X and Y and Subject (COLS)
        #dim(dataset)=10299x563 
        # (why 10299 rows? it is the number of the rows from the training (7352) and testing (2947) datasets)
        # (why? 563 cols? it is the number of  cols from the data (561), Subject (1) , and Y (1) datasets)
        dataset <- cbind(x,subject,y)

  
      
  
  ## step3: Making the variable and activity names more friendly (descriptive)
  #fulfills requirement 3 (Uses descriptive activity names to name the activities in the data set)
  #fulfills requirement 4 (Appropriately labels the data set with descriptive variable names)
  
        #replacing the activity numbers (1,2,3,4,5,6) with activity names from activity_labels.txt
        dataset <- dataset %>% merge(activity_labels, by.x="activity", by.y="activity_id") %>% select(-activity);
        
        #replacing the variables codenames (V1,V2,V3, etc) with more desriptive variables names from features.txt
        newColumnNames <- c();#empty vector
        #reading the variable names from the features.txt file
        for(i in 1:561)#561 because we have 561 variable in the file
        {
          newColumnNames <- c(newColumnNames, features[i,"V2"]);
        }
  
        #now we will add the last two columns which are "subject" and "activity"
        newColumnNames <- c(newColumnNames, "subject", "activity");
  
        #replacing the current column names with the new ones
        colnames(dataset) <- newColumnNames; # better names

  
  
  
  ##Step4: Extracts only the measurements on the mean and standard deviation for each measurement.
  ## fulfilling requirement two
  
         # Extracting only the measurements on the mean and standard deviation for each measurement. 
         # dim() = 10299x81 (10299 row/observation, 81 column/variable = 79 column/variable that contain either "std" or "mean" as instructed + activity (1) and subject (1) columns)
         # "activity" and "subject" were added to make sure they stay with the dataset
         dataset_sub <- dataset[,grep("std|mean|activity|subject", names(dataset))]

  
  
  #step5: creating an independent tidy dataset with the average of each variable for each activity and each subject, and returning back the result
  #fulfilling requirement five
  
  #creating the tidy dataset in one liner!
  #grouping data by activity and subject, and then taking the mean of each variable.
  # dim() = 180x81 (180 observation /81 variable)
  # why 180? since we have 6 activies and 30 subjects, and we wan the average for each activity and subject, then we will have 6*30 = 180 observation.
  dataset_tidy <- dataset_sub %>% group_by(activity, subject) %>% summarise_each(funs(mean));
  
  #writing the tidy data to a file
  write.table(dataset_tidy, file="tidy_data.txt", row.names=FALSE);
  
  #sending back the result
  dataset_tidy;
}