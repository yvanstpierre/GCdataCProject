# Zipped file archive at 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# was downloaded and all files extracted locally. The following text files were then
# manually copied to the working directory:
#    1. activity_labels.txt (names and codes of activities)
#    2. features.txt (names of measurements and related datasets columns)
#    3. subject_test.txt and subject_train.txt (subject numbers in datasets rows order)
#    4. y_test.txt and y_train.txt (activity codes in datasets rows order)
#    5. X_test.txt and X_train.txt (test and training measurements datasets)
#
# Step 1: Merging the training and the test sets to create one data set.
#    Subject numbers, activity codes and measurements datasets are merged for both the
#    training and test sets, and the resulting datasets are appended, creating
#    a large data frame called "large_msmts".
#
#    First, the data is read in its current state:

  msmt_tt <- read.table("X_test.txt")
  msmt_tr <- read.table("X_train.txt")
  act_tt <- read.table("y_test.txt")
  act_tr <- read.table("y_train.txt")
  sub_tt <- read.table("subject_test.txt")
  sub_tr <- read.table("subject_train.txt")
  
#    Then the large dataset is assembled, with activity and subject as first 2 columms:
  
  large_tt <- cbind(act_tt, sub_tt, msmt_tt)
  large_tr <- cbind(act_tr, sub_tr, msmt_tr)
  large_msmts <- rbind(large_tt, large_tr)
  
# Step 2: Extracting only the measurements on the mean and standard deviation 
# for each measurement.
#    From the file "features.txt" providing column names and numbers for the 
#    data files, names including "mean()" and "std()" are flagged and then used
#    to subset the measurements dataset by columns.Note that "mean()"-defined
#    values do not include "meanFreq()" instances, so parameter fixed=TRUE.
#    First columns are also set for activity and subject id.
  
  col_incl <- read.table("features.txt", stringsAsFactors=FALSE) 
  col_incl$V1 <- grepl("mean()",col_incl$V2, fixed=TRUE)|grepl("std()",col_incl$V2)
  firstcols <- data.frame(V1=c(TRUE, TRUE), V2=c("Activity", "Subject_ID"), stringsAsFactors=FALSE)
  col_incl <- rbind(firstcols, col_incl)
  sel_msmts <- large_msmts[, which(col_incl$V1)]
  names(sel_msmts) <- c("act_code", "sub_id", names(sel_msmts)[3:68])
  
  # Step3 : Using descriptive activity names to name the activities in the data set.
  #   Merging names provided in the activity_labels.txt file by activity code,
  #   and dropping the activity code.
  
  act_lab <- read.table("activity_labels.txt")
  names(act_lab) <- c("act_code", "actlabl")
  sel_msmts <- merge(act_lab, sel_msmts, by="act_code")
  sel_msmts <- sel_msmts[, 2:69]
  
  # Step4 : Labelling the data set with descriptive variable names.
  #    Variable names are based on the values in the col_incl dataframe. 
  #    For each variable, the name is transformed from "root-function()-A" 
  #    to "root.A.function" when A in X,Y,Z is present, or "root.function" 
  #    otherwise, with function being either "mean" or "std". Activity and
  #    Subject_ID remain as is.
  
  strnames <- strsplit(col_incl[which(col_incl$V1), 2], split="-")
  newnames <- character(68)
  for(i in 1:68) {
    newnames[i] <- unlist(strnames[i])[1]
    if(!is.na(unlist(strnames[i])[3])) {
      newnames[i] <- paste(newnames[i],".",unlist(strnames[i])[3], sep="")
    }
    if(!is.na(unlist(strnames[i])[2])) {
      if(unlist(strnames[i])[2]=="mean()") {
      newnames[i] <- paste(newnames[i],".mean", sep="")
      }
      if(unlist(strnames[i])[2]=="std()") {
      newnames[i] <- paste(newnames[i],".std", sep="")
      }
    }
  }
  names(sel_msmts)<- newnames
  
  # Step 5 : Creating a tidy data set with the average of each variable 
  #     for each activity and each subject.
  #     Here, the dplyr package is loaded and the summarised dataset is created.
  
  library(dplyr)
  
  tidy_dat <- sel_msmts %>% group_by(Activity, Subject_ID) %>% summarise_each(funs(mean))
  write.table(tidy_dat, "TidyData.txt", row.names = FALSE)
  
