##
## Getting and Cleaning Data - Course Project
##
## This script performs the functions necessary to create a tidy data set, as
## called for in this class project, based on the accelerometer data provided.
## Please refer to the readme and codebook files for additional documentation.

#
# Main control function to invoke different steps in the analysis 
#
cleanHARdata <- function(){

  # This analysis runs through the following steps: 
  # 1. Merge the training and test sets to create one data set
  # 2. Extract only the measurements on the mean and std dev for ea. measurement 
  # 3. Appropriately label the data set with descriptive variable names
  # 4. Use descriptive activity names to name the activities in the data set
  # 5. Create a second, independent tidy data set with the average of each
  #    variable for each activity and each subject. 
    
  # Load needed libraries
  library(data.table)
  library(reshape2)
  
  # Initialize file path variables
  flist <- buildPaths()
  
  # 1. Merge training and test to create one data set (of measurements)
  ds <- mergeObs(flist)

  # 2. Extract only the measurements on the mean and std dev
  # in a data table.
  #   a) Get feature vector of sensor data (sd) measurements for mean and std.
  #   b) Subset based on only the cols in the features vector
  features <- getSdFeatures(flist)
  dt <- as.data.table(ds[, features$colidx])
  
  # 3. Appropriately label the data set with descriptive variable names
  labelVariables(dt, features)

  # Merge subject and activity column-wise
  dt <- as.data.table(mergeActivity(dt, flist, c("subject", "activity")))

  # Set keys on table to improve performance
  setkey(dt, subject, activity)
  
  # 4. Use descriptive names for the activities
  dt <- labelActivity(dt, flist)
  
  # 5. Create a second, independent tidy data set with the average of each
  #    variable for each activity and each subject.
  # a) Melt table to single variable (i.e., name of each measurement)
  # b) Calculate the average for each measurement for each subject and each activity
  dtm <- as.data.table(melt(dt,id = c("subject","activity")))
  dtm <- dtm[, mean(value), by=list(subject,activity,variable)]

#   dtm <- as.data.table(melt(dt,
#                             id = c("subject","activity"),
#                             variable.name = "measure",
#                             value.name = "average"))
#   dtm <- dtm[, mean(average), by=list(subject,activity,measure)]

  # Change column names - could not get the following to work:
  # melt(..., variable.name = "measure", value.name = "mean")
  # So, we set names explicitly
  setnames(dtm, c("variable", "V1"), c("measure", "mean"))

  # Output tidy data set
  write.table(dtm, file = flist$tidy, row.names=F, quote = F)
}

#
# Build paths to the files needed for this analysis
#
buildPaths <- function() {
  
  # Build data.frame contaning relative paths to required files.
  # Make indexing simple for later use
  # E.g. flist$x.train is relative path to "X_train.txt"
  dir <- "UCI\ HAR\ Dataset/"
  train.dir <- paste0(dir, "train/")
  test.dir <- paste0(dir, "test/")
  
  flist <- data.frame(
            s.train = paste0(train.dir, "subject_train.txt"),
             s.test = paste0(test.dir,  "subject_test.txt"),
            x.train = paste0(train.dir, "X_train.txt"),
             x.test = paste0(test.dir,  "X_test.txt"),
            y.train = paste0(train.dir, "y_train.txt"),
             y.test = paste0(test.dir,  "y_test.txt"),
               vars = paste0(dir, "features.txt"),
               acts = paste0(dir, "activity_labels.txt"),
               tidy = "UCI_HAR_tidy_data.txt",
            stringsAsFactors=FALSE)
}

#
# Get subset of sensor data needed for this analysis
#
getSdFeatures <- function(files) {
  
  # Get list of names for each feature/column
  features <- read.table(files$vars, sep = " ", 
                                 col.names = c("colidx", "colname"))
  
  # Make names lowercase
  features$colname = tolower(features$colname)

  # Pick columns that only deal with mean() or std() measurements
  fcols <- grep("mean\\(\\)|std\\(\\)", features$colname)  
  features[fcols, ]
}

#
# Label activity data with corresponding descriptive names
#
labelActivity <- function(dt, files) {
  
  # Read descriptive activity names
  af <- read.table(files$acts, header = F, col.names = c("act.num", "act.desc"))
  
  # Match on activity number to update with descriptive name
  dt$activity = af$act.desc[match(dt$activity, af$act.num)] 
  
  dt
}

#
# Edit column names in f to create appropriate labels for data set dt.
#
labelVariables <- function(dt, f) {
  
  # Acknowledging the lecture's guidance on naming text variables,
  # a slightly different convention is used here. Periods replace dashes
  # and whitespace to improve legibility, since the variable names are very long.
  # It was also important to easily identify original columns used in the
  # analysis for traceability purposes, since many are dropped. Therefore, 
  # feature (variable) names are prefixed with a unique ID (vNNN), where NNN
  # is the original column number.  
  ftmp <- sapply(f, function(x) sprintf("v%03d.%s", f[,1], f[,2]))[,1]
  
  # Replace dashes, commas, and whitespace with a period
  ftmp <- gsub("[-,[:blank:]]", ".", ftmp)
  
  # Use names consisting only of dots and alphanumeric characters
  ftmp <- gsub("[^[:alnum:].]", "", ftmp)
  
  # Set new column names
  setnames(dt, ftmp)
}

#
# Append subject/activity data to specified data frame
#
mergeActivity <- function(dt, files, cols) {
  
  # Read subject files - train & test entries
  sf1 <- read.table(files$s.train, header = F, col.names = cols[1], colClasses = 'numeric')
  sf2 <- read.table(files$s.test, header = F, col.names = cols[1], colClasses = 'numeric')
  
  # Append test to train to match obs order, then append as column to obs data
  dt <- cbind(dt, rbind(sf1, sf2))
  
  # Read activity files - train & test entries
  yf1 <- read.table(files$y.train, header = F, col.names = cols[2], colClasses = 'numeric')
  yf2 <- read.table(files$y.test, header = F, col.names = cols[2], colClasses = 'numeric')
 
  # Append test to train to match above order, then append as column to obs data
  dt <- cbind(dt, rbind(yf1, yf2))
}

#
# Read measurements from 2 text files and merge them row-wise.
#
mergeObs <- function(files) {

  #
  # Assume all files exist. In real life, having all the file names in data frame
  # allows for a single routine in which they can be checked for existence.
  #
  
  # Read train & test obs files. All 561 are numeric.
  xf1 <- read.table(files$x.train, header = F, colClasses = rep('numeric', 561))
  xf2 <- read.table(files$x.test, header = F, colClasses = rep('numeric', 561))
  
  # Append test to train set
  ds <- rbind(xf1, xf2)
}
