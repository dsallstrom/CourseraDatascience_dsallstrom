## Getting and Cleaning Data Final Project
## By: Duncan Sallstrom


## Install and load dyplr package
install.packages("dplyr")
library(dplyr)


## Download file into R
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                destfile="~/desktop/CourseraData")


## Read datasets into R
test_dta <- read.table("~/Desktop/UCI HAR Dataset/test/X_test.txt", sep="")
test_dta_y <- read.table("~/Desktop/UCI HAR Dataset/test/Y_test.txt", sep="")
test_dta_subject <- read.table("~/Desktop/UCI HAR Dataset/test/subject_test.txt", sep="")
train_dta <- read.table("~/Desktop/UCI HAR Dataset/train/X_train.txt", sep="")
train_dta_y <- read.table("~/Desktop/UCI HAR Dataset/train/Y_train.txt", sep="")
train_dta_subject <- read.table("~/Desktop/UCI HAR Dataset/train/subject_train.txt", sep="")
varnames <- read.table("~/Desktop/UCI HAR Dataset/features.txt")


## NOTE: You may need to play with your working directories. I set this up to download the files onto
## the desktop, and the code reads it from there. 


## Function merges TestTypes and SubjectNames onto the test and train data.
## Also stacks the test and train data.
stack_data <- function(){
    T1 <- test_dta
    T2 <- rename(test_dta_y, V562 = V1)
    T3 <- rename(test_dta_subject, V563 = V1)
    T4 <- cbind(T1, T2, T3)
    
    TR1 <- train_dta
    TR2 <- rename(train_dta_y, V562 = V1)
    TR3 <- rename(train_dta_subject, V563 = V1)
    TR4 <- cbind(TR1, TR2, TR3)

    Dta <- rbind(T4, TR4)
    
    Dta
}

## Storing this data into the global environment.
## This script could have been one function, but I felt this method increased readability.
Dta <- stack_data()


name_edit <- function() {

    T1 <- Dta

    ## Attach variable names
    NamesVector <- c(as.vector(as.character(varnames$V2)), "TestType", "SubjectNumber")
    colnames(T1) <- NamesVector

    ## Only keep variables having to deal with mean or standard deviation measurements.
    ## Plus the subject number and testtype variables.
    T2 <- T1[, grep("std|mean|TestType|SubjectNumber", names(T1))]
    
    ## Clean variable names.
    nameslist1 <- names(T2)
    nameslist2 <- list()
    for (i in nameslist1) {
        if (grepl("t", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "t")}
        if (grepl("f", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "f")}
        if (grepl("mean", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "Mean")}
        if (grepl("std", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "Std")}
        if (grepl("Acc", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "Accel")}
        if (grepl("Gyro", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "Gyro")}
        if (grepl("Jerk", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "Jerk")}
        if (grepl("Mag", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "Mag")}
        if (grepl("Gravity", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "Gravity")}
        if (grepl("Body", i) == TRUE) {nameslist2[i] = paste0(nameslist2[i], "Body")}
      
    }
    
    ## Cut down the nameslist and substitute NULL with ""
    ## Also add TestType and SubjectNumber
    nameslist3 <- nameslist2[1:79]
    nameslist4 <- gsub("NULL", "", nameslist3)
    nameslist5 <- c(nameslist4, "TestType", "SubjectNumber")
    nameslist5
    
    colnames(T2) <- nameslist5
    T2
    
}

## Dta2 is the final, pre-meansed dataset
Dta2 <- name_edit()


## Function to take the means for subjectnumber and testype by_groups.
Means <- function(){

    T1 <- Dta2
    T2 <- aggregate(T1[,1:79], list(T1[,80], T1[,81]), mean)
    
    colnames(T2) <- c("TestType", "SubjectNumber", names(T1[,1:79]))
    T2

}

Dta3 <- Means()

write.table(Dta2, "MovementData", row.name=FALSE)
write.table(Dta3, "MovementDataMeans", row.name=FALSE)
