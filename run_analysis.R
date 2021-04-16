# Getting and Cleaning Data Assignment

library(dplyr)

read.table_instructions <- list(
  file = list(
    activity_labels = "UCI HAR Dataset/activity_labels.txt",
    features = "UCI HAR Dataset/features.txt",
    subject_train = "UCI HAR Dataset/train/subject_train.txt",
    y_train = "UCI HAR Dataset/train/y_train.txt",
    X_train = "UCI HAR Dataset/train/X_train.txt",
    subject_test = "UCI HAR Dataset/test/subject_test.txt",
    y_test = "UCI HAR Dataset/test/y_test.txt",
    X_test = "UCI HAR Dataset/test/X_test.txt"
  ),
  colClasses = list(
    activity_labels = c("integer", "character"),
    features = c("integer", "character"),
    subject_train = "integer",
    y_train = "integer",
    X_train = rep("numeric", 561),
    subject_test = "integer",
    y_test = "integer",
    X_test = rep("numeric", 561)
  ),
  nrows = list(
    activity_labels = 6,
    features = 561,
    subject_train = 7352,
    y_train = 7352,
    X_train = 7352,
    subject_test = 2947,
    y_test = 2947,
    X_test = 2947
  )
)

data_files <- with(read.table_instructions,
                   Map(read.table,
                       file = file, colClasses = colClasses, nrows = nrows,
                       quote = "", comment.char = "",
                       stringsAsFactors = FALSE))

message("    ...data files were successfully loaded into R, \n",
        "       in the list with name 'data_files'.")


#Step 1: Merge Data

merge_data <- with(data_files,
                    rbind(cbind(subject_train, y_train, X_train),
                          cbind(subject_test,  y_test,  X_test)))

#Step 2: Extract mean and stdev for each measurement

extract_feature <- grep("mean\\(\\)|std\\(\\)",
                        data_files$features[[2]])

extract_add <- c(1, 2, extract_data + 2)

extract_data <- merge_data[ , extract_add]

#Step 3: Add descriptive activity names

extract_data[[2]] <- factor(extract_data[[2]],
                           levels = data_files$activity_labels[[1]],
                           labels = data_files$activity_labels[[2]])

#Step 4: Add appropriate data labels

descriptive_names <- data_files$features[[2]][extract_feature]


## Create a tidy data set with appropriate labels for the variable names
tidy_data <- extract_data
names(tidy_data) <- c("subject", "activity", descriptive_names)


#Step 5: Create separate tidy dataset

tidy_data_summary <- tidy_data %>% group_by(subject,activity) %>% summarize_all(funs(mean)) %>% ungroup()

new_names <- c(names(tidy_data_summary[c(1,2)]),
                           paste0("Avg-", names(tidy_data_summary[-c(1, 2)])))
names(tidy_data_summary) <- new_names

#Export Table

write.table(tidy_data_summary, "tidy_data_summary.txt", row.names = FALSE)
