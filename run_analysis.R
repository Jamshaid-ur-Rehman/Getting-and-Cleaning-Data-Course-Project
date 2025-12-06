library(dplyr)

# activity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", 
                              col.names = c("code", "activity"))

# Read features
features <- read.table("UCI HAR Dataset/features.txt", 
                       col.names = c("index", "feature"))

# Read and merge training data
train <- cbind(
  read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject"),
  read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity_code"),
  read.table("UCI HAR Dataset/train/X_train.txt")
)

# Read and merge test data  
test <- cbind(
  read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject"),
  read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity_code"),
  read.table("UCI HAR Dataset/test/X_test.txt")
)

# Step 1: Merge
merged_data <- rbind(train, test)

# Step 2: Extract mean/std columns
mean_std_cols <- grep("mean\\(\\)|std\\(\\)", features$feature)
merged_data <- merged_data[, c(1, 2, mean_std_cols + 2)]

# Step 3: Add activity names
merged_data$activity <- factor(merged_data$activity_code, 
                               levels = activity_labels$code,
                               labels = activity_labels$activity)
merged_data$activity_code <- NULL
merged_data <- merged_data[, c("subject", "activity", setdiff(names(merged_data), c("subject", "activity")))]

# Step 4: Clean variable names
clean_names <- features$feature[mean_std_cols]
clean_names <- gsub("[()-]", "_", clean_names)
clean_names <- gsub("^t", "time_", clean_names)
clean_names <- gsub("^f", "freq_", clean_names)
names(merged_data)[3:ncol(merged_data)] <- clean_names

# Step 5: Create tidy averages
tidy_avg <- merged_data %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean), .groups = "drop")

# Save results
write.table(tidy_avg, "tidy_data.txt", row.names = FALSE)
print("Analysis complete! File saved as 'tidy_data.txt'")
print(dim(tidy_avg))
