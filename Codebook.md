Samsung data analysis
========================================================
### Course project. Getting and cleaning data
```{r}
# Read data
## Activity label data
activity.names <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/activity_labels.txt")[,2]

## Test and train data (x values)
test.data <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/test/X_test.txt")
train.data <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/train/X_train.txt")

# Test and train activity data (y values)
test.activity <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/test/y_test.txt")[,1]
train.activity <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/train/y_train.txt")[,1]

## Subject data
test.subject <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/test/subject_test.txt")[,1]
train.subject <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/train/subject_train.txt")[,1]
```

### 1. Merging test and train data
```{r}
all.data <- data.frame(mapply(append, test.data, train.data))
all.activity <- append(test.activity, train.activity)
all.subject <- append(test.subject, train.subject)
```

### 2. Extracting the mean and standard deviation for all measurment 

```{r}
feature.names <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/features.txt")[,2]

#Values containing mean and standard deviation 
interesting.features <- grep("-mean\\(\\)|-std\\(\\)", feature.names) 
all.data <- all.data[,interesting.features]
feature.names <- feature.names[interesting.features]
```

### 3. Descriptive activity names

```{r}
activity.names <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/activity_labels.txt")[,2]
activity.names <- gsub("_", " ", tolower(as.character(activity.names))) # Replaces "_" by " " 
all.data$activity <- as.factor(activity.names[all.activity])
```

### 4. Labels in the data set

```{r}
feature.names <- gsub("-", ".", gsub("[()]", "", feature.names))
colnames(all.data) <- append(feature.names, c("activity"))

#First tidy data set
write.table(all.data, "C:/Users/Elena/Desktop/all_data.txt")
```

### 5. Creating the 2nd tidy data set
```{r}
per.subject <- split(all.data, all.subject) #Splits data per subject
names(per.subject[[1]])
# Calculates the mean grouped by activity inside the data frame 
averagePerActivity <- function(frame) {
        per.activity <- split(frame, frame$activity)
        means <- lapply(per.activity, function(x) mapply(mean, x[-ncol(x)]))
        result <- cbind(do.call(rbind, means), data.frame(activity=names(means)))
        rownames(result) <- NULL
        result
}
# Rearranges mean values per subject
tidy <- NA
for (i in 1:30) {
        subject <- averagePerActivity(per.subject[[i]])
        subject$subject <- rep(i, nrow(subject))
        if (is.na(tidy)) {
                tidy <- subject
        } else {
                tidy <- rbind(tidy, subject)
        }
}

# Places subject and activity to the front
tidy <- tidy[,c(ncol(tidy), ncol(tidy)-1, seq(1,ncol(tidy)-2))]
# Write final tidy data set table
write.table(tidy, "C:/Users/Elena/Desktop/tidy_data.txt")
