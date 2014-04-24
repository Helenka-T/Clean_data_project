#Course project

# Read data
activity.names <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/activity_labels.txt")[,2]
test.data <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/test/X_test.txt")
train.data <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/train/X_train.txt")

test.activity <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/test/y_test.txt")[,1]
train.activity <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/train/y_train.txt")[,1]

test.subject <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/test/subject_test.txt")[,1]
train.subject <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/train/subject_train.txt")[,1]

interesting.features <- grep("-mean\\(\\)|-std\\(\\)", feature.names)
x_test <- x_test[,interesting.features]
x_train <- x_train[,interesting.features]

x_test$activity <- activity.names[y_test]
x_train$activity <- activity.names[y_train]

#Item1: Merging test and train data
all.data <- data.frame(mapply(append, test.data, train.data))
all.activity <- append(test.activity, train.activity)
all.subject <- append(test.subject, train.subject)
subject_test_train = data.frame(mapply(append, subject_test, subject_train))

#Item2: Extracting the mean and standard deviation for all measurment 
##Creating function to calculate mean values for x
feature.names <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/features.txt")[,2]
interesting.features <- grep("-mean\\(\\)|-std\\(\\)", feature.names)
all.data <- all.data[,interesting.features]
feature.names <- feature.names[interesting.features]

#Item3: Descriptive activity names
activity.names <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/activity_labels.txt")[,2]
activity.names <- gsub("_", " ", tolower(as.character(activity.names)))
all.data$activity <- as.factor(activity.names[all.activity])

#Item4: Labels in the data set
# Replace - by . and delete parantheses
feature.names <- gsub("-", ".", gsub("[()]", "", feature.names))
colnames(all.data) <- append(feature.names, c("activity"))

write.table(all.data, "C:/Users/Elena/Desktop/tidy_data.txt")


# Item 5: Creating the 2nd tidy data set
per.subject <- split(all.data, all.subject)
names(per.subject[[1]])
averagePerActivity <- function(frame) {
        per.activity <- split(frame, frame$activity)
        means <- lapply(per.activity, function(x) mapply(mean, x[-ncol(x)]))
        result <- cbind(do.call(rbind, means), data.frame(activity=names(means)))
        rownames(result) <- NULL
        result
}
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
# Put subject and activity to the front
tidy1 <- tidy[,c(ncol(tidy), ncol(tidy)-1, seq(1,ncol(tidy)-2))]
