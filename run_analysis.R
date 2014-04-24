
##Download test and train data for the analysis

x_test <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/test/X_test.txt")
x_train <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("C:/Users/Elena/Desktop/UCI HAR Dataset/train/y_train.txt")

#Item1: Merging test and train data

x_test_train = data.frame(mapply(append, x_test, x_train))
y_test_train = data.frame(mapply(append, y_test, y_train))

#Item2: Extracting the mean and standard deviation for all measurment 
##Creating function to calculate mean values for x
mean_data <- function (data, id = 1:561){
        p <- c()
        for (i in id){
                p <- c(p, mean(data[,i]))
        }
        p
}

##Calculating mean values for x
mean_x = mean_data(x_test_train)
##Calculating mean values for y
mean_y = sapply(y_test_train, mean)

##Creating a function to calculate standard deviation for x
st_dev <- function (data, id = 1:561){
        p <- c()
        for (i in id){
                p <- c(p, sd(data[,i]))
        }
        p
}

##Calculating standard deviation for x
std_x = st_dev(x_test_train)
##Calculating standard deviation for y
std_y = sapply(y_test_train, sd)

x_all <- data.frame(mapply(append, x_test_train, mean_x))
x_all <- data.frame(mapply(append, x_all, std_x))
y_all <- data.frame(mapply(append, y_test_train, mean_y))
y_all <- data.frame(mapply(append, y_all, std_y))

rownames(x_all)[10300] <- "Mean values"
rownames(x_all)[10301] <- "St. dev."

rownames(y_all)[10300] <- "Mean values"
rownames(y_all)[10301] <- "St. dev"

