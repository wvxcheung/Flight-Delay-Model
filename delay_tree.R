# Load the rpart package
library("rpart")
# Load the rpart.plot package
library("rpart.plot")

# Reading csv file
data <- read.csv("flight-data.csv")
# Drop tail number columm as it causes issues as test data does not have tail included in original tree
data <- data[ , -c(11)]

#Part A
# Separating data 80/20 split
a <- nrow(data)
b <- a * 0.80
sample_rows <- sample(a, b)
# Setting test and train data
train <- data[sample_rows,]
test <- data[-sample_rows,]

#Part B
# Tree with all variables set to default
flight_tree <- rpart(delay ~ ., data = train, method = "class")
rpart.plot(flight_tree)
# Run predict
predict_flight <- predict(flight_tree, test, type = "class")
# Confusion matrix to check accuracy
table(predict_flight, test$delay)
mean(predict_flight == test$delay)
# 87.98186% accuracy on initial run

#Part C
# Tree with 5 variables chosen upon initial inspection, default parameters
flight_tree <- rpart(delay ~ dayweek + daymonth + weather + carrier + distance, data = train, method = "class")
rpart.plot(flight_tree)
# Run predict
predict_flight <- predict(flight_tree, test, type = "class")
# Confusion matrix to check accuracy
table(predict_flight, test$delay)
mean(predict_flight == test$delay)
# 83.21995% accuracy on initial run

#Part D
# Tree with 5 variables I felt best, with paramaters to increase accuracy
flight_tree <- rpart(delay ~ dayweek + deptime + weather + dest + schedtime, data = train, cp = 0, minsplit = 10, method = "class")
rpart.plot(flight_tree)
# Run predict
predict_flight <- predict(flight_tree, test, type = "class")
# Confusion matrix to check accuracy
table(predict_flight, test$delay)
mean(predict_flight == test$delay)
# getting best accuracy out of all models
# 89.56916% accuracy on initial run