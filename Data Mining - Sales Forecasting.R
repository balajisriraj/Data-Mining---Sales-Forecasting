rm(list = ls())

#Import the training and test data set
training = read.table("training.txt", header = TRUE, stringsAsFactors = TRUE)
test = read.table("testStudent.txt", header = TRUE, stringsAsFactors = TRUE)

#Remove unwanted predictors from test and training dataset
training$articleNo <- NULL
training$barcode <- NULL
training$id <- NULL
training$version <- NULL
test$articleNo <- NULL
test$barcode <- NULL
test$id <- NULL
test$version <- NULL

#Add new column to identify the test and training data indexes once they are combined
training["IsTrain"] <- TRUE
test["IsTrain"] <- FALSE

#Combine the test and training data sets
combined <- rbind(training, test)

str(combined)

#Change the launch month into a numeric value as a numeric value is better for prediction
combined$launchMonth = match(combined$launchMonth,month.name)
str(combined$launchMonth)
combined$launchMonth = as.factor(combined$launchMonth)
str(combined$launchMonth)

#Split the data into training and test
training2 = combined[combined$IsTrain == TRUE,]
test2 = combined[combined$IsTrain == FALSE,]

#Remove the extra column which was inserted initially
training2$IsTrain = NULL
test2$IsTrain = NULL

#Take backup of training data
training3 = training2

#Data exploration through plotting using ggplot2
install.packages("ggplot2")
library(ggplot2)
ggplot(data=training3, aes(x=launchMonth, y=SalesQuantity, fill = launchMonth)) +
  geom_bar(stat="identity")+scale_fill_hue(l = 40) + ggtitle("Sales vs Launch Month")


#Since the response variable is left skewed, perform Log Transform
qplot(training2$SalesQuantity, xlab = "Sales Quantity", ylab = "Count", main = "Sales Distribution")
training2$SalesQuantity = log(training2$SalesQuantity)
summary(training2$SalesQuantity)
qplot(training2$SalesQuantity, xlab = "log(Sales Quantity)", ylab = "Count", main = "Log Sales Distribution")

#Build model using random forest in H2o package
install.packages("h2o")
library("h2o")
?h2o.randomForest

#Select the appropriate predictors and the response variable into vectors 
features = as.vector(colnames(training2))
predictors = features[-44]
response = features[44]

#Initialize the H2o package for running on system
?h2o.init
h2o.init(nthreads = -1)

#Convert the training data set into H2o format
trainingh2o = as.h2o(training2)

#Perform random forest and set seed
rf_model = h2o.randomForest(x = predictors, y = response, 
                            seed = 1234567, training_frame = trainingh2o)
summary(rf_model)

#Convert test data into H2o format
testh2o = as.h2o(test2)

#Predict the results using the model
prediction = h2o.predict(rf_model,testh2o)
summary(prediction)

#Perform anti log operation on the results
prediction = as.data.frame(exp(prediction))

#Perform floor operation as the data is left skewed
prediction = floor(prediction)
View(prediction)

#If the predicted value is 0 change it to 1
prediction[prediction$C1 == 0] = 1

#Read the test data again for ID's
testdata = read.table("testStudent.txt", header = TRUE, stringsAsFactors = TRUE)

#Store the ID's and predicted values in a data frame
result = data.frame("ID"=testdata$id, "Prediction"= prediction)
colnames(result) = c("ID", "Prediction")
View(result)

#Write the results into a .csv file
write.csv(result, file = "FinalSubmission.csv", row.names = FALSE)

#Read the actual results provided
actual = read.table("testInstructor.txt", header = T, stringsAsFactors = T)

#Plot of Actual values vs Predicated values
plot(actual$SalesQuantity, result$Prediction, xlab = "Actual Sales Quantity", 
     ylab = "Predicted Sales Quantity", main = "Predicted vs Actual Sales")

##Verification of MSE and MAPE by comparing predicted with actual results
MSE = function(actual, predicted){
  mean((actual - predicted)^2)
}

MAPE <- function(actual, predicted) {
  mean(abs(actual-predicted)/abs(actual))
}

#MSE
mserr = MSE(actual$SalesQuantity,result$Prediction)
mserr

#MAPE
maperr = MAPE(actual$SalesQuantity,result$Prediction)
maperr

#Stop H2o process
h2o.shutdown()

