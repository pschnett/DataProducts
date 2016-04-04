###Problem Statement
#### Using Machine Learning Workflow to process and transform US Department of 
#### Transportation data to create a prediction Model.  THis model
#### must predict whether a flight would arrive 15+ minutes after
#### the scheduled arrival time with 70+% accuracy

### Flight Delays Data
# Load the data into a data frame with columns and rows
origData <- read.csv2('https://pschnettnorthcentralus.blob.core.windows.net/downloads/FlightDelayDOT.csv', sep=",", header = TRUE, stringsAsFactors = FALSE)

# That is a lot of rows to process so to speed thing up let's 
#restrict data to only flight between certain large airports
airports <- c('ATL', 'LAX', 'ORD', 'DFW', 'JFK', 'SFO', 'CLT', 'LAS', 'PHX')
origData <- subset(origData, DEST %in% airports & ORIGIN %in% airports)

###Project Columns
# Yes, it definitely appears that the column X has no value so lets remove this column.
# The column can be removed by setting it's value to NULL
origData$X <- NULL

# In looking at the data I see the possible correlations between ORIGIN_AIRPORT_SEQ_ID and ORIGIN_AIRPORT_ID
# and between DEST_AIRPORT_SEQ_ID and DEST_AIRPORT_ID.  I am not sure we will use these fields,
# but if they are correlated we need only one of each pair
#
#  Let's Check the values using corrilation function, cor().  Closer to 1 =>  more correlated
cor(origData[c("ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_AIRPORT_ID")])
# Wow.  A perfect 1.  So ORIGIN_AIRPORT_SEQ_ID and ORIGIN_AIRPORT_ID are moving in lock step.
# Let's check DEST_AIRPORT_SEQ_ID, DEST_AIRPORT_ID
cor(origData[c("DEST_AIRPORT_SEQ_ID", "DEST_AIRPORT_ID")])
# Another perfect 1.  So DEST_AIRPORT_SEQ_ID and DEST_AIRPORT_SEQ_ID are also moving in lock step.
#
# Let's drop the columns ORIGIN_AIRPORT_SEQ_ID and DEST_AIRPORT_SEQ_ID since they are not providing
# any new data
origData$ORIGIN_AIRPORT_SEQ_ID <- NULL
origData$DEST_AIRPORT_SEQ_ID <- NULL

# UNIQUE_CARRIER and CARRIER also look related, actually they look like identical
# We can see if the are identical by filtering the rows to those we they are different.
# R makes this easy, no loops to write.  All iteration is done for us.
mismatched <- origData[origData$CARRIER != origData$UNIQUE_CARRIER,]
nrow(mismatched)
# 0 mismatched, so UNIQUE_CARRIER and CARRIER identical.  So let's rid of the UNIQUE_CARRIER column
origData$UNIQUE_CARRIER <- NULL

### Clean Missing Data
#Column Selected ARR_DEL15
#Minimum missing value = 0
#Maximum missing value = 1
#Cleaning Mode = Remove entire row
onTimeData <- origData[!is.na(origData$ARR_DEL15) & origData$ARR_DEL15!="" & !is.na(origData$DEP_DEL15) & origData$DEP_DEL15!="",]

# Changing the format of a column and all of the data for the row in that column is hard in some languages 
# but simple in R.  
# We just type in a simple command
onTimeData$DISTANCE <- as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED <- as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED <- as.integer(onTimeData$DIVERTED)

#   Let's take the Arrival departure and delay fields.  Sometime algorithm perform better 
# when you change the fields into factors which are like enumeration values in other languages
# This allows the algorithm to use count of when a value is a discrete value. 
###Metadata Editor
onTimeData$ARR_DEL15 <- as.factor(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15 <-as.factor(onTimeData$DEP_DEL15)
onTimeData$DEST_AIRPORT_ID <- as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID <- as.factor(onTimeData$ORIGIN_AIRPORT_ID)
onTimeData$DAY_OF_WEEK <- as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$DEST <- as.factor(onTimeData$DEST)
onTimeData$ORIGIN <- as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK <- as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$CARRIER <- as.factor(onTimeData$CARRIER)

#----------------------------------------------------------------
# Training the Algorithm
#----------------------------------------------------------------
# Download and install caret locally
#install.packages('caret')

# Load caret
library(caret)

# Set random number seed for reproducability
set.seed(122515)

# set the columns we are going to use to train algorithm
featureCols <- c("ARR_DEL15", "DAY_OF_WEEK", "CARRIER", "DEST","ORIGIN","DEP_TIME_BLK")

# created filtered version of onTimeData dataframe
onTimeDataFiltered <- onTimeData[,featureCols]
# create vector contain row indicies to put into the training data frames
inTrainRows <- createDataPartition(onTimeDataFiltered$ARR_DEL15, p=0.70, list=FALSE)

# Create the training data frame
trainDataFiltered <- onTimeDataFiltered[inTrainRows,]
# Create the testing data frame.  Notice the prefix "-" 
testDataFiltered <- onTimeDataFiltered[-inTrainRows,]

# Check split 
#   Should be 70%

nrow(trainDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))
#   Should be 30%
nrow(testDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))


# Create a train prediction model
#install.packages('e1071', dependencies=TRUE)

#  Logistic Regression
logisticRegModel <- train(ARR_DEL15 ~ ., data=trainDataFiltered, method="glm", family="binomial",
                          trControl=trainControl(method="cv", number=10, repeats=10))

save(logisticRegModel, file = "flightDelayLogisticRegModel.RData")

load("flightDelayLogisticRegModel.RData")

#   Predict using trained model against test data

#   Logistic Regression
logRegPrediction <- predict(logisticRegModel, trainDataFiltered)



#    Get detailed statistics of prediction versus actual via Confusion Matrix 
logRegConfMat <- confusionMatrix(logRegPrediction, testDataFiltered[,"ARR_DEL15"])
logRegConfMat

testPred <- predict(logisticRegModel, testDataFiltered[-1,])
head(testPred)

prediction <- function(DAY_OF_WEEK, CARRIER, DEST, ORIGIN, DEP_TIME_BLK) {
  predDF <- data.frame(cbind(DAY_OF_WEEK, CARRIER, DEST, ORIGIN, DEP_TIME_BLK))
  intPred <- predict(logisticRegModel, predDF)
  if (intPred == '1.00') {
    "DELAY OVER 15 MINS"
  }
  else {
    "ON TIME"
  }
}

prediction("7", "AA", "JFK", "ORD", "1900-1959")
prediction("7", "AA", "JFK", "ORD", "1200-1259")
# Improving performance

# We use the Random Forest algorithm which creates multiple decision trees and uses bagging 
# to improve performance

#  install the package - this only needs to be done once.  After the package is installed
#  comment out this line unless you really want the latest version of the package to be downloaded
#  and installed
#install.packages('randomForest')

#  load the random forest library into the current session
library(randomForest)

# This code will run for a while!  It ran for 8 minutes on a system with a i7-4790K, 16 GB of memory, and a 500 GB SSD.
rfModel <- randomForest(trainDataFiltered[-1], trainDataFiltered$ARR_DEL15, proximity = TRUE, importance = TRUE)
rfModel

#   Random Forest
rfValidation <- predict(rfModel, testDataFiltered)
#    Get detailed statistics of prediction versus actual via Confusion Matrix 
rfConfMat <- confusionMatrix(rfValidation, testDataFiltered[,"ARR_DEL15"])
rfConfMat