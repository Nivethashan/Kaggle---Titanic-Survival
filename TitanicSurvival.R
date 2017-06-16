trainData <- read.csv("F:/finalproject/kaggle/train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("F:/finalproject/kaggle/test.csv", header = TRUE, stringsAsFactors = FALSE)
trainData = trainData[-c(1,9:12)]
trainData$Sex = gsub("female", 1, trainData$Sex)
trainData$Sex = gsub("male", 0, trainData$Sex)
master_vector = grep("Master.",trainData$Name, fixed=TRUE)
miss_vector = grep("Miss.", trainData$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", trainData$Name, fixed=TRUE)
mr_vector = grep("Mr.", trainData$Name, fixed=TRUE)
dr_vector = grep("Dr.", trainData$Name, fixed=TRUE)
for(i in master_vector) {
  trainData$Name[i] = "Master"
}
for(i in miss_vector) {
  trainData$Name[i] = "Miss"
}
for(i in mrs_vector) {
  trainData$Name[i] = "Mrs"
}
for(i in mr_vector) {
  trainData$Name[i] = "Mr"
}
for(i in dr_vector) {
  trainData$Name[i] = "Dr"
}
master_age = round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age = round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(trainData)) {
  if (is.na(trainData[i,5])) {
    if (trainData$Name[i] == "Master") {
      trainData$Age[i] = master_age
    } else if (trainData$Name[i] == "Miss") {
      trainData$Age[i] = miss_age
    } else if (trainData$Name[i] == "Mrs") {
      trainData$Age[i] = mrs_age
    } else if (trainData$Name[i] == "Mr") {
      trainData$Age[i] = mr_age
    } else if (trainData$Name[i] == "Dr") {
      trainData$Age[i] = dr_age
    } else {
      print("Uncaught Title")
    }
  }
}
trainData["Child"]
for (i in 1:nrow(trainData)) {
  if (trainData$Age[i] <= 12) {
    trainData$Child[i] = 1
  } else {
    trainData$Child[i] = 2
  }
}
trainData["Mother"] 
for(i in 1:nrow(trainData)) {
  if(trainData$Name[i] == "Mrs" & trainData$Parch[i] > 0) {
    trainData$Mother[i] = 1
  } else {
    trainData$Mother[i] = 2
  }
}
PassengerId = testData[1]
print(PassengerId)
testData = testData[-c(1, 8:11)]

testData$Sex = gsub("female", 1, testData$Sex)
testData$Sex = gsub("^male", 0, testData$Sex)

test_master_vector = grep("Master.",testData$Name)
test_miss_vector = grep("Miss.", testData$Name)
test_mrs_vector = grep("Mrs.", testData$Name)
test_mr_vector = grep("Mr.", testData$Name)
test_dr_vector = grep("Dr.", testData$Name)

for(i in test_master_vector) {
  testData[i, 2] = "Master"
}
for(i in test_miss_vector) {
  testData[i, 2] = "Miss"
}
for(i in test_mrs_vector) {
  testData[i, 2] = "Mrs"
}
for(i in test_mr_vector) {
  testData[i, 2] = "Mr"
}
for(i in test_dr_vector) {
  testData[i, 2] = "Dr"
}

test_master_age = round(mean(testData$Age[testData$Name == "Master"], na.rm = TRUE), digits = 2)
test_miss_age = round(mean(testData$Age[testData$Name == "Miss"], na.rm = TRUE), digits =2)
test_mrs_age = round(mean(testData$Age[testData$Name == "Mrs"], na.rm = TRUE), digits = 2)
test_mr_age = round(mean(testData$Age[testData$Name == "Mr"], na.rm = TRUE), digits = 2)
test_dr_age = round(mean(testData$Age[testData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(testData)) {
  if (is.na(testData[i,4])) {
    if (testData[i, 2] == "Master") {
      testData[i, 4] = test_master_age
    } else if (testData[i, 2] == "Miss") {
      testData[i, 4] = test_miss_age
    } else if (testData[i, 2] == "Mrs") {
      testData[i, 4] = test_mrs_age
    } else if (testData[i, 2] == "Mr") {
      testData[i, 4] = test_mr_age
    } else if (testData[i, 2] == "Dr") {
      testData[i, 4] = test_dr_age
    } else {
      print(paste("Uncaught title at: ", i, sep=""))
      print(paste("The title unrecognized was: ", testData[i,2], sep=""))
    }
  }
}

testData[89, 4] = test_miss_age 





testData["Child"]
for (i in 1:nrow(testData)) {
  if (testData$Age[i] <= 12) {
    testData$Child[i] = 1
  } else {
    testData$Child[i] = 2
  }
}
testData["Mother"] 
for(i in 1:nrow(testData)) {
  if(testData$Name[i] == "Mrs" & testData$Parch[i] > 0) {
    testData$Mother[i] = 1
  } else {
    testData$Mother[i] = 2
  }
}

testData$Mother
library(randomForest)
fit <- randomForest(as.factor(Survived)~ Pclass + Name + Sex + Age + SibSp + Parch +Child + Mother,data=trainData, importance=TRUE, ntree=2000,sampsize=5,replace=TRUE,na.action=TRUE)
print(fit)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, testData)
submit <- data.frame(pid=PassengerId, Survived = Prediction)
write.csv(submit, file = "F:/finalproject/kaggle/firstforest.csv", row.names = FALSE)
