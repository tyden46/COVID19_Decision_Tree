library(caret)
library(dslabs)
setwd("C://Users//tyden46//Documents//")
data=read.csv("CovidSurvivalData.csv")
data$outcome=factor(data$outcome)
data$age=as.numeric(data$age)
data$sex=factor(data$sex)
data$country=factor(data$country)
data$chronic_disease_binary=factor(data$chronic_disease_binary)

test_index <- createDataPartition(data$outcome, times = 1, p = 0.2, list = FALSE)    # create a 20% test set
testSet <- data[test_index,]
trainingSet <- data[-test_index,]

library(rpart.plot)
set.seed(2)
rpart_fit=train(outcome ~ ., data=trainingSet,
                method = 'rpart',
                tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
prediction=predict(object=rpart_fit, newdata=testSet)
plot(rpart_fit)
confusionMatrix(prediction, reference = testSet$outcome)$overall["Accuracy"]
par(mar = c(1, 1, 1, 1))
plot(rpart_fit$finalModel)
text(rpart_fit$finalModel, cex=0.6)
