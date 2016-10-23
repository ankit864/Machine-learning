library(randomForest)
library(MASS)
library(knitr)
library(caret)
library(gbm)
kable(head(Boston))
set.seed(123)
split <- createDataPartition(y=Boston$medv, p = 0.7, list=FALSE)
train <- Boston[split,]
test<- Boston[-split,]

rf <- randomForest(medv~., data=train, mtry=6, importance = TRUE)
yhat <- predict(rf, test)

mean((yhat - test$medv)^2)
kable(importance(rf))
varImpPlot(rf)
rf.caret <- train(medv ~., train,
                  preProc=c('center', 'scale'),
                  method='rf',
                  importance=TRUE)

rf.caret

boost <- gbm(medv~. , data=train, 
             distribution = 'gaussian', 
             n.trees = 5000, 
             interaction.depth = 4)

summary(boost)

par(mfrow=c(1,2))
plot(boost, i='rm')
plot(boost, i='lstat')

boost.pred <- predict (boost, test, n.trees=5000)
mean((boost.pred - test$medv)^2)

ctr <- trainControl(method = "cv", number = 10)

boost.caret <- train(medv~., train,
                     method='bstTree',
                     preProc=c('center','scale'),
                     trControl=ctr)
boost.caret
plot(boost.caret)

boost.caret.pred <- predict(boost.caret, test)
mean((boost.caret.pred - test$medv)^2)
