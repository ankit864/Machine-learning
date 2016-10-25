###Decision Tree Basics: Regression

require(knitr)
library(tree)
library(ISLR)
attach(Hitters)
library(rpart.plot)

# Remove NA data
Hitters<- na.omit(Hitters)
# log transform Salary to make it a bit more normally distributed
hist(Hitters$Salary) #Plot1
Hitters$Salary <- log(Hitters$Salary)
hist(Hitters$Salary) #Plot2
tree.fit <- tree(Salary~Hits+Years, data=Hitters)
summary(tree.fit)
plot(tree.fit) #Plot3
###################################################################
###################################################################
###################################################################
#Tree Pruning
library(caret)
split <- createDataPartition(y=Hitters$Salary, p=0.5, list=FALSE)

train <- Hitters[split,]
test <- Hitters[-split,]

#Create tree model
trees <- tree(Salary~., train)
plot(trees) #Plot4
text(trees, pretty=0) #Plot5
#Cross validate to see whether pruning the tree will improve performance
cv.trees <- cv.tree(trees)
plot(cv.trees) #Plot6
prune.trees <- prune.tree(trees, best=4)
plot(prune.trees)
text(prune.trees, pretty=0) #Plot7
yhat <- predict(prune.trees, test)
plot(yhat, test$Salary) #Plot8
abline(0,1) #Plot9
mean((yhat - test$Salary)^2)
Heart <-read.csv('C:\\Users\\user\\Documents\\R\\MyRScripts\\DecisionTrees\\Heart.csv')
kable(head(Heart))
dim(Heart)
split <- createDataPartition(y=Heart$AHD, p = 0.5, list=FALSE)
train <- Heart[split,]
test <- Heart[-split,]

trees <- tree(AHD ~., train)
plot(trees) #Plot10

cv.trees <- cv.tree(trees, FUN=prune.misclass)
plot(cv.trees) #Plot11

cv.trees

prune.trees <- prune.misclass(trees, best=4)
plot(prune.trees)
text(prune.trees, pretty=0) #Plot12

tree.pred <- predict(prune.trees, test, type='class')
confusionMatrix(tree.pred, test$AHD)

attach(Carseats)
kable(head(Carseats))

Carseats$Sales <- ifelse(Sales <= median(Sales), 'Low', 'High')
Carseats$Sales <- factor(Carseats$Sales)

Carseats<-na.omit(Carseats)
#Split data into train / validation
set.seed(111)
split <- createDataPartition(y=Carseats$Sales, p=0.6, list=FALSE)
train <- Carseats[split,]
test <- Carseats[-split,]

sales.tree <- tree(Sales ~., data=train)
summary(sales.tree)

plot(sales.tree)
text(sales.tree, pretty=0) #Plot13

sales.pred <- predict(sales.tree, test, type='class')
confusionMatrix(sales.pred, test$Sales)

set.seed(12)
cv.sales.tree <- cv.tree(sales.tree, FUN=prune.misclass)
plot(cv.sales.tree) #Plot14

prune.sales.tree <- prune.misclass(sales.tree, best=4)
prune.pred <- predict(prune.sales.tree, test, type='class')

plot(prune.sales.tree)
text(prune.sales.tree, pretty=0) #Plot15
