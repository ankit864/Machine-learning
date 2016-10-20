#Data import
Knn<-read.csv("C:\\Users\\user\\Documents\\R\\MyRScripts\\KNN\\iris.data")
names(Knn) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
####################################################
#Data Visualisation
library(ggvis)
Knn %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
Knn %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()
###################################################
#Data pre-processing
set.seed(1234)
ind <- sample(2, nrow(Knn), replace=TRUE, prob=c(0.67, 0.33))
Knn.training <- Knn[ind==1, 1:4]
Knn.test <- Knn[ind==2, 1:4]
Knn.trainLabels <- Knn[ind==1, 5]
Knn.testLabels <- Knn[ind==2, 5]
##################################################
#Building Knn classifier
Knn_pred <- knn(train = Knn.training, test = Knn.test, cl = Knn.trainLabels, k=3)
Knn_pred
#####################################################
# Evaluation
library(gmodels)
#make a cross tabulation or a contingency table. This type of table is 
#often used to understand the relationship between two variables. 
#In this case, you want to understand how the classes of your test data, 
#stored in Knn.testLabels relate to your model that is stored in Knn_pred
CrossTable(x = Knn.testLabels, y = Knn_pred, prop.chisq=FALSE)

#From this table, you can derive the number of correct and incorrect predictions: 
#one instance from the testing set was labeled Versicolor by the model, 
#while it was actually a flower of species Virginica. You can see this in the 
#first row of the "Virginica" species in the iris.testLabels column. 
#In all other cases, correct predictions were made. You can conclude that 
#the model's performance is good enough and that you don't need to improve the 
#model!
