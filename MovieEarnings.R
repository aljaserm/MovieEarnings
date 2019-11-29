movies <- read.csv("C:/Users/aljas/OneDrive/Documents/Development/R/MovieEarnings/MovieEarnings/Movie_regression.csv", header = TRUE)
# View(movies)

summary(movies)
movies$Time_taken[is.na(movies$Time_taken)] <- mean(movies$Time_taken, na.rm = TRUE)

# Test-Train Split
# install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(movies,SplitRatio = 0.8)
train = subset(movies,split == TRUE)
test = subset(movies,split == FALSE)

#install required packages
# install.packages('rpart')
# install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

#Run regression tree model on train set
regtree <- rpart(formula = Collection~., data = train, control = rpart.control(maxdepth = 3))
#press F1 on rpart for help on this function

#Plot the decision Tree
rpart.plot(regtree, box.palette="RdBu", digits = -3)

#Predict value at any point
test$pred <- predict(regtree, test, type = "vector")

MSE2 <- mean((test$pred - test$Collection)^2)

#Tree Pruning
fulltree <- rpart(formula = Collection~., data = train, control = rpart.control( cp = 0))
rpart.plot(fulltree, box.palette="RdBu", digits = -3)
printcp(fulltree)
plotcp(regtree)

mincp <- regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]

prunedtree <- prune(fulltree, cp = mincp)
rpart.plot(prunedtree, box.palette="RdBu", digits = -3)

test$fulltree <- predict(fulltree, test, type = "vector")
MSE2full <- mean((test$fulltree - test$Collection)^2)

test$pruned <- predict(prunedtree, test, type = "vector")
MSE2pruned <- mean((test$pruned - test$Collection)^2)

library (randomForest)
set.seed (1)
bagging =randomForest(Collection~Budget+Trailer_views, data = train ,mtry=2, importance =TRUE)
test$bagging <- predict(bagging, test)
MSE2bagging <- mean((test$bagging - test$Collection)^2)

#Random forest
# install.packages('randomForest')
library(randomForest)

randomfor <- randomForest(Collection~., data = train,ntree=500)
#Predict Output 
test$random <- predict(randomfor, test)
MSE2random <- mean((test$random - test$Collection)^2)

#Boosting
# install.packages('gbm')
library (gbm)
set.seed (0)
boosting = gbm(Collection~., data = train, distribution="gaussian",n.trees =5000 , interaction.depth =4, shrinkage =0.2,verbose =F)
#distribution = 'Gaussian' for regression and 'Bernoulli' for classification
test$boost = predict (boosting, test, n.trees =5000)
MSE2boost <- mean((test$boost - test$Collection)^2)



