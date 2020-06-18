train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\WineData.csv ")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\WineHoldoutData.csv ')



sum(is.na(train))
sum(is.na(test))
library(Amelia)  
missmap(train, main="Train Data - Missings Map",
        col=c("yellow", "black"), legend=FALSE)       
missmap(test, main="Test Data - Missings Map",
        col=c("yellow", "black"), legend=FALSE)


boxplot(train)
boxplot(test)

OutVals = boxplot(train, plot=FALSE)$out
OutVals1 = boxplot(test, plot=FALSE)$out
plot(OutVals1)
plot(OutVals)
boxplot(train)
y = c(1,2,3,4,5,6,7,9,10,11)
for (i in y)
{
  x <- train[,i]
  qnt <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  train[,i] = x
}
for (i in y)
{
  x <- test[,i]
  qnt <- quantile(x, probs=c(.25, .75))
  caps <- quantile(x, probs=c(.05, .95))
  H <- 1.5 * IQR(x)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  test[,i] = x
}


train= train[!duplicated(train),]
test = test[!duplicated(test),]

train1 = train[,-c(3)]
test1=  test[,-c(3)]

library(randomForest)
rf = randomForest(quality~.,data= train1, mtry= 3.60 , n.trees= 500 )
summary(rf)
pred = predict(rf, newdata = test1)
mean((pred - test1$quality)^2)
importance(rf)
varImpPlot(rf)
library(e1071)
library(MASS)
lr = glm(quality~., data =train)
summary(lr)
svm = svm(quality~., data=train1, kernel='linear', cost= 10)
summary(svm)
pred1 = predict(svm, newdata = test1)
mean((pred1 - test1$quality)^2)

svm1 = svm(quality~., data=train1, kernel='polynomial', cost= 10, degree = 2)
summary(svm1)
pred2 = predict(svm1, newdata = test1)
mean((pred2 - test1$quality)^2)

svm2 = svm(quality~., data=train1, kernel='radial', cost= 10, gamma= 2)
summary(svm2)
pred3 = predict(svm2, newdata = test1)
mean((pred3 - test1$quality)^2)

library(gbm)
boosting = gbm(quality~., data=train1,distribution="gaussian",n.trees=100 , interaction.depth=5)
summary(boosting)
pred4 = predict(boosting, newdata = test1,n.trees= 100)
mean((pred4 - test1$quality)^2)


tune= tune(randomForest,quality~.,data= train1,  ranges =list(mtry=c(3,5,6),n.trees= c(500,100,400)))
summary(tune)
pred_tune = predict(tune$best.model, newdata = test1)
mean((pred_tune - test1$quality)^2)


tune1= tune(svm, quality~.,data= train1,kernel ='linear' , range = list(cost= c(3,4,5,6,7,8,9)))
summary(tune1)
pred_tune1 = predict(tune1$best.model, newdata = test1)
mean((pred_tune1 - test1$quality)^2)

tune2= tune(svm,quality~.,data= train1,kernel= 'polynomial', range = list(cost= c(3,4,5),degree= c(2,3,4)))
summary(tune2)
pred_tune2 = predict(tune2$best.model, newdata = test1)
mean((pred_tune2 - test1$quality)^2)

tune3= tune(svm,quality~.,data= train1,kernel= 'radial', range = list(cost= c(3,4,5),gamma= c(10,0.01,0.1)))
summary(tune3)
pred_tune3 = predict(tune3$best.model, newdata = test1)
mean((pred_tune3 - test1$quality)^2)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
train2 = train1[!(train1$quality==9),]
test2 = test1
train2$quality = as.factor(train2$quality)
test2$quality= as.factor(test2$quality)
fix(train2)
levels(train2$quality)
rf_c = randomForest(quality~.,data= train2, mtry= 3.60 , n.trees= 500 )
summary(rf_c)
pred_c = predict(rf_c, newdata = test2)
pred_c
mean(pred_c != test2$quality)

svm_c = svm(quality~., data=train2, kernel='linear', cost= 10)
summary(svm_c)
pred1_c = predict(svm_c, newdata = test2)
mean((pred1_c != test2$quality)^2)

svm1_c = svm(quality~., data=train2, kernel='polynomial', cost= 10, degree = 2)
summary(svm1_c)
pred2_c = predict(svm1_c, newdata = test2)
mean((pred2_c != test2$quality)^2)

svm2_c = svm(quality~., data=train2, kernel='radial', cost= 10, gamma= 2)
summary(svm2_c)
pred3_c = predict(svm2_c, newdata = test2)
mean((pred3_c != test2$quality)^2)

tune_c= tune(randomForest,quality~.,data= train2, range = list(mtry= c(3.6,4,5),n.trees = c(100,300,400)))
summary(tune_c)
pred_tune_c = predict(tune_c$best.model, newdata = test2)
mean((pred_tune_c != test2$quality)^2)

tune1_c= tune(svm, quality~.,data= train2,kernel ='linear' , range = list(cost= c(3,4,5,6,7,8,9)))
summary(tune1_c)
pred_tune1_c = predict(tune1_c$best.model, newdata = test2)
mean((pred_tune1_c != test2$quality)^2)

tune2_c= tune(svm,quality~.,data= train2,kernel= 'polynomial', range = list(cost= c(3,4,5),degree= c(2,3,4,5)))
summary(tune2_c)
pred_tune2_c = predict(tune2_c$best.model, newdata = test2)
mean((pred_tune2_c != test2$quality)^2)

tune3_c= tune(svm,quality~.,data= train2,kernel= 'radial', range = list(cost= c(3,4,5),gamma= c(2,10,0.01,0.1)))
summary(tune3_c)
pred_tune3_c = predict(tune3_c$best.model, newdata = test2)
mean((pred_tune3_c != test2$quality)^2)

train3 = train1[!(train1$quality== 7),]
test3 = test1[!(test1$quality== 7),]
train3 = train3[!(train3$quality== 8),]
test3 = test3[!(test3$quality== 8),]
train3 = train3[!(train3$quality== 5),]
test3 = test3[!(test3$quality== 5),]
train3 = train3[!(train3$quality== 5),]
test3 = test3[!(test3$quality== 5),]
train3 = train3[!(train3$quality== 6),]
test3 = test3[!(test3$quality== 6),]
train3 = train3[!(train3$quality== 9),]
test3$quality = ifelse(test3$quality == 4, 1 , 0)
train3$quality = ifelse(train3$quality == 4, 1 , 0)
levels(test3$quality)
boosting_c = gbm(quality~., data=train3, distribution="bernoulli", n.trees=100 , interaction.depth=5)
summary(boosting_c)
pred4_c = predict(boosting_c, newdata = test3,n.tree= 100,type= 'response')
pred4_c = ifelse(pred4_c > 0.5,1,0)
mean(pred4_c != test3$quality)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#greater than 7
#tuning boosting 

train4 = train1
test4 = test1
train4$quality  =  ifelse(train4$quality>6,1,0)
test4$quality  =  ifelse(test4$quality>6,1,0)
boosting_c1 = gbm(quality~., data=train4, distribution="bernoulli", n.trees=100 , interaction.depth=5)
summary(boosting_c1)
pred4_c1 = predict(boosting_c1, newdata = test4,n.tree= 100,type= 'response')
pred4_c1 = ifelse(pred4_c1 > 0.5,1,0)
mean(pred4_c1 != test4$quality)

train4$quality = as.factor(train4$quality)
test4$quality= as.factor(test4$quality)


rf_c1 = randomForest(quality~.,data= train4, mtry= 3.60 , n.trees= 500 )
summary(rf_c1)
pred_c1 = predict(rf_c1, newdata = test4)
pred_c1
mean(pred_c1 != test4$quality)

svm_c1 = svm(quality~., data=train4, kernel='linear', cost= 10)
summary(svm_c1)
pred1_c1 = predict(svm_c1, newdata = test4)
mean((pred1_c1 != test4$quality)^2)

svm1_c1 = svm(quality~., data=train4, kernel='polynomial', cost= 10, degree = 2)
summary(svm1_c1)
pred2_c1 = predict(svm1_c1, newdata = test4)
mean((pred2_c1 != test4$quality)^2)

svm2_c1 = svm(quality~., data=train4, kernel='radial', cost= 10, gamma= 2)
summary(svm2_c1)
pred3_c1 = predict(svm2_c1, newdata = test4)
mean((pred3_c1 != test4$quality)^2)

tune_c1= tune(randomForest,quality~.,data= train4, range = list(mtry= c(3.6,4,5),n.trees = c(100,300,400)))
summary(tune_c1)
pred_tune_c1 = predict(tune_c1$best.model, newdata = test4)
mean((pred_tune_c1 != test4$quality)^2)

tune1_c1= tune(svm, quality~.,data= train4,kernel ='linear' , range = list(cost= c(3,4,5,6,7,8,9)))
summary(tune1_c1)
pred_tune1_c1 = predict(tune1_c1$best.model, newdata = test4)
mean((pred_tune1_c1 != test4$quality)^2)

tune2_c1= tune(svm,quality~.,data= train4,kernel= 'polynomial', range = list(cost= c(3,4,5),degree= c(2,3,4,5)))
summary(tune2_c1)
pred_tune2_c1 = predict(tune2_c1$best.model, newdata = test4)
mean((pred_tune2_c1 != test4$quality)^2)

tune3_c1= tune(svm,quality~.,data= train4,kernel= 'radial', range = list(cost= c(3,4,5),gamma= c(2,10,0.01,0.1)))
summary(tune3_c1)
pred_tune3_c1 = predict(tune3_c1$best.model, newdata = test4)
mean((pred_tune3_c1 != test4$quality)^2)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
train5 = train1
test5 = test1
sum(train5$quality==9)
train5$quality[train5$quality == 9]= 7 

train5$quality = as.factor(train5$quality)
test5$quality= as.factor(test5$quality)
fix(train2)
levels(train5$quality)
rf_c_r = randomForest(quality~.,data= train2, mtry= 3.60 , n.trees= 500 )
summary(rf_c_r)
pred_c_r = predict(rf_c_r, newdata = test2)
pred_c_r
mean(pred_c_r != test2$quality)

svm_c_r = svm(quality~., data=train2, kernel='linear', cost= 10)
summary(svm_c_r)
pred1_c_r = predict(svm_c_r, newdata = test2)
mean((pred1_cR != test2$quality)^2)

svm1_c_r = svm(quality~., data=train2, kernel='polynomial', cost= 10, degree = 2)
summary(svm1_c_r)
pred2_c_r = predict(svm1_c_r, newdata = test2)
mean((pred2_c_r != test2$quality)^2)

svm2_c_r = svm(quality~., data=train2, kernel='radial', cost= 10, gamma= 2)
summary(svm2_c_r)
pred3_c_r = predict(svm2_c_r, newdata = test2)
mean((pred3_c_r != test2$quality)^2)

tune_c_r= tune(randomForest,quality~.,data= train2, range = list(mtry= c(3.6,4,5),n.trees = c(100,300,400)))
summary(tune_c_r)
pred_tune_c_r = predict(tune_c_r$best.model, newdata = test2)
mean((pred_tune_c_r != test2$quality)^2)

tune1_c_r= tune(svm, quality~.,data= train2,kernel ='linear' , range = list(cost= c(3,4,5,6,7,8,9)))
summary(tune1_c_r)
pred_tune1_c_r = predict(tune1_c_r$best.model, newdata = test2)
mean((pred_tune1_c_r != test2$quality)^2)

tune2_c_r= tune(svm,quality~.,data= train2,kernel= 'polynomial', range = list(cost= c(3,4,5),degree= c(2,3,4,5)))
summary(tune2_c_r)
pred_tune2_c_r = predict(tune2_c_r$best.model, newdata = test2)
mean((pred_tune2_c_r != test2$quality)^2)

tune3_c_r= tune(svm,quality~.,data= train2,kernel= 'radial', range = list(cost= c(3,4,5),gamma= c(2,10,0.01,0.1)))
summary(tune3_c_r)
pred_tune3_c_r = predict(tune3_c_r$best.model, newdata = test2)
mean((pred_tune3_c_r != test2$quality)^2)
#--------------------------------------------------------------------------------------------------------------------------------------
