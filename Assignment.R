#Read the Raw data and construct an R data.frame
setwd('E:/Humboldt2017/Data Science/Excercise/Assignment')
source("func_set.R")
T.data <- clean.raw()





#11.18 工作日志
#1.模型变量方面
#尝试改变输入变量,发现对预测精确度提升仍然很小
#目前尝试了改变参数和改变输入变量,对模型的修正能力都很弱
#注意到title和地区没有进入分类,不符合直觉(模型问题 or 聚类问题)
#缺乏一个更好的数据微调系统
#item size感觉有更好的处理方式,还有一些信息没有提取出来
#应该先按尺码类型分出产品类型,然后按大中小统一分类
#对item_color的分类非常无力,考虑用数值代替分类--看一看文献

#2.模型选择方面
#随机森林方法仍无法有效提升预测精度
#可能是树方法本身限制了有效性
#明天尝试一下朴素贝叶斯

#11.19 工作日志
#1.模型变量方面
#继续对item_size进行处理
#完成对item_size以及class的分类处理,但不完美,考虑自动分类措施
#新变量代入树方法中,仍无明显改善


#11.20 工作日志
#尝试了多层感知器方法,但数据集中类型变量太多,能放进MLP的变量太少
#开始了函数库编辑工作

#11.21 工作日志
#暂时完成了函数库工作
#brand变量应该丢弃,任何监督学习得到的分类都会导致CV失效
#开始处理颜色数据

#当前工作的理想状态是构建一个具有时间延迟的神经网络，并且按地区坐标具有局部输入




#-------------------------------------------------------------------------------
#Prediction Part
c_data <- T.data$c_data

#I.Decision Tree
p_data <- subset(c_data,select = -c(brand_id,order_date,delivery_date,user_dob,user_reg_date,item_id,order_item_id,user_id))
p_data$class <- data.item_size$class
p_data$item_size <- data.item_size$size





p_data$brand <- data.brand$brand
summary(p_data)
str(p_data)
p_data$return <- as.factor(p_data$return)



library(caret)
set.seed(3035)
intrain <- createDataPartition(y = p_data$return, p= 0.7, list = FALSE)
training <- p_data[intrain,]
testing <- p_data[-intrain,]
summary(training)

#Training the tree
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(return ~class + item_size + item_price + age + user_title  + delivery_time, data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

#age
#delivery_time
prp(dtree_fit$finalModel)

fit <- predict(dtree_fit,testing)
fit
testing$return
M <- abs(as.numeric(testing$return)  - as.numeric(fit))
M.trans <- ifelse(M == 0,1,0)
sum(M.trans)/length(M.trans) #0.62% Prediction Accuracy

#rpart
library(rpart)
library(rpart.plot)
training1 <- subset(training,select = -c(item_color))
rctrl1 <- rpart.control(cp = 0.0005)
tree <- rpart(return~.,data = training1,control = rctrl1)
prp(tree)
tree.fit <- predict(tree,testing)
tree.fit.bio <- ifelse(tree.fit[,2] > 0.5,1,0)
tree.fit.wrong <- abs(as.numeric(testing$return) - 1 - tree.fit.bio)
tree.fit.right <- ifelse(tree.fit.wrong == 1,0,1)
sum(tree.fit.right)/length(tree.fit.right)
sum(abs(as.numeric(training$return) - 1 - tree.fit.bio))/length(testing$return)

tree.fit.t <- predict(tree,testing,class = "class")
tree.fit.t
testing$return
tree.fit.t.bio <- ifelse(tree.fit.t[,1] > 0.5,1,0)
sum(abs(as.numeric(testing$return) - 1 - tree.fit.t.bio))/length(testing$return)





#crossvalidation
library(cvTools)
set.seed(1234)
Folds <- createFolds(training$return,k=5)
mat <- 1:5
l.fold <- lapply(1:5,function(x){mat[mat != x]})
cvtree <- function(folds,cp,minsplit){
  rctrl1 <- rpart.control(minsplit = minsplit, cp = cp)
  tree <- rpart(return~.,data = training[unlist(Folds[folds]),],control = rctrl1)
  temp <- 1:5
  temp.t <- training[unlist(Folds[temp[temp %in% folds == FALSE]]),]
  fit <- predict(tree,temp.t,type = "class")
  M <- abs(as.numeric(temp.t$return)  - as.numeric(fit))
  M.trans <- ifelse(M == 0,1,0)
  return(sum(M.trans)/length(M.trans))
}
#debug(cvtree)


Cvalid.result <- function(mins.c){
  mins <- mins.c[1]
  c <- mins.c[2]
  CVali <- lapply(l.fold,cvtree,minsplit = mins,cp = c)
  CVali.avg <- mean(unlist(CVali))
  return(c(mins,c,CVali.avg))
}

CV.matirx.gen <- function(row1,row2){
  row.f <- rep(row1,each = length(row2))
  row.s <- rep(row2,length(row1))
  return(cbind(row.f,row.s))
}
Cvalid.matrix <- CV.matirx.gen(seq(10,35,by = 5),1:20/1000)

Cvalid.result <- apply(Cvalid.matrix,1,Cvalid.result)
#-------------------------------------------------------------------------
#II. Logit
lr <- glm(return ~brand + item_price+user_title+user_state+delivery_time+age+days_reg+na_delivery_date+na_age,training,family = binomial(link = "logit"))
summary(lr)
pred.lr <- predict(lr,training,type = "response")
pred.lr
y <- as.numeric(training$return) - 1
accuracy <- sum(abs(y - ifelse(pred.lr >= 0.5,0,1))) / length(y)
accuracy

pred.lr.t <- predict(lr,testing,type = "response")
y <- as.numeric(testing$return) - 1
accuracy <- sum(abs(y - ifelse(pred.lr.t >= 0.5,0,1))) / length(y)
accuracy


library(glmnet)
Y <- training$return


lr.fac <- sapply(subset(training,select=-return),is.factor)
lr.fac.name <- names(lr.fac == TRUE)
lr.num.name <- names(lr.fac == FALSE)

X1.1 <- NULL
for(att in lr.fac.name){
  X1.1 <- cbind(X1,model.matrix(~training[,att]))
}
temp <- training[,lr.num.name]
X1.2 <- as.matrix(temp[,lr.num.name != "return"])
X <- cbind(X1.1,X1.2)
X

lr.net <- glmnet(X,Y,family = "binomial",alpha = 1, nlambda = 100, standardize = TRUE)
lr.net
predict(lr.net,newx = X1.1,type = "class",s = 0.007)
lr.cv <- cv.glmnet(X[,6:8],Y,family = "binomial",alpha = 1, nlambda = 100, standardize = TRUE)
predict(lr.cv, newx = x[1:5,], s = "lambda.min")




#-------------------------------------------------------------------------
library(tree)
tree.model <- tree(return~brand + item_price+user_title+user_state+delivery_time+age+days_reg+na_delivery_date+na_age ,training)
summary(training)
confusion=table(training$return,predict(tree.model, training, type="class"))
predict(tree.model, training, type="class")
accuracy=sum(diag(confusion)) *100/sum(confusion)
accuracy

#--------------------------------------------------------------------------
#Random Forest
library(randomForest)
RF <- randomForest(return~class + brand + item_price+user_title+user_state+delivery_time+days_reg+na_delivery_date +item_size, data = training,ntree = 100,importance = TRUE)
plot(RF)
RF.prd <- predict(RF,testing)
RF.prd
RF.prd.f <- abs(as.numeric(testing$return) - as.numeric(RF.prd)) -1
sum(abs(RF.prd.f)/length(testing$return))


training
c_data$item_size


#--------------------------------------------------------------------------
#Neural Network
library(RSNNS)
iris <- iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)]
summary(iris)
head(iris)
iris$inputsTrain
irisValues <- iris[,1:4]
irisTargets <- decodeClassLabels(iris[,5])
iris <- splitForTrainingAndTest(irisValues, irisTargets, ratio=0.15)
iris <- normTrainingAndTestSet(iris)

summary(training)
model <- mlp(iris$inputsTrain, iris$targetsTrain, size=5, learnFuncParams=c(0.1),
             maxit=50, inputsTest=iris$inputsTest, targetsTest=iris$targetsTest)


summary(model)
model
weightMatrix(model)
extractNetInfo(model)

par(mfrow=c(2,2))
plotIterativeError(model)

predictions <- predict(model,iris$inputsTest)

plotRegressionError(predictions[,2], iris$targetsTest[,2])

confusionMatrix(iris$targetsTrain,fitted.values(model))
confusionMatrix(iris$targetsTest,predictions)
plotROC(fitted.values(model)[,2], iris$targetsTrain[,2])
plotROC(predictions[,2], iris$targetsTest[,2])

confusionMatrix(iris$targetsTrain, encodeClassLabels(fitted.values(model),
                                                     method="402040", l=0.4, h=0.6))


#My MLP
summary(training)
library(dummies)
d.item_size <- as.data.frame(dummy(training$item_size))

d.class <- as.data.frame(dummy(training$class))
d.title <- as.data.frame(dummy(training$user_title))
d.na_age <- as.data.frame(dummy(training$na_age))
d.na_delivery <- as.data.frame(dummy(training$na_delivery))
temp <- training[,names(training) %in% names(training[,is.numeric(training) == TRUE])]
temp
is.numeric(training) == TRUE
