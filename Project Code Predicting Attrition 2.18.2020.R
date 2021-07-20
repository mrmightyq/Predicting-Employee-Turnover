#Import and Store Dataset into 'HR'
HR <- read.csv("C:/Users/KnudseQ/Desktop/HR_Dataset for Class.csv", stringsAsFactors = TRUE)
attach(HR)
#EXplore data and variable types
summary(HR)
str(HR)
#Create Dummy Columns
#library(psych)
#HR$Department <- dummy.code(HR$Department)
#HR$EducationField <- dummy.code(HR$EducationField)
#HR$JobRole <- dummy.code(HR$JobRole)
#HR$MaritalStatus <- dummy.code(HR$MaritalStatus)
#Load Rcmdr
library(Rcmdr)
#Histogram of Job Sat by Department

#Non-significant findings with M/F t-tests

#T test distance from home and attrition
t.test(DistanceFromHome ~ Attrition, alternative = "two.sided", conf.level = 0.95, 
       var.equal = FALSE, data = HR)

# T Test job satisfaction and attrition
t.test(JobSatisfaction ~ Attrition, alternative = "two.sided", conf.level = 0.95, 
       var.equal = FALSE, data = HR)
#Years with Current Manager T Test
t.test(YearsWithCurrManager ~ Attrition, alternative = "two.sided", conf.level = 0.95, 
       var.equal = FALSE, data = HR)
# Work life balance T Test
t.test(WorkLifeBalance ~ Attrition, alternative = "two.sided", conf.level = 0.95, 
       var.equal = FALSE, data = HR)
#Model predicting job satisfaction
jsat_linear <- lm(JobSatisfaction ~ HourlyRate + NumCompaniesWorked, data = HR)
summary(jsat_linear)
#Convert Conceptually Scalar Variables to Numeric
#HR$BusinessTravel <- as.numeric(HR$BusinessTravel)
#HR$Gender <- as.numeric(HR$Gender)
#HR$OverTime <- as.numeric(HR$OverTime)


#Remove Irrelevant Column: Employee Count
HR <- HR[,-9]
#Remove Irrelevant Column: Employee ID
HR <- HR[,-9]
#Remove Irrelevant Column: Over18
HR <- HR[,-20]
#Remove Irrelevant Column: Standard Hours
HR <- HR[,-24]

#Summary of Dataset
summary(HR)

#Bivariate Analysis
library(corrplot)
corrplot(cor(sapply(HR,as.integer)),method = "pie")

#Feature Engineering

#Holistic Statisfaction 
HR$SatisfactionOverall <- scale(HR$EnvironmentSatisfaction)+
  scale(HR$JobSatisfaction)+scale(HR$RelationshipSatisfaction)+
  scale(HR$WorkLifeBalance)/4

#Stagnation Metric
HR$Stagnating <- scale(HR$YearsInCurrentRole)+scale(HR$YearsWithCurrManager)+
                 scale(HR$YearsSinceLastPromotion)/3
  
#Tenure per Job Worked  
HR$TenurePerJob <- round(ifelse(HR$NumCompaniesWorked!=0, 
                                HR$TotalWorkingYears/HR$NumCompaniesWorked,0),2)

#Value for High Job Importance to Pay
HR$UnderValueJobLevel <- scale(HR$JobLevel)/median(MonthlyIncome)

#High Job Importance, High Pay
HR$BigShot <- scale(HR$JobLevel)+scale(HR$MonthlyIncome)

#Time Since Promoted
HR$TimeSincePromotion <- scale(HR$YearsInCurrentRole) - scale(HR$YearsSinceLastPromotion)

#Excessive Travel
HR$AlwaysOnTheRoad <- scale(HR$DistanceFromHome) + HR$BusinessTravel

#Compa Ratio per job role and overall
#Compa ratio overall
MedianCompaOverall <- median(HR$MonthlyIncome)
HR$OverallCompaRatio <- HR$MonthlyIncome/MedianCompaOverall
#Compa per job department 
HRCompaMedian <- median(HR[HR$Department == 'Human Resources',]$MonthlyIncome)
SalesCompaMedian <- median(HR[HR$Department == 'Sales',]$MonthlyIncome)
RDCompaMedian <- median(HR[HR$Department == 'Research & Development',]$MonthlyIncome)
HR$DepartmentCompaRatio <- ifelse(HR$Department == 'Human Resources',HR$MonthlyIncome/HRCompaMedian,
                           ifelse(HR$Department=='Research & Development',HR$MonthlyIncome/RDCompaMedian,
                           HR$MonthlyIncome/SalesCompaMedian))
#Binning 

#Age Grouping
HR$AgeGroup <- with(HR,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,
              ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1)))))))) 
#Distance Grouping
HR$DistanceGroup <- with(HR,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,
                    ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,
                    ifelse(DistanceFromHome>5,2,1)))))) 
#Years with Current Manager
HR$YearsWithManagerGroup <- with(HR,ifelse(YearsWithCurrManager>15,5,
                            ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,
                            ifelse(YearsWithCurrManager>2,2,1))))) 

                                                                                                                        

#Exploring HR Dataset Visually
library(magrittr)
library(ggplot2)
library(knitr)
library(ggthemes)
library(dplyr)
library(forcats)

numeric=HR %>% dplyr::select(Age,DailyRate,DistanceFromHome,HourlyRate,MonthlyIncome,
                             MonthlyRate,NumCompaniesWorked,PercentSalaryHike,YearsAtCompany,
                             YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager,
                             TotalWorkingYears,TrainingTimesLastYear,StockOptionLevel)
corrplot(cor(numeric),method="circle",type="upper")

#Gender and Salary Boxplot
ggplot(HR,aes(Gender,MonthlyIncome,fill=Gender))+geom_boxplot()+theme_few()+
  theme(legend.position="none",plot.title=element_text(hjust=0.5,size=10))+
  labs(x="Gender",y="Salary",title="Salary with Gender", subtitle="Females Earn Slighly More on Average")
  +coord_flip()





# Partioning the data train 80% test 20%
set.seed(1234)
ind <- sample(2, nrow(HR), replace =T, prob = c(0.8, 0.2))
train <- HR[ind==1,]
test <- HR[ind==2,]

# Logistic regression w/out interactions
logistic <- glm(Attrition ~ Age + BusinessTravel + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
               Gender + HourlyRate + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + 
               MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike + 
               PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
               TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
               YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)
#Test for multicollinearity
exp(coef(logistic))
vif(logistic)
#Remove Hourly Rate
logistic <- glm(Attrition ~ Age + BusinessTravel + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
                  Gender + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + 
                  MonthlyIncome + MonthlyRate + NumCompaniesWorked + OverTime + PercentSalaryHike + 
                  PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                  YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)
#Remove Monthly Rate
logistic <- glm(Attrition ~ Age + BusinessTravel + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
                  Gender + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + 
                  MonthlyIncome + NumCompaniesWorked + OverTime + PercentSalaryHike + 
                  PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                  YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)
#Remove Job Level
logistic <- glm(Attrition ~ Age + BusinessTravel + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
                  Gender + JobInvolvement + JobSatisfaction + MaritalStatus + 
                  MonthlyIncome + NumCompaniesWorked + OverTime + PercentSalaryHike + 
                  PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                  YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)
#Remove Performance Rating
logistic <- glm(Attrition ~ Age + BusinessTravel + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
                  Gender + JobInvolvement + JobSatisfaction + MaritalStatus + 
                  MonthlyIncome + NumCompaniesWorked + OverTime + PercentSalaryHike 
                  + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                  YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)
#Remove Percent Salary Hike 
logistic <- glm(Attrition ~ Age + BusinessTravel + DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
                  Gender + JobInvolvement + JobSatisfaction + MaritalStatus + 
                  MonthlyIncome + NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
                  StockOptionLevel + TotalWorkingYears + 
                  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                  YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)
#Remove Daily Rate
logistic <- glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + EnvironmentSatisfaction + 
                  Gender + JobInvolvement + JobSatisfaction + MaritalStatus + 
                  MonthlyIncome + NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
                  StockOptionLevel + TotalWorkingYears + 
                  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                  YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)
#Remove Stock Option Level
logistic <- glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + EnvironmentSatisfaction + 
                  Gender + JobInvolvement + JobSatisfaction + MaritalStatus + 
                  MonthlyIncome + NumCompaniesWorked + OverTime + RelationshipSatisfaction  
                  + TotalWorkingYears + 
                  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                  YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)
#Remove Gender
logistic <- glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + EnvironmentSatisfaction  
                  + JobInvolvement + JobSatisfaction + MaritalStatus + 
                  MonthlyIncome + NumCompaniesWorked + OverTime + RelationshipSatisfaction  
                + TotalWorkingYears + 
                  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                  YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)
#Remove Total Years Working
logistic <- glm(Attrition ~ Age + BusinessTravel + DistanceFromHome + EnvironmentSatisfaction  
                + JobInvolvement + JobSatisfaction + MaritalStatus + 
                  MonthlyIncome + NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
                  TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                  YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(logit), data = train)
summary(logistic)

#Misclassification error - train data
p1 <- predict(logistic, train, type = 'response')
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$Attrition)
tab1
1 - sum(diag(tab1))/sum(tab1)

#Misclassification error - test data
log.prd <- predict(logistic, test, type = 'response')
pred2 <- ifelse(log.prd>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$Attrition)
tab2
1 - sum(diag(tab2))/sum(tab2)

#Plot Logistic
library(ROCR)
library(pROC)
log.plot <- plot.roc (as.numeric(test$Attrition),
                        as.numeric(log.prd),lwd=2, type="b", print.auc=TRUE, col ="blue")

#Goodness-of-fit test
with(logistic, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))

#Decision Tree
set.seed(123)
library(tree)
dtree.model <- tree::tree (Attrition ~., data = train)
plot(dtree.model)
text(dtree.model, all = T)
#Predict
dtree.prd <- predict(dtree.model, test, type = "class")
#Confusion Matrix
confusionMatrix(dtree.prd,test$Attrition)
#Roc Curve
dtree.plot <- plot.roc (as.numeric(test$Attrition),
                        as.numeric(dtree.prd),lwd=2, type="b", print.auc=TRUE, col ="blue")


#Random Forest
library(randomForest)
set.seed(222)  
rf <- randomForest(as.factor(Attrition)~., data = train)
print(rf)
#Prediction & Confusion Matrix - train data 
library(caret)
p3 <- predict(rf, train)
head(p3)
head(train$Attrition)
confusionMatrix(p3, train$Attrition)

# Prediction & Confusion Matrix - test data 
rf.prd <- predict(rf, test)
confusionMatrix(rf.prd, test$Attrition)

#plot ROC
rf.plot<- plot.roc(as.numeric(test$Attrition), as.numeric(p4),lwd=2, type="b",print.auc=TRUE,col ="blue")

#GBM
# Setting the basic train control used in all GBM models

ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# Simple GBM
library(gbm)
gbmfit <- train(Attrition ~., 
                data = train, 
                method = "gbm", 
                verbose = FALSE, 
                metric = "ROC", 
                trControl = ctrl)

gbmpreds <- predict(gbmfit, test)
library(ROCR)
library(pROC)
rocgbm <- roc(as.numeric(test$Attrition), as.numeric(gbmpreds))
rocgbm$auc


#Exterme Gradient Boost
formula = Attrition~.
fitControl <- trainControl(method="cv", number = 3,classProbs = TRUE )
xgbGrid <- expand.grid(nrounds = 50,
                       max_depth = 12,
                       eta = .03,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9
)
XGB.model <- train(formula, data = train,
                   method = "xgbTree"
                   ,trControl = fitControl
                   , verbose=0
                   , maximize=FALSE
                   ,tuneGrid = xgbGrid
)

ggplot(varImp(XGB.model)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') + 
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_light()

XGB.prd <- predict(XGB.model,test)
confusionMatrix(XGB.prd, test$Attrition)
XGB.plot <- plot.roc (as.numeric(test$Attrition),
                      as.numeric(XGB.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
# Neural Network 
#Dummy Coding
HR$Attrition <- ifelse(HR$Attrition=="Yes", 1, 0)
HR$Married <- ifelse(HR$MaritalStatus=="Married", 1, 0)
HR$Divorced <- ifelse(HR$MaritalStatus=="Divorced", 1,0)
HR$Single <- ifelse(HR$MaritalStatus=="Single", 1,0)
HR$OverTime <- ifelse(HR$OverTime=="Yes", 1,0)

library(neuralnet)
library(NeuralNetTools)
library(nnet)
nn1 <- nnet(Attrition~., data = train,size = 5,maxit = 2000,decay = .01)
plotnet(nn1)

nn.prd <- predict(nn1,test,type = "raw")
cutoff <- floor(nn.prd+.5)
plot(nn.prd)
abline(a=0.5,b=0,h=0.5)


#Assigning class to the test dataset based on the manual cutoff.
test$class_nn1 = ifelse(cutoff==1,"Yes","No")
test$class_nn1 <- as.factor(test$class_nn1)

## Evaluate Model Performance using class i.e, Yes/No.
confusionMatrix(test$class_nn1, reference = test$Attrition,positive = "Yes", mode = "everything")

nn.plot <- plot.roc (as.numeric(test$Attrition),
                      as.numeric(nn.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")

#old not good with categorical
n <- neuralnet(as.factor(Attrition) ~ DailyRate + DistanceFromHome + EnvironmentSatisfaction + 
                  HourlyRate + JobInvolvement + JobLevel + JobSatisfaction +  
                 MonthlyIncome + MonthlyRate + NumCompaniesWorked + PercentSalaryHike + 
                 PerformanceRating + RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                 TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + 
                 YearsSinceLastPromotion + YearsWithCurrManager + Married + Divorced + Single + OverTime, 
               data = train,
               hidden = c(4,2),
               stepmax = 1e6,  
               err.fct = "ce",
               lifesign = "minimal", 
               threshold = 0.1,
               linear.output = FALSE,
               rep = 1,
               algorithm = "rprop+")
plot(n)

#Prediction 
output <- compute(n, train[, -2])
head(output$net.result)
head(train[2])

#Confusion Matrix & Misclassification Error - training data
output <- compute(n, train[,-2])
p6 <- output$net.result
pred_nn <- ifelse(p6>0.5, 1, 0)
table_nn <- table(pred_nn, train$Attrition)
table_nn
1-sum(diag(table_nn))/sum(table_nn)

#Confusion Matrix & Misclassification Error - test data
output <- compute(n, test[,-2])
p7 <- output$net.result
pred_nntest <- ifelse(p7>0.5, 1, 0)
table_nntest <- table(pred_nntest, test$Attrition)
table_nntest
1-sum(diag(table_nntest))/sum(table_nntest)

#Model Performance Evaluation
library(ROCR)
neural_prediction <- predict(n, HR, type = 'prob')
neural_prediction <- prediction(neural_prediction, HR$Attrition)
eval <- performance(neural_prediction, "acc")
plot(eval)

#Identify Best Values
max <- which.max(slot(eval, "y.values")[[1]])
max
acc <- slot(eval, "y.values")[[1]][max]
acc
cut <- slot(eval, "x.values")[[1]][max]
cut
print(c(Accuracy=acc, Cutoff = cut))

# Reciever Operating Characteristics (ROC) Curve
roc <- performance(neural_prediction, "tpr","fpr")
plot(roc,
     colorize=T,
     main = "ROC Curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity")
abline(a=0, b=1)

# Area Under Curve (AUC)
auc <- performance(neural_prediction, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc,4) 
legend(.6, .2, auc, title = "AUC", cex = 1.2)

#Feature Importance of Neural NetModel
require(clusterGeneration)
require(nnet)
set.seed(2)
num.vars<-15
num.obs<-1470
require(devtools)
#Remove corp VPN
source_gist('6206737')
gar.fun('y',n)

#Support Vector Machine
library("e1071")
set.seed(123)
svm<-svm(
  train$Attrition~.,
  type="C-classification",
  data=train
)
# Prediction table for training data
t=table(train$Attrition,
        predict(svm,train[,-2]))/nrow(train)
sum(diag(t)) # Correct rate 
# Prediction for test data
t=table(test$Attrition,
        predict(svm,test[,-2]))/nrow(test)
sum(diag(t))
svm.prd <- predict(svm,newdata=test)
confusionMatrix(svm.prd,test$Attrition)
svm.plot <-plot.roc (as.numeric(test$Attrition), 
                     as.numeric(svm.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
svm.plot
#Tune SVM #Error message here
tuned <- tune(svm,factor(Attrition)~.,data = train)
svm.model <- svm(train$Attrition~., data=train
                 ,type="C-classification", gamma=tuned$best.model$gamma
                 ,cost=tuned$best.model$cost
                 ,kernel="radial")
svm.prd <- predict(svm.model,newdata=SVMtest.Data)
confusionMatrix(svm.prd,SVMtest.Data$Attrition)


#Ridge, Lasso Elastic Net
library(caret)
library(glmnet)
library(psych)

# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter =  T)

# Linear Model
set.seed(1234)
lm <- train(as.numeric(Attrition) ~.,
            train,
            method = 'lm',
            trControl = custom)
lm$results
lm
plot(lm$finalModel)

lm.prd <- predict(lm,newdata=test)
confusionMatrix(lm.prd,test$Attrition)
lm.plot <-plot.roc (as.numeric(test$Attrition), 
                     as.numeric(lm.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
# Ridge Regression
set.seed(1234)
ridge <- train(Attrition ~.,
               train,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = seq(0.001, 1, length=5)),
               trControl = custom)
plot(ridge)
plot(ridge$finalModel, xvar = 'lambda', label = T)
plot(ridge$finalModel, xvar = 'dev', label = T)
plot(varImp(ridge, scale=T))
#For ROC
ridge.prd <- predict(ridge,newdata=test)
confusionMatrix(ridge.prd,test$Attrition)
ridge.plot <-plot.roc (as.numeric(test$Attrition), 
                    as.numeric(lm.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
# Lasso Regression 
set.seed(1234)
lasso <- train(Attrition ~.,
               train,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = seq(0.001, 0.2, length=5)),
               trControl = custom)
# Plot Results
lasso
plot(lasso)
plot(lasso$finalModel, xvar = 'lambda', label = T)
plot(lasso$finalModel, xvar = 'dev', label = T)
#For ROC
lasso.prd <- predict(lasso,newdata=test)
confusionMatrix(lasso.prd,test$Attrition)
lasso.plot <-plot.roc (as.numeric(test$Attrition), 
                    as.numeric(lm.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")
#Elastic Net Regression 
set.seed(1234)
en <- train(Attrition ~.,
            train,
            method = 'glmnet',
            tuneGrid = expand.grid(alpha = seq(0,1, length=10),
                                   lambda = seq(0.001, 0.2, length=5)),
            trControl = custom)
plot(en)
plot(en$finalModel, xvar = 'lambda', label = T)
plot(en$finalModel, xvar = 'dev', label = T)
plot(varImp(en, scale=T))

en.prd <- predict(en,newdata=test, type="prob")
confusionMatrix(en.prd,test$Attrition)
en.plot <-plot.roc (as.numeric(test$Attrition), 
                    as.numeric(lm.prd),lwd=2, type="b", print.auc=TRUE,col ="blue")


#Compare Models
model_list <- list(LinearModel = lm, Ridge = ridge, Lasso = lasso, ElasticNet = en)
res <- resamples(model_list)
summary(res)
bwplot(res)
xyplot(res, 'RMSE')

#Best Model
en$bestTune
best <- en$finalModel
coef(best, s = en$bestTune$lambda)

# Save Final Modfel for Later Use
saveRDS(en, "final_model.rds")
fm <- readRDS("final_model.rds")
print(fm)

#Prediction
prediction_en <- predict(fm, train)
sqrt(mean(train$Attrition-prediction_en)^2)

prediction_en <- predict(fm, test)
sqrt(mean(test$Attrition-prediction_en)^2)


# List of predictions (option 2)
preds_list <- list(log_preds, nn_preds, rf_preds, gbm_preds, en_preds)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(HR$attrition), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Logistic Regression", "Neural Network", "Random Forest", "GBM", "Elastic Net"),
       fill = 1:m)

plot(rocrf, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, col = "darkolivegreen", add = T)
plot(rocgbm, ylim = c(0,1), print.thres = T, print.thres.cex = 0.8, col = "burlywood", add = T)


#plotting the ROC curves (better view) 
par(mfrow=c(5,2))
plot.roc (as.numeric(test$Attrition), as.numeric(XGB.prd),main="XGBoost",lwd=2, type="b", print.auc=TRUE, col ="blue")
plot.roc (as.numeric(test$Attrition), as.numeric(dtree.prd), main="Decision Tree",lwd=2, type="b", print.auc=TRUE, col ="brown")
plot.roc (as.numeric(test$Attrition), as.numeric(log.prd),main="Logistic",lwd=2, type="b", print.auc=TRUE, col ="green")
plot.roc (as.numeric(test$Attrition), as.numeric(svm.prd),main="SVM",lwd=2, type="b", print.auc=TRUE, col ="red")
plot.roc (as.numeric(test$Attrition), as.numeric(nn.prd), main="Neural Network",lwd=2, type="b", print.auc=TRUE, col ="seagreen4")
plot.roc (as.numeric(test$Attrition), as.numeric(rf.prd), main="Random Forest",lwd=2, type="b", print.auc=TRUE, col ="slateblue4")
plot.roc (as.numeric(test$Attrition), as.numeric(lm.prd), main="Linear Model",lwd=2, type="b", print.auc=TRUE, col ="orange")
plot.roc (as.numeric(test$Attrition), as.numeric(ridge.prd), main="Ridge",lwd=2, type="b", print.auc=TRUE, col ="purple")
plot.roc (as.numeric(test$Attrition), as.numeric(lasso.prd), main="Lasso",lwd=2, type="b", print.auc=TRUE, col ="goldenrod4")
plot.roc (as.numeric(test$Attrition), as.numeric(en.prd), main="Elastic Net",lwd=2, type="b", print.auc=TRUE, col ="gray")

#Over Sample Yes Responses
HR2 <- rbind(HR, HR[Attrition==1,])
HR5 <- rbind(HR, HR[HR$Attrition==1,], HR[HR$Attrition==1,], HR[HR$Attrition==1,], HR[HR$Attrition==1,])
nrow(HR5)
nrow(HR5[HR5$Attrition==1,])/nrow(HR5)


#Sort on High Probability of TO

#Export to Excel
write.xlsx(HR, file = "HR_TO_PRD.xlsx", sheetName = "Active Employees", append = TRUE)



