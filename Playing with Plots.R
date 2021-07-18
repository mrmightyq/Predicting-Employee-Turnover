HR <- read.csv("C:/Users/KnudseQ/Desktop/HR_Dataset for Class.csv", stringsAsFactors = TRUE)
attach(HR)


.libPaths("/home/rstudioshared")
library(dplyr)
library(ggplot2)
ggplot(data = HR, aes( x =  JobSatisfaction,  fill = as.factor(Attrition )))+ #Define the x and fill value
  #I use fill instead of color because color refers only to the perimeter of the bars and fill to the
  # inside of each figure
  geom_bar()  + # Add the bar plot
  facet_grid(~Gender) # Change the default colors

ggplot(data = HR, aes( x =  WorkLifeBalance + JobSatisfaction,  fill = as.factor(Attrition) ) ) +
  geom_bar(position = 'dodge')

#Monthly income with years at company
scatterplot(MonthlyIncome~YearsAtCompany,data=HR,main="Distribution of monthly
            income with work experience",ylab="Monthly Income"
            ,xlab = "Years at company")

ggplot(data= HR, aes(x = BusinessTravel)) +
  geom_bar(aes(fill=as.factor(WorkLifeBalance))) +
  coord_flip()

interaction.plot(Attrition, JobSatisfaction, MonthlyIncome, type="b", 
                col=c("red","blue"), pch=c(16, 18),
                main = "Interaction between attrition and job satisfaction",data=HR)

HR %>% 
  count(WorkLifeBalance = factor(WorkLifeBalance), JobSatisfaction = factor(JobSatisfaction)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = WorkLifeBalance, y = pct, fill = JobSatisfaction, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)

HR %>% 
  count(WorkLifeBalance = factor(BusinessTravel), JobSatisfaction = factor(JobSatisfaction)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = WorkLifeBalance, y = pct, fill = JobSatisfaction, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)

HR %>% 
  count(WorkLifeBalance = factor(WorkLifeBalance), Attrition = factor(Attrition)) %>% 
  mutate(Percent = prop.table(n)) %>% 
  ggplot(aes(x = WorkLifeBalance, y = Percent, fill = Attrition, label = scales::percent(Percent))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)


HR %>% 
  count(Gender = factor(Gender), Attrition = factor(Attrition)) %>% 
  mutate(Percent = prop.table(n)) %>% 
  ggplot(aes(x = Gender, y = Percent, fill = Attrition, label = scales::percent(Percent))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)

qplot(Age, JobSatisfaction, data=HR, color=Gender)
g <- ggplot(HR, aes(Attrition, OverTime))

g + geom_hex() + geom_smooth()

g + geom_point() + geom_smooth()

g + geom_hex(alpha=0.3) + geom_smooth(color="red", lwd=2) + geom_point(size=3) 

ggplot(HR, aes(x=Gender, y=Attrition)) + geom_hex()
ggplot(titanic, aes(x=Age, y=Fare)) + geom_smooth()
ggplot(titanic, aes(x=Age, y=Fare)) + geom_smooth(method="lm")
ggplot(titanic, aes(x=Age, y=Fare)) + geom_density2d()


#stacked bar chart by percent 
str(HR)
# library
library(ggplot2)

# bucket work life balance
WorkLifeBalance <- cut(x = WorkLifeBalance, breaks = c(1, 25, 50, 75, 100))

# Rename the levels 
levels(AAA_factor) <- c("low", "medium", "high", "very_high")
# Stacked + percent
ggplot(HR, aes(fill=MaritalStatus, y=JobSatisfaction, x=Gender)) + 
  geom_bar(position="fill", stat="identity")


#Plot of Attrition 
ggplot(HR,aes(Attrition,fill=Attrition))+geom_bar()
#Percentage of Attrition
prop.table(table(HR$Attrition))

library(grid)
library(gridExtra)
agePlot <- ggplot(HR,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
travelPlot <- ggplot(HR,aes(BusinessTravel,fill=factor(Attrition)))+geom_bar()
ratePlot <- ggplot(HR,aes(DailyRate,Attrition))+geom_point(size=4,alpha = 0.05)
depPlot <- ggplot(HR,aes(Department,fill = Attrition))+geom_bar()#Fix axes text direction to avoid overlap
grid.arrange(agePlot,travelPlot,ratePlot,depPlot,ncol=2,top = "Fig. 1")


ggplot(HR,aes(BusinessTravel,fill=factor(JobSatisfaction)))+geom_bar()
ggplot(HR,aes(BusinessTravel,fill=factor(WorkLifeBalance)))+geom_bar()
ggplot(HR,aes(BusinessTravel,fill=factor(Gender)))+geom_bar()

distPlot <- ggplot(HR,aes(DistanceFromHome,fill=factor(Attrition)))+geom_bar()
distPlot
eduPlot <- ggplot(HR,aes(Education,fill=factor(Attrition)))+geom_bar()
edufieldPlot
edufieldPlot <- ggplot(HR,aes(EducationField,fill=factor(Attrition)))+geom_bar()#fix text like above
edufieldPlot
envPlot <- ggplot(HR,aes(EnvironmentSatisfaction,fill=factor(Attrition)))+geom_bar()
envPlot
genPlot <- ggplot(HR,aes(Gender,fill=factor(Attrition)))+geom_bar()
genPlot
grid.arrange(distPlot,eduPlot,edufieldPlot,envPlot,genPlot,ncol=2,top = "Fig. 2")



#Feature Engineering 
HR_2 <- HR
HR$TenurePerJob <- round(ifelse(HR$NumCompaniesWorked!=0, 
                           HR$TotalWorkingYears/HR$NumCompaniesWorked,0),2)
HR$TenurePerJob


HR$TimeSincePromotion <- HR$YearsInCurrentRole - HR$YearsSinceLastPromotion
HR$TimeSincePromotion


Training1_os$YearsWithoutChange2 <- Training1_os$TotalWorkingYears - 
              Training1_os$YearsSinceLastPromotion

HR$JobLevel_centered <- (HR$JobLevel-mean(HR$JobLevel))/sd(HR$JobLevel)
HR$MonthlyIncome_centered <- (HR$MonthlyIncome-mean(HR$MonthlyIncome))/sd(HR$MonthlyIncome)
HR$BigShot <- HR$JobLevel_centered + HR$MonthlyIncome_centered
HR$UnderValueJobLevel <- HR$JobLevel_centered - HR$MonthlyIncome_centered

HR$DistanceFromHomeCentered <- (HR$DistanceFromHome-mean(HR$DistanceFromHome))/sd(HR$DistanceFromHome)
HR$BusinessTravelCentered <- (HR$BusinessTravel-mean(HR$BusinessTravel))/sd(HR$BusinessTravel)
HR$AlwaysOnTheRoad <- HR$DistanceFromHomeCentered + HR$BusinessTravelCentered

HR$YearsInCurrentRoleCentered <- (HR$YearsInCurrentRole-mean(HR$YearsInCurrentRole))/sd(HR$YearsInCurrentRole)
HR$YearsWithCurrManagerCentered <- (HR$YearsWithCurrManager-mean(HR$YearsWithCurrManager))/sd(HR$YearsWithCurrManager)
HR$YearsSinceLastPromotionCentered <- (HR$YearsSinceLastPromotion-mean(HR$YearsSinceLastPromotion))/sd(HR$YearsSinceLastPromotion)
HR$Stagnating <- HR$YearsInCurrentRoleCentered + HR$YearsWithCurrManagerCentered + HR$YearsSinceLastPromotionCentered

head(HR$YearsInCurrentRoleCentered)
head(scale(YearsInCurrentRole))



HR$AvgSatis <- scale(EnvironmentSatisfaction)+scale(JobInvolvement)+
                                    scale(JobSatisfaction)+scale(RelationshipSatisfaction)+
                                      scale(WorkLifeBalance)/4
#Binning 
HR$AgeGroup <- with(HR,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,
               ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1)))))))) #Creating Age Groups

HR$DistanceGroup <- with(HR,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,
                ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,ifelse(DistanceFromHome>5,2,1)))))) #Creating Distance Groups

HR$YearsWithManagerGroup <- with(Training1_os,ifelse(YearsWithCurrManager>15,5,
                          ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,
                          ifelse(YearsWithCurrManager>2,2,1))))) #Creating YearsWithManager Groups

HR$TenureGroup <- with(Training1_os,ifelse(TenurePerJob>35,9,ifelse(TenurePerJob>30,8,
                  ifelse(TenurePerJob>25,7,ifelse(TenurePerJob>20,6,ifelse(TenurePerJob>15,5,
                  ifelse(TenurePerJob>10,4,ifelse(TenurePerJob>5,3,
                  ifelse(TenurePerJob>2,2,1))))))))) #Creating Tenure Per Job groups

HR$Change2Group <- with(Training1_os,ifelse(YearsWithoutChange2>10,3,
                   ifelse(YearsWithoutChange2>5,2,1))) #Creating Years Without Change2

HR$Change1Group <- with(Training1_os,ifelse(YearWithoutChange>2.5,3,
                   ifelse(YearWithoutChange>-2.5,2,1))) #Creating Years Without Change 1

#HR$AvgSatisGroup <- with(Training1_os,ifelse(AvgSatis<2.5,1,2)) # Create Average Satisfaction Groups

HR$WorkYearGroup <- with(Training1_os,ifelse(TotalWorkingYears>35,9,
                    ifelse(TotalWorkingYears>30,8,ifelse(TotalWorkingYears>25,7,
                    ifelse(TotalWorkingYears>20,6,ifelse(TotalWorkingYears>15,5,
                    ifelse(TotalWorkingYears>10,4,ifelse(TotalWorkingYears>5,3,
                    ifelse(TotalWorkingYears>2,2,1)))))))))

HR$NumCompGroup <- with(Training1_os,ifelse(NumCompaniesWorked>4,3,
                  ifelse(NumCompaniesWorked>2,2,1))) #Creating Number of Companies Worked


#Correlation matrix
library(corrplot)
library(psych)

Training_cor <- HR

for(i in 1:ncol(Training_cor)){
  
  Training_cor[,i]<- as.integer(Training_cor[,i])
}

corrplot(cor(Training_cor))
plot(cor.ci(Training_cor))


# Coding the categorical Variables

HR$BusinessTravel <- as.integer(HR$BusinessTravel)
HR$Department <- as.integer(HR$Department)
HR$Gender <- as.integer(HR$Gender)
HR$MaritalStatus <- as.integer(HR$MaritalStatus)
HR$OverTime <- as.integer(HR$OverTime)
HR$JobRole <- as.integer(HR$JobRole)
HR$EducationField <- as.integer(HR$EducationField)


HR_1 <- HR
for(i in 1:ncol(HR)){
  HR[,i] <- as.factor(HR[,i])
}

library(caret)
fit_rpart <- train(Attrition ~.,HR,method = 'rpart', trControl = trainControl(method = 'cv',number = 3))

fit_rpart <- train(Attrition ~.,Train,method = 'rpart', trControl = trainControl(method = 'cv',number = 3))

set.seed(123)
fit_rf <- train(Attrition ~.,HR,method = 'rf', trControl = trainControl(method = 'repeatedcv',number = 3)) # Random Forest
plot(varImp(fit_rf))
cor(as.numeric(HR$Attrition),HR$MonthlyIncome)
print(fit_rf)
library(randomForest)
rf <- randomForest(as.factor(Attrition)~., data = HR)
print(rf)
(varImp(rf))
plot(varImp(rf))
  

xgbGrid <- expand.grid(nrounds = 300,
                       max_depth = 1,
                       eta = 0.3,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9)

set.seed(12)
fit_xgb <- train(Attrition ~.,HR,method = 'xgbTree',tuneGrid = xgbGrid,trControl = 
                   trainControl(method = 'repeatedcv',number = 3,classProbs = TRUE))


fit_nn <- train(Attrition ~.,HR,method = 'pcaNNet',trControl = trainControl
                (method = 'repeatedcv',number = 3),tuneGrid = expand.grid(size = 25,decay = 0.01))
