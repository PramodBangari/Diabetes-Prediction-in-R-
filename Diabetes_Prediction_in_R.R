## 
## Logistic Regression 
##
library(dplyr)
library(ggplot2)

## Setting current working directory
setwd("F:/Imarticus/Projects/")

## Loading the data file
df_diabetes = read.csv("logisticregression/diabetes.csv")

## Viewing some sample data
head(df_diabetes)

## Dimension of the loaded dataframe
dim(df_diabetes)

## Basic statistics 
summary(df_diabetes)

## Checking is there any missing values
sum(is.na(df_diabetes))

## Columnwinse missing values if any
col_miss_values = colSums(is.na(df_diabetes))
class(col_miss_values)
length(col_miss_values)
## From the above statements we can observe that there are any missing values

df_diabetes$Outcome = as.factor(df_diabetes$Outcome)
head(df_diabetes$Outcome)
glimpse(df_diabetes)

## Exploring the Insulin variable

temp_df = df_diabetes %>% filter(Insulin <= 0) 
dim(temp_df)
table(temp_df$Outcome)
range(df_diabetes$Insulin)

## Exploring the Target variable
table(df_diabetes$Outcome)
round(prop.table(table(df_diabetes$Outcome)),2)

## Let us build the model with base data
## Splitting train and test set
df_sample = sort(sample(1:nrow(df_diabetes), nrow(df_diabetes)*0.75))
df_sample[1:10]

df_train = df_diabetes[df_sample, ]
df_test = df_diabetes[-df_sample, ]
dim(df_train)
dim(df_test)
## Exploring the Train and Test set Target Variable 
round(prop.table(table(df_train$Outcome)),2)
round(prop.table(table(df_test$Outcome)),2)

## Building the model
lr1 = glm(Outcome ~ ., data=df_train, family = "binomial")
summary(lr1)

## Making Predictions
pred_lr1 <- predict(lr1,select(df_test, -Outcome))
sample(pred_lr1, 10)
pred_lr1 <- ifelse(pred_lr1 >= 0.5, 1, 0)
sample(pred_lr1,10)

prop.table(table(df_test$Outcome))
prop.table(table(pred_lr1))

library(caret)
## Get Confusion Matrix
confusionMatrix(data = factor(pred_lr1), 
                reference = factor(df_test$Outcome), 
                positive = "1")

## With Scaled Data
df1 = df_diabetes
dim(df1)
head(df1)
#df_diabetes = df1
df_diabetes = df_diabetes %>% select(-Outcome)
str(df_diabetes)

df_diabetes = scale(df_diabetes)
head(df_diabetes)

df_diabetes = cbind(df_diabetes, df1["Outcome"])
colnames(df_diabetes)
class(df_diabetes)

df1$Insulin[1:5]

##  Scale function formula (0-mean(df1$Insulin))/sd(df1$Insulin)

library(caTools)

train = sample.split(df_diabetes$Outcome, SplitRatio = 0.8)
train[1:10]
## Let us build the model with Scaled data
## Splitting train and test set
df_sample = sort(sample(1:nrow(df_diabetes), nrow(df_diabetes)*0.8))

df_train = subset(df_diabetes, train == TRUE)
df_test = subset(df_diabetes, train == FALSE)

## Exploring the Train and Test set Target Variable 
round(prop.table(table(df_train$Outcome)),2)
round(prop.table(table(df_test$Outcome)),2)

## Building the model
lrs = glm(Outcome ~ ., data=df_train, family = "binomial")
summary(lrs)

## Making Predictions
pred_lrs <- predict(lrs,select(df_test, -Outcome))
sample(pred_lrs, 10)
pred_lrs <- if_else(pred_lrs >= 0.4, 1, 0)
sample(pred_lrs,10)

prop.table(table(df_test$Outcome))
prop.table(table(pred_lrs))

library(caret)
## Get Confusion Matrix
confusionMatrix(data = factor(pred_lrs), 
                reference = factor(df_test$Outcome), 
                positive = "1")

## Exploratory Data Analysis

# Copying the original df from backup to working df.
df_diabetes = df1
head(df_diabetes)
dim(df_diabetes)
## Checking the target variable
table(df_diabetes$Outcome)

## Percentage of Targets
round(prop.table(table(df_diabetes$Outcome)*100),2)

## Visualizing Target variable
barplot(table(df_diabetes$Outcome), xlab="Diabetees Yes or No", ylab="Frequency",
        main="Target Variable", 
        col=as.factor(df_diabetes$Outcome))

ggplot(df_diabetes, aes(as.factor(Outcome), fill=as.factor(Outcome))) +
  geom_bar() +
  xlab("Diabetes Yes or No") +
  ylab("Count") +
  ggtitle("Diabetes Target variable Count")

## Pregnancies Vs Target variable
table(df_diabetes$Pregnancies, df_diabetes$Outcome)

## Visualizing Age Variable
range(df_diabetes$Age)

hist(df_diabetes$Age,breaks = 6)

ggplot(df_diabetes, aes(Age)) +
  geom_histogram(bins=10)


## We can create bins for Age
df2 = df1 %>% mutate(AgeGrp = case_when(Age > 60 ~ "Old",
                                        Age > 50 ~ "NotOld",
                                        Age > 40 ~ "UpperMiddle",
                                        Age > 30 ~ "Middle",
                                        TRUE ~"Young"))

table(df2$AgeGrp, df2$Outcome)

ggplot(df2, aes(AgeGrp, fill=as.factor(Outcome))) +
  geom_bar(position = position_dodge()) +
  ggtitle("Agegroup Vs Diabetes Outcome") +
  scale_fill_discrete(name = "Outcome")

## Exploring Blood Pressure variable 
range(df_diabetes$BloodPressure)
median(df_diabetes$BloodPressure)
table(df_diabetes$BloodPressure)
sum(df_diabetes$BloodPressure == 0)

df_diabetes = df2
## Creating Bloodpressure group
df3 = df2 %>% mutate(BPGrp = case_when(BloodPressure > 90 ~ "HighBP",
                                       BloodPressure >= 80 ~ "Normal",
                                        TRUE ~"LowBP"))
table(df3$BPGrp, df3$Outcome)

ggplot(df3, aes(BPGrp, fill=as.factor(Outcome))) +
  geom_bar(position = position_dodge()) +
  ggtitle("BP Group Vs Diabetes Outcome") +
  scale_fill_discrete(name = "Outcome")

df_diabetes = df3
df_diabetes$Outcome = as.factor(df_diabetes$Outcome)
glimpse(df_diabetes)

range(df_diabetes$Glucose)
df_diabetes$GlucoseLevel = ifelse(df_diabetes$Glucose <= 150, "Normal","High")

range(df_diabetes$SkinThickness)

## Exploring Insulin with Outcome
range(df_diabetes$Insulin)

ggplot(df_diabetes, aes(Insulin)) +
  geom_histogram(bins = 10)

df_diabetes$insulinIndicator = ifelse(df_diabetes$Insulin <= 170, "Normal","High")
ggplot(df_diabetes, aes(insulinIndicator, fill=as.factor(Outcome))) +
  geom_bar(position = position_dodge()) +
  ggtitle("Insulin Indicator Vs Diabetes Outcome") +
  scale_fill_discrete(name = "Outcome")

## Exploring BMI & Imputing the missing values
range(df_diabetes$BMI)
sum(df_diabetes$BMI == 0.0)
df_diabetes[df_diabetes$BMI == 0.0, "BMI"] = NA
df_diabetes[is.na(df_diabetes$BMI), "AgeGrp"]
df_diabetes[(is.na(df_diabetes$BMI) & (df_diabetes$AgeGrp == "Young")), "BMI"] = median(df_diabetes[df_diabetes$AgeGrp == "Young", "BMI"], na.rm = TRUE)
sum(is.na(df_diabetes$BMI))

df2 = df_diabetes  ## Just taking a backup

df_diabetes = df2 %>% mutate(BMIGrp = case_when(BMI > 30 ~ "Obese",
                                       BMI > 25 ~ "Overweight",
                                       TRUE ~"Normal"))

ggplot(df_diabetes, aes(BMIGrp, fill=as.factor(Outcome))) +
  geom_bar(position = position_dodge()) +
  ggtitle("BMI Group Vs Diabetes Outcome") +
  scale_fill_discrete(name = "Outcome")

glimpse(df_diabetes)

## Selecting only the required variables
df_final = df_diabetes %>% select(-c("Glucose","BloodPressure","Insulin","BMI","Age"))
glimpse(df_final)

factors <- names(which(sapply(df_final, is.character)))
factors

## Converting all character variables to factor variables
df_final[sapply(df_final, is.character)] = lapply(df_final[sapply(df_final, is.character)], 
                                      as.factor)
glimpse(df_final)

## Splitting train and test set
df_sample = sort(sample(1:nrow(df_final), nrow(df_final)*.8))
df_train = df_final[df_sample,]
df_test = df_final[-df_sample,]

## Checking Target variable Ratio
round(prop.table(table(df_train$Outcome)),2)
round(prop.table(table(df_test$Outcome)),2)

## Building the model
lr2 = glm(Outcome ~ ., data =df_train, family = "binomial")
summary(lr2)

pred_lr2 = predict(lr2, df_test)
pred_lr2 = ifelse(pred_lr2 >= 0.5, 1, 0)

table(pred_lr2)

confusionMatrix(factor(pred_lr2), df_test$Outcome, positive = "1")

## Converting Dichotomy variables to numeric
df_final$GlucoseLevel = as.character(df_final$GlucoseLevel)

df_final$GlucoseLevel = ifelse(df_final$GlucoseLevel == "Normal",0,1)
df_final$insulinIndicator = ifelse(df_final$insulinIndicator == "Normal",0,1)
glimpse(df_final$GlucoseLevel)

str(df_final)

# Saving names of categorical variables
factors <- names(which(sapply(df_final[,-4], is.factor)))
factors 


str(df_final)
library(CatEncoders)
# Label Encoder
for (i in factors){
  encode <- LabelEncoder.fit(df_final[, i])
  df_final[, i] <- transform(encode, df_final[, i])
}
str(df_final)

## Splitting train and test set
df_sample = sort(sample(1:nrow(df_final), nrow(df_final)*.8))
df_train = df_final[df_sample,]
df_test = df_final[-df_sample,]

## Checking Target variable Ratio
round(prop.table(table(df_train$Outcome)),2)
round(prop.table(table(df_test$Outcome)),2)

## Building the model
lr3 = glm(Outcome ~ ., data = df_train, family = binomial)
summary(lr3)

pred_lr3 = predict(lr3, df_test)
sample(pred_lr3)
pred_lr3 = ifelse(pred_lr3 >= 0.3, 1, 0)

table(pred_lr3)

confusionMatrix(factor(pred_lr3), df_test$Outcome, positive = "1")

## ROC Curve**
library(ROCit)

ROCit_base <- rocit(score=pred_lr3,class=df_test$Outcome)
plot(ROCit_base)

library(randomForest)
rf1 = randomForest(Outcome ~ ., data=df_train)
summary(rf1)
varImp(rf1)
varImpPlot(rf1)

pred_rf1 = predict(rf1, df_test)

confusionMatrix(factor(pred_rf1), df_test$Outcome, positive = "1")

## ROC Curve**
ROCit_base <- rocit(score=as.numeric(pred_rf1),class=as.numeric(df_test$Outcome))
plot(ROCit_base)

## After removing unimportant variables
df_train = df_train %>% select(-c("BPGrp","insulinIndicator"))
df_test = df_test %>% select(-c("BPGrp","insulinIndicator"))

rf2 = randomForest(Outcome ~ ., data=df_train)
summary(rf2)

pred_rf2 = predict(rf2, df_test)

confusionMatrix(factor(pred_rf2), df_test$Outcome, positive = "1")

## ROC Curve**
ROCit_base <- rocit(score=as.numeric(pred_rf2),class=as.numeric(df_test$Outcome))
plot(ROCit_base)
