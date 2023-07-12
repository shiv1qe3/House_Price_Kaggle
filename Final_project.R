#importing libraries
library(knitr)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(moments)
library(caret)
library(glmnet)
library(corrplot)
library(randomForest)


#importing the dataset
train <- read.csv("train.csv")
test <- read.csv("test.csv")
#creating an combined set for data cleaning and feature engineering
combined_set<- bind_rows(train,test)

#plotting SalePrice to check the skewness
ggplot(data=combined_set[!is.na(combined_set$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = ,)

##Plotting Correlation plot between numeric value which has higher correlation than 0.5.
numericVars <- which(sapply(combined_set, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

all_numVar <- combined_set[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#checking dimensions and structure
trainDim <- dim(train)
trainDim
testDim <- dim(test)
testDim
trainR <- trainDim[1];testR <- testDim[1]
trainR
trainC <- trainDim[2];testC <- testDim[2]
trainC


# data cleaning

#cleaning the combined_set data

#imputing the missing values

# define a function to check NAs as we will be using it's frequenty to check
# what NAs are left after imputation
checkNAs <- function(df){
  # identify columns (by column number) with missing values
  naCols <- which(colSums(is.na(df))>0)
  # get columns with missing values sorted by number of missing values
  sort(colSums(sapply(combined_set[naCols],is.na)), decreasing = TRUE)
}

checkNAs(combined_set)


#imputation of individual features that can not be predicted in combined(test+train) dataset using any other features are:

#Fence
#Alley
#MiscFeature
#Utilities
#Functional
#Exterior1st
#Exterior2nd


#visualizing frequency distribution of these variables:

# visualizing Fence
plot1 <- combined_set%>% 
  ggplot(aes(x = Fence))+
  geom_histogram(stat = "count")+
  geom_label(stat='count',aes(label=..count..))

grid.arrange(plot1, ncol = 2)

# visualizing Alley
plot2 <- combined_set%>% 
  ggplot(aes(x = Alley))+
  geom_histogram(stat = "count")+
  geom_label(stat='count',aes(label=..count..))

grid.arrange(plot2, ncol = 2)

# visualizing MiscFeature
plot3 <- combined_set%>% 
  ggplot(aes(x = MiscFeature))+
  geom_histogram(stat = "count")+
  geom_label(stat='count',aes(label=..count..))

grid.arrange(plot3, ncol = 2)

# visualizing Utilities
plot4 <- combined_set%>% 
  ggplot(aes(x = Utilities))+
  geom_histogram(stat = "count")+
  geom_label(stat='count',aes(label=..count..))

grid.arrange(plot4, ncol = 2)

# visualizing Functional
plot5 <- combined_set%>% 
  ggplot(aes(x = Functional))+
  geom_histogram(stat = "count")+
  geom_label(stat='count',aes(label=..count..))

grid.arrange(plot5, ncol = 2)

# visualizing Exterior1st 
plot6 <- combined_set%>% 
  ggplot(aes(x = Exterior1st))+
  geom_histogram(stat = "count")+
  geom_label(stat='count',aes(label=..count..))+
  coord_flip()

grid.arrange(plot6, ncol = 2)

#Visualizing Exterior2nd
plot7 <- combined_set%>% 
  ggplot(aes(x = Exterior2nd))+
  geom_histogram(stat = "count")+
  geom_label(stat='count',aes(label=..count..))+
  coord_flip()

grid.arrange(plot7, ncol = 2)

#Conclusion:
#Fence - NA value indicates no fence. Changing the value to None
#Alley - NA value indicates no alley. Changing the value to None
#Misc Feature - NA means no additional misc features. Changing the value to None 
#utilities - There is only Na in utilities so we will be replacing those values by AllPub (2916 values out of 2919)as almost all values are Allpub in the dataset.
#Functional - Functional has 2 NA values so we are replcaing it by Typ value(2717 values out of 2919) as almost all values are Typ in the dataset. 
#Exterior1 - we have only one missing value in this so we are replacing the NA by maximum used value.
#Exterior2 - we have only one missing value in this so we are replacing the NA by maximum used value.

#Imputation on the basis of above analysis

combined_set$Fence[is.na(combined_set$Fence)] <- "None"
combined_set$Alley[is.na(combined_set$Alley)] <- "None"
combined_set$MiscFeature[is.na(combined_set$MiscFeature)] <- "None"
combined_set$Utilities[is.na(combined_set$Utilities)] <- "AllPub"
combined_set$Functional[is.na(combined_set$Functional)] <- "Typ"
combined_set$Exterior1st[is.na(combined_set$Exterior1st)] <- "VinylSd"
combined_set$Exterior2nd[is.na(combined_set$Exterior2nd)] <- "VinylSd"



##Imputation based on other features which can help in predicting the missing values

##1 Pool variables

#in Pool quality we know that NA mean no pool(from data_description). So we are going to replace all the NA values with None.

combined_set$PoolQC[combined_set$PoolArea == 0] <- 'None'
combined_set[is.na(combined_set$PoolQC) & combined_set$PoolArea >0,c("PoolQC","PoolArea")]


#we are going to replace these 3 values with the closest value from mean of poolarea.

# calculate the means of PoolAreas for each of the PoolQC in the data set
mean_pool_area <- combined_set[!is.na(combined_set$PoolQC),c("PoolQC","PoolArea")] %>% 
  group_by(PoolQC) %>% 
  summarize(AreaMean = round(mean(PoolArea),0))
mean_pool_area

# we are going to replace 2421 with Ex
# 2504 with Ex
# 2600 with Fa
combined_set$PoolQC[2421] <- 'Ex'
combined_set$PoolQC[2504] <- 'Ex'
combined_set$PoolQC[2600] <- 'Fa'

#changing values to the values are ordinal.
combined_set$PoolQC<-as.integer(recode(combined_set$PoolQC, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5))
table(combined_set$PoolQC)


##2 Fireplace - fireplace is related with fireplace quality.
#if there exists a fireplace then there should be fireplace quality.
#checking missing values of fireplace quality for which the values present for Fireplaces.

# such houses should have "None" values in the FireplaceQu feature.
combined_set$FireplaceQu[is.na(combined_set$FireplaceQu) & combined_set$Fireplaces == 0] <- "None"

# check that houses with no fireplaces all have "None" value for FireplaceQu
fireplace <- combined_set[combined_set$Fireplaces == 0 ,c("Fireplaces","FireplaceQu")]

table(fireplace)


Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

combined_set$FireplaceQu[is.na(combined_set$FireplaceQu)] <- 'None'
combined_set$FireplaceQu<-as.integer(recode(combined_set$FireplaceQu, 'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5))
table(combined_set$FireplaceQu)


##3 Lot Variables (Lot frontage and Lot Shape)
#House within the same neighborhood tend to have similar lot frontage.
ggplot(combined_set[!is.na(combined_set$LotFrontage),], aes(x=as.factor(Neighborhood), y=LotFrontage)) +
  geom_bar(stat='summary', fun.y = "median", fill='purple') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

for (i in 1:nrow(combined_set)){
  if(is.na(combined_set$LotFrontage[i])){
    combined_set$LotFrontage[i] <- as.integer(median(combined_set$LotFrontage[combined_set$Neighborhood==combined_set$Neighborhood[i]], na.rm=TRUE)) 
  }
}


##4 Garage Variable
# 7 feature variables in this

# Feature Name        Number of NA's
#GarageCars           1
#GarageArea           1 
#GarageType           157
#GarageYrBlt          159
#GarageFinish         159
#GarageQual           159
#GarageCond           159

#changing the values to None where NA indicates that there is no Garage.

combined_set$GarageType[is.na(combined_set$GarageType)] <- 'None'
combined_set$GarageFinish[is.na(combined_set$GarageFinish)] <- 'None'
combined_set$GarageQual[is.na(combined_set$GarageQual)] <- 'None'
combined_set$GarageCond[is.na(combined_set$GarageCond)] <- 'None'

#putting the values of year built(YearBuilt) into Garage Year built(GarageYrBlt)

combined_set$GarageYrBlt[is.na(combined_set$GarageYrBlt)] <- combined_set$YearBuilt[is.na(combined_set$GarageYrBlt)]

#changing the value of NA to 0 for GarageArea and GarageCars
combined_set$GarageArea[is.na(combined_set$GarageArea)] <- 0
combined_set$GarageCars[is.na(combined_set$GarageCars)] <- 0


## 5. Basement Variable:
## "BsmtQual"     "BsmtCond"     "BsmtExposure" "BsmtFinType1"
## "BsmtFinSF1"   "BsmtFinType2" "BsmtFinSF2"   "BsmtUnfSF"   
## "TotalBsmtSF"  "BsmtFullBath" "BsmtHalfBath"

#check if all 79 NAs are the same observations among the variables with 80+ NAs
# From excel we observed that BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2 has the common 79 NA
#Find the additional NAs; BsmtFinType1 is the one with 79 NAs

combined_set[!is.na(combined_set$BsmtFinType1) & (is.na(combined_set$BsmtCond)|is.na(combined_set$BsmtQual)|is.na(combined_set$BsmtExposure)|is.na(combined_set$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]


# will impute on the basis of the result for the above table
# will be imputing using the mode for each of these variables
#333 has 1 NA for BsmtFinType2
#949, 1488, 2349 has 1 NA for BsmtExposure repectively.
#2041,2186 has 1 NA for BsmtCond respectively.
#2218,2219 has 1 NA for BsmtQual repectively.
#2525 has 1 NA for BsmtCond 

combined_set$BsmtFinType2[333] <- names(sort(-table(combined_set$BsmtFinType2)))[1]
combined_set$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(combined_set$BsmtExposure)))[1]
combined_set$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(combined_set$BsmtCond)))[1]
combined_set$BsmtQual[c(2218, 2219)] <- names(sort(-table(combined_set$BsmtQual)))[1]

# putting None value for all those variables where NA means No Basement
combined_set$BsmtQual[is.na(combined_set$BsmtQual)] <- 'None'
combined_set$BsmtFinType1[is.na(combined_set$BsmtFinType1)] <- 'None'
combined_set$BsmtFinType2[is.na(combined_set$BsmtFinType2)] <- 'None'
combined_set$BsmtExposure[is.na(combined_set$BsmtExposure)] <- 'None'
combined_set$BsmtCond[is.na(combined_set$BsmtCond)] <- 'None'

# Replacing integer value NA with 0 
combined_set$BsmtFinSF1[is.na(combined_set$BsmtFinSF1)] <-0
combined_set$BsmtFinSF2[is.na(combined_set$BsmtFinSF2)] <-0
combined_set$BsmtUnfSF[is.na(combined_set$BsmtUnfSF)] <-0
combined_set$TotalBsmtSF[is.na(combined_set$TotalBsmtSF)] <-0
combined_set$BsmtFullBath[is.na(combined_set$BsmtFullBath)] <-0
combined_set$BsmtHalfBath[is.na(combined_set$BsmtHalfBath)] <-0


## 6. Masonary veneer type and Masonary veneer area
#checking the missing value
combined_set[is.na(combined_set$MasVnrType) | is.na(combined_set$MasVnrArea), c("MasVnrType","MasVnrArea")]

#So all the values has same NA, just one value corresponding to 2611 has a value in MasVnrArea where its type is missing
# changing the value of MasVnrType to None where there is no area given.
combined_set$MasVnrType[is.na(combined_set$MasVnrType) & is.na(combined_set$MasVnrArea)] <- "None"

#changing the MasVnrArea NA to 0 
combined_set$MasVnrArea[is.na(combined_set$MasVnrArea)] <- 0

#Putting mode in 2611
combined_set$MasVnrType[2611] <- names(sort(-table(combined_set$MasVnrType)))[2] 
combined_set[2611, c('MasVnrType', 'MasVnrArea')]

## 7. MS Zoning
#Linking zoning with MSSubClass

combined_set[is.na(combined_set$MSZoning), c("MSSubClass","MSZoning")]

#creating distribution of house type versus its zoning classification 
# for the three dwelling types of the missing zoning values
missZoning <- unique(combined_set$MSSubClass[is.na(combined_set$MSZoning)])
combined_set[!is.na(combined_set$MSZoning) & combined_set$MSSubClass %in% missZoning, c("MSZoning","MSSubClass")] %>% 
  ggplot(aes(x = MSZoning, fill = factor(MSSubClass)))+
  geom_histogram(stat = "count")

#from the histogram we can see that house type of 70 and 30 have zoning classification of RL and RM.
#type 20 is of zoning RL. 
#imputing accordingly.

combined_set$MSZoning[is.na(combined_set$MSZoning) & combined_set$MSSubClass %in% c(70,30)] <- "RM"
combined_set$MSZoning[is.na(combined_set$MSZoning) & combined_set$MSSubClass == 20] <- "RL"

## 8. Kitchen Variables
#Replcaing kitchen quality with most common value i.e. TA
combined_set$KitchenQual[is.na(combined_set$KitchenQual)] <- "TA"

## 9 . Electrical System
#imputing mode
combined_set$Electrical[is.na(combined_set$Electrical)] <- names(sort(-table(combined_set$Electrical)))[1]

##10. Sale condition (SaleType and SaleCondition)
#linking sale type to sale condition
combined_set[is.na(combined_set$SaleType), c("SaleType","SaleCondition")]

combined_set[!is.na(combined_set$SaleType) & !is.na(combined_set$SaleCondition), c("SaleType","SaleCondition")] %>% 
  ggplot(aes(x = SaleType, fill = factor(SaleCondition)))+
  geom_histogram(stat = "count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#majority sale condition is of type WD
combined_set$SaleType[is.na(combined_set$SaleType)] <- "WD"

##checking if there is any remaing NA's in the dataset
checkNAs(combined_set)

##Moving onto the next step of feature Engineering
#converting highly skewed numerical variables into their log values.

# create a new copy of the master data set (allSet)
combined_set_new <- combined_set

# get classes of features in the data set
featureClasses <- sapply(names(combined_set_new), function(x){class(combined_set_new[[x]])})

# get numeric or integer class features
numFeatures <- names(featureClasses[featureClasses == "numeric" | featureClasses == "integer"])

# get character class features
charFeatures <- names(featureClasses[featureClasses == "character"])

# determine skewness of each numeric feature
skewedVals <- sapply(numFeatures, function(x){skewness(combined_set_new[[x]],na.rm = T)})

# identify skewed features with threshold of -2,+2
skewedFeatures <- skewedVals[skewedVals < -2 | skewedVals > 2]

# log-transform skewed features
for (i in names(skewedFeatures)) {
  combined_set_new[[i]] <- log(combined_set_new[[i]] + 1)
}

##Converting character features into dummy variables
dummies <- dummyVars(~., combined_set_new[charFeatures])
dummyVariables <- predict(dummies, combined_set_new[charFeatures])

# compile the data set again by combining both numerical and dummy variables features
combined_set_new <- cbind(combined_set_new[numFeatures], dummyVariables)
combined_set_new

##Moving onto the next step of model training and testing

#Spiting the data back into train and test

sales_price_with_Na_val <- which(is.na(combined_set_new["SalePrice"]))
train <- combined_set_new[-sales_price_with_Na_val,]
test <- combined_set_new[sales_price_with_Na_val,]

#Splitting the train dataset

set.seed(42) #So that we can reproduce, we set the random "seed"
c <- train[sample(1:1460),] #and sample(1:1460) just mixes up the (1460) rows
#We designate 4/5 of the set as the "training" set and 1/5 as the "test" set
data_train <- train[1:1168,]
data_test  <- train[1169:1460,]

#1. Linear Model

ols_full <- lm(SalePrice ~ ., data = data_train)
summary(ols_full)

# BASIC VALIDATION

PredScores <- predict(ols_full) #Determine predictions
Eps <- PredScores - data_train$SalePrice #...and residuals
par(mfrow=c(1,2)) #Let's plot and see if they look Normal-ish
hist(Eps)
qqnorm(Eps) #Doesn't look normal
qqline(Eps)

#...but is just the correlation of predictions and outcomes -- nice interpretation
cor(PredScores,data_train$SalePrice)^2
#...in  R just from model
summary(ols_full)$r.squared

#But note that this is optimistic as it is based on in-sample fit
PredScores <- predict(ols_full,newdata=data_test) #Determine predictions
test_lm<-log(PredScores+1)
##cor(PredScores,data_test$SalePrice)^2
test.y = log(data_test$SalePrice+1)
linear.rmse <- sqrt(mean((test_lm-test.y)^2))
linear.rmse


#Forward selection Model
#A key difference is that we no longer require the test sample!  So really we can use the full data set:
ols_full_forw <- lm(SalePrice ~ ., data = data_train ) 
step(ols_full_forw, direction="forward")
summary(ols_full_forw)$r.squared

#But note that this is optimistic as it is based on in-sample fit
PredScores <- predict(ols_full_forw,newdata=data_test) #Determine predictions
PredScores
test_fm <- log(PredScores+1)
test_fm
##cor(PredScores,data_test$SalePrice)^2
test.y = log(data_test$SalePrice+1)
forward.rmse <- sqrt(mean((test_fm-test.y)^2))
forward.rmse

## For the next three models (Ridge Regression, Lasso Regression, GBM) we are implimenting using the same Y (outcome) and not doing the log transformation.
##Ridge regression
# convert the train and test data sets into matrices excluding the Id and SalePrice features
train.Matrix =as.matrix(data_train[,names(data_train) != c("Id","SalePrice")])
test.Matrix =as.matrix(data_test[,names(data_test) != c("Id","SalePrice")])

# create Outcome variables for train and test
train.y = (data_train$SalePrice)
test.y = (data_test$SalePrice)


set.seed(4)
# create a grid of lambda values
grid = 10^seq(10,-2, length = 100)

# train the Ridge Regression model (alpha = 0) using the grid of selected lambdas
ridge.mod = glmnet(train.Matrix,train.y,alpha = 0, lambda = grid)
dim(coef(ridge.mod))


set.seed(1)
cv.out <- cv.glmnet(train.Matrix, train.y, alpha = 0)
plot(cv.out)
ridgeBestLambda <- cv.out$lambda.min
ridgeBestLambda;log(ridgeBestLambda)

# predict the response (SalePrice) in the testing data using the best lambda
ridge.predict <- (predict(ridge.mod,s = ridgeBestLambda, newx = test.Matrix))
ridge.rmse <- sqrt(mean((ridge.predict-test.y)^2))
ridge.rmse
test.Matrix_1 =as.matrix(test[,names(test) != c("Id","SalePrice")])
#perdicting on the test set
ridge.predict <- (predict(ridge.mod,s = ridgeBestLambda, newx = test.Matrix_1))
ridge.predict
# writing the file in CSV
write.csv(ridge.predict,"ridge_solution.csv",row.names=FALSE)

#LASSO

set.seed(5)
lasso.mod = glmnet(train.Matrix,train.y,alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(train.Matrix, train.y, alpha = 1)
plot(cv.out)
lassoBestlambda <- cv.out$lambda.min
lassoBestlambda
lasso.predict <- predict(lasso.mod,s = lassoBestlambda, newx = test.Matrix)
lasso.predict
lasso.rmse <- sqrt(mean((lasso.predict-test.y)^2))
lasso.rmse
#predicting the test values
lasso.predict_new <- predict(lasso.mod,s = lassoBestlambda, newx = test.Matrix_1)
lasso.predict_new
#writing the CSV
write.csv(lasso.predict_new,"lasso_solution.csv",row.names=FALSE)

#GBM
set.seed(2)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = FALSE)

gbm.mod <- train(SalePrice~., data = data_train, method = "gbm", trControl = control, verbose = FALSE)

gbm.mod
#predicting on data_test
gbm.predict <- predict(gbm.mod,newdata = data_test)
gbm.rmse <- sqrt(mean((gbm.predict-test.y)^2))
gbm.rmse

#predicting on test data
gbm.predict_new <- predict(gbm.mod,newdata = test)
gbm.predict_new
write.csv(gbm.predict_new,"gbm_solution.csv",row.names=FALSE)


##
# again Predicting on all the 3 models (Ridge, Lasso, GBM) using log to tranform the output #variable Y (sale Price) for better RMSE visualization values.
#Ridge regression
# convert the train and test data sets into matrices excluding the Id and SalePrice features
train.Matrix =as.matrix(train[,names(train) != c("Id","SalePrice")])
test.Matrix = as.matrix(test[,names(test) != c("Id","SalePrice")])

# create a vector of the log-transformed response (SalePrice)
train.y = log(train$SalePrice + 1)

set.seed(4)
# create a grid of lambda values
grid = 10^seq(10,-2, length = 100)

# train the Ridge Regression model (alpha = 0) using the grid of selected lambdas
ridge.mod = glmnet(train.Matrix,train.y,alpha = 0, lambda = grid)
dim(coef(ridge.mod))

set.seed(5)
# split the train data set into training and testing data sets (75/25 ratio).
# number of training rows
nTrain <- round(0.75 * nrow(train.Matrix))

# sample row IDs
sampleTrain <- sample(nrow(train.Matrix),nTrain)

# create training and testing data sets
training <- train.Matrix[sampleTrain,]
testing <- train.Matrix[-sampleTrain,]
training.y <- train.y[sampleTrain]
testing.y <- train.y[-sampleTrain]

set.seed(1)
cv.out <- cv.glmnet(training, training.y, alpha = 0)
plot(cv.out)

ridgeBestLambda <- cv.out$lambda.min
ridgeBestLambda;log(ridgeBestLambda)

ridge.predict <- predict(ridge.mod,s = ridgeBestLambda, newx = testing)
ridge.rmse <- sqrt(mean((ridge.predict-testing.y)^2))
ridge.rmse
#predicting on test set.
test.predict.ridge <- exp(predict(ridge.mod,s = ridgeBestLambda,newx = test.Matrix))-1
test.predict.ridge
#writing in csv file
write.csv(test.predict.ridge,"test_ridge.csv",row.names=FALSE)

##Lasso Regression
set.seed(5)
lasso.mod = glmnet(train.Matrix,train.y,alpha = 1, lambda = grid)
plot(lasso.mod)


set.seed(1)
cv.out <- cv.glmnet(training, training.y, alpha = 1)
plot(cv.out)

lassoBestlambda <- cv.out$lambda.min
lassoBestlambda;log(lassoBestlambda)


lasso.predict <- predict(lasso.mod,s = lassoBestlambda, newx = testing)
lasso.rmse <- sqrt(mean((lasso.predict-testing.y)^2))
lasso.rmse
test.predict.lasso <- exp(predict(lasso.mod, s = lassoBestlambda, newx = test.Matrix))-1
test.predict.lasso
#writing in csv file
write.csv(test.predict.lasso,"test_lasso.csv",row.names=FALSE)

##GBM
set.seed(5)
# re-split the combined data into train and test data sets
salesPriceNA <- which(is.na(combined_set_new["SalePrice"]))
train <- combined_set_new[-salesPriceNA,]
test <- combined_set_new[salesPriceNA,]

# convert the response in the train data into log
train$SalePrice <- log(train$SalePrice + 1)

# split the train data set into training and testing data sets (75/25 ratio)
# to validate the model

# number of training rows
nTrain <- round(0.75 * nrow(train))

# sample row IDs
sampleTrain <- sample(nrow(train),nTrain)

# create training and testing data sets
training <- train[sampleTrain,!names(train) == "Id"]
testing <- train[-sampleTrain,!names(train) == "Id"]

testing.y <- testing$SalePrice

set.seed(2)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = FALSE)

gbm.mod <- train(SalePrice~., data = training, method = "gbm", trControl = control, verbose = FALSE)

gbm.mod
gbm.predict <- predict(gbm.mod,newdata = testing)
gbm.rmse <- sqrt(mean((gbm.predict-testing.y)^2))
gbm.rmse


test.predict.gbm <- exp(predict(gbm.mod,newdata = test)) - 1
test.predict.gbm
write.csv(test.predict.gbm,"test_gbm.csv",row.names=FALSE)

# predicting using ensemble model
solution <- data.frame(Id = as.integer(rownames(test)),SalePrice =  as.numeric(test.predict.ridge*.2 + test.predict.lasso*.2 + test.predict.gbm*.6))

#writing the result in csv file.
write.csv(solution,"ensemble_sol.csv",row.names=FALSE)

solution1 <- data.frame(Id = as.integer(rownames(data_test)),SalePrice =  as.numeric(test.predict.ridge*.2 + test.predict.lasso*.2 + test.predict.gbm*.60))
solution <- solution1$SalePrice
ensemble.rmse <- sqrt(mean((solution-test.y)^2))
ensemble.rmse
