library(lubridate)
library(corrgram)
library(lattice)
library(ggplot2)
library(mice)
library(VIM)
library(ggplot2)
library(GGally)
library(PerformanceAnalytics)
library(mlbench)
library(caret)

#for avoiding exponential prints
options(scipen=999)

setwd("/Users/arnabdutta/git/CarPricePrediction")
# car sale prediction

# impute the missing values by regressin on other variables
data_imputation <- function(df){
  # check which columns are missing and how many
  #print(md.pattern(df)) 
  
  # impute by predictive mean matching
  df_only_numeric <- subset(df, select = -c(category))
  df_non_numeric <- subset(df, select = c(category))
  
  # impute with predictive mean matching
  imputed_Data <- mice(df_only_numeric, m=5, maxit = 5, method = 'pmm', seed = 500) 
  
  # get the imputed data frame
  completed_df <- complete(imputed_Data,1)
  
  # return with the categorical values concated back
  return(cbind(df_non_numeric, completed_df))
}


# feature engineer
feature_engineer <- function(df){
  
  # convert modification time to year, month
  df$modification_year <- year(df$modification_time)
  df$modification_month <- month(df$modification_time)
  df$modification_time <- NULL
  
  # get car registration year, month
  df$first_registration_year <- df$first_registration %/% 100
  df$first_registration_month <- df$first_registration %% 100
  df$first_registration <- NULL
  
  # create some additional features
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  df$car_age <- current_year - df$first_registration_year
  df$mod_at_age <- df$modification_year - df$first_registration_year

  # drop the other years and months
  df$modification_year <- NULL
  df$modification_month <- NULL
  df$first_registration_year <- NULL
  df$first_registration_month <- NULL
  
  # convert to factor representation
  df$category <- factor(df$category)
  
  return(df)
}

# cleanses the input data frame
cleanse_and_transform <- function(df){
  # find na containing columns
  na_cols <- colnames(df)[apply(is.na(df), 2, any)]
  print(na_cols)
  
  # impute model_id with column mode
  cond.map <- c("NEW"=1, "USED"=0)
  central.lock.map <- c("false"=0, "true"=1)
  makeid.map <- c("1900"=0, "25100"=1)
  
  # convert the 2-level factors to numeric 
  df$condition <- cond.map[as.character(df$condition)]
  df$features_central_locking <- central.lock.map[as.character(df$features_central_locking)]
  df$make_id <- makeid.map[as.character(df$make_id)]
  
  # convert all logical columns to numeric
  cols <- sapply(df, is.logical)
  df[,cols] <- lapply(df[,cols], as.numeric)
  
  # drop redundant ones and create other ones
  df <- feature_engineer(df)
  
  return(df)
}


feature_scaling <- function(df){
  df$mileage  <- scale(df$mileage , center = TRUE, scale = max(df$mileage , na.rm = TRUE))
  df$mod_at_age  <- scale(df$mod_at_age , center = TRUE, scale = max(df$mod_at_age , na.rm = TRUE))
  df$car_age  <- scale(df$car_age , center = TRUE, scale = max(df$car_age , na.rm = TRUE))
  
  return(df)
}

feature_selection <- function(df){
  # for each columns, calculate variance,
  print(dim(df))
}

############################################################
# read the training data
############################################################
read_train <- read.csv("data/train.dsv", sep="|", quote = "\"")
read_test <- read.csv("data/test.dsv", sep="|", quote = "\"")

############################################################
# cleanse and data transformation
# changing data types, creating new features
############################################################
df_train <- cleanse_and_transform(read_train)
df_test <- cleanse_and_transform(read_test)
read_train <- NULL
read_test <- NULL

df_only_numeric <- subset(df_train, select = -c(category))
# fancy visualizations
aggr(df_only_numeric, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(df_only_numeric), prop = TRUE, digits=2, 
                  combined = TRUE, bars=TRUE, only.miss = TRUE, cex.axis=0.24, 
                  gap=3, ylab=c("missing data","Pattern"))


df_only_numeric_test <- subset(df_test, select = -c(category))

# fancy visualizations
aggr(df_only_numeric_test, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(df_only_numeric_test), prop = TRUE, digits=2, 
                  combined = TRUE, bars=TRUE, only.miss = TRUE, cex.axis=0.24, 
                  gap=3, ylab=c("missing data","Pattern"))

############################################################
# impute missing values by regression
############################################################
imputed_df_train <- data_imputation(df_train)
imputed_df_test <- data_imputation(df_test)
df_train <- NULL
df_test <- NULL

############################################################
# feature scaling, standardizing
# convert all the numeric columns to mean 0, unit variance
############################################################
scaled_df_train <- feature_scaling(imputed_df_train)
scaled_df_test <- feature_scaling(imputed_df_test)
imputed_df_train <- NULL
imputed_df_test <- NULL

############################################################
# feature selection
# drop features with 0 variance, if any
############################################################

nearZeroVar(scaled_df_train)

selected_features <- feature_selection(scaled_df_train)

# both the train and test should have the same feature space
curated_df_train <- subset(scaled_df_train, select = -c(selected_features))
curated_df_test <- subset(scaled_df_test, select = -c(selected_features))



sort(diag(var(complete_df)))
complete_df$features_adaptive_cruise_ctl <- NULL


correlationMatrix <- cor(engineered_df[,2:13])

# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=2, repeats=1)
# train the model
model <- train(price__consumer_gross_euro~., data=engineered_df, method="blassoAveraged", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

engineered_df <- subset(engineered_df, category == 'OffRoad')
trainIndex <- createDataPartition(engineered_df$price__consumer_gross_euro, p = .8, list = FALSE)

scaled_df_train <- engineered_df[ trainIndex,]
scaled_df_test  <- engineered_df[-trainIndex,]

print(dim(scaled_df_test))
print(dim(scaled_df_train))

model_final <- train(price__consumer_gross_euro~
                       mileage   + mod_at_age  + car_age,         data=scaled_df_train, 
                     method="blassoAveraged", 
                     preProcess="scale")

summary(model_final)

# predict
predicted <- predict(model_final, newdata = subset(scaled_df_test, select = c(mileage, mod_at_age, car_age)))
actuals <- scaled_df_test$price__consumer_gross_euro

# calculate residuals
residuals<- predicted - actuals
# standardize residuals
st_residuals <- (residuals - mean(residuals))/sqrt(var(residuals))

# plot to check
qplot(predicted, actuals) + geom_abline(aes(slope=1,intercept=0,color="red"), size =1.3)
qplot(predicted, st_residuals) + geom_abline(aes(slope=0,intercept=0,color="red"), size =1.3)


RMSE(as.numeric(predicted), as.numeric(actuals))
