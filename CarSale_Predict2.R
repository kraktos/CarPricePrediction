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
  print(md.pattern(df)) 
  
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
  
  # convert the 2-level factors to numeric 
  df$condition <- cond.map[as.character(df$condition)]
  df$features_central_locking <- central.lock.map[as.character(df$features_central_locking)]
  
  # convert all logical columns to numeric
  cols <- sapply(df, is.logical)
  df[,cols] <- lapply(df[,cols], as.numeric)
  
  # drop redundant ones and create other ones
  df <- feature_engineer(df)
  
  return(df)
}


feature_scaling <- function(df){
  df$mileage  <- scale(df$mileage , center = FALSE, scale = max(df$mileage , na.rm = TRUE))
  df$mod_at_age  <- scale(df$mod_at_age , center = FALSE, scale = max(df$mod_at_age , na.rm = TRUE))
  df$car_age  <- scale(df$car_age , center = FALSE, scale = max(df$car_age , na.rm = TRUE))
  
  return(df)
}

# read the training data
read_train <- read.csv("data/train.dsv", sep="|", quote = "\"")

# cleanse and data transformation
df <- cleanse_and_transform(read_train)

df_only_numeric <- subset(df, select = -c(category))

# fancy visualizations
aggr_plot <- aggr(df_only_numeric, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(df_only_numeric), prop = TRUE, digits=2, combined = TRUE, bars=TRUE, only.miss = TRUE, cex.axis=0.24, gap=3, ylab=c("missing data","Pattern"))

# impute missing values by regression
complete_df <- data_imputation(df)

sort(diag(var(complete_df)))
complete_df$features_adaptive_cruise_ctl <- NULL

# feature scaling
engineered_df <- feature_scaling(complete_df)

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

trainIndex <- createDataPartition(engineered_df$price__consumer_gross_euro, p = .8, list = FALSE)

engineered_df_TRAIN <- engineered_df[ trainIndex,]
engineered_df_TEST  <- engineered_df[-trainIndex,]

print(dim(engineered_df_TEST))
print(dim(engineered_df_TRAIN))

model_final <- train(price__consumer_gross_euro~mileage+mod_at_age+car_age+condition+features_central_locking, 
                     data=engineered_df_TRAIN, 
                     method="blassoAveraged", 
                     preProcess="scale")

summary(model_final)

# predict
predicted <- predict(model_final, newdata = subset(engineered_df_TEST, select = -c(category)))
actuals <- engineered_df_TEST$price__consumer_gross_euro

# calculate residuals
residuals<- predicted - actuals

# plot to check
plot(predicted, residuals)
abline(0,0)

RMSE(as.numeric(predicted), as.numeric(actuals))
