library(lubridate)
library(corrgram)
library(lattice)
library(ggplot2)
library(mice)
library(VIM)
library(ggplot2)


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
  imputed_Data <- mice(df_only_numeric, m=5, maxit = 50, method = 'pmm', seed = 500) 
  
  # get the imputed data frame
  completed_df <- complete(imputed_Data,1)
  
  # return with the categorical values concated back
  return(cbind(df_non_numeric, completed_df))
}

# feature engineer
feature_engineer <- function(df){
  # get car registration year, month
  df$first_registration_year <- df$first_registration %/% 100
  df$first_registration_month <- df$first_registration %% 100
  df$first_registration <- NULL
  
  # create some additional features
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  df$age_since_reg <- current_year - df$first_registration_year
  df$age_since_mod <- current_year - df$modification_year
  
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
  
  # convert mod time to year, month
  df$modification_year <- year(df$modification_time)
  df$modification_month <- month(df$modification_time)
  df$modification_time <- NULL
  
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

# feature engineering
engineered_df <- feature_engineer(complete_df)

sum(is.na(df$model_id))
sum(is.na(df$first_registration))
sum(is.na(df$mileage))

sum(is.na(complete_df$model_id))
sum(is.na(complete_df$first_registration))
sum(is.na(complete_df$mileage))

# use the imputed data frame to  
sp <- ggplot(engineered_df, aes(x=age_since_reg, y=price__consumer_gross_euro)) + geom_point(shape=1)
sp + facet_grid(category ~ .)


sp <- ggplot(engineered_df, aes(x=age_since_mod, y=price__consumer_gross_euro)) + geom_point(shape=1)
sp + facet_grid(category ~ .)

edf <- subset(engineered_df, select = -c(category))
              
cor(as.matrix(edf[,3]), as.matrix(edf[,-3]))
pairs(edf)
