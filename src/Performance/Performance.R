###################################################################
####################### PART 1: DESCRIPTIVE #######################
###################################################################

## Clean the environment
rm(list=ls()) 

## Install and load packages
if (!require("pacman")) install.packages("pacman", quiet=TRUE) ; require("pacman")
p_load(rtoot, httpuv, tidyverse, lubridate, rvest, caret, dplyr)

## Set working directory
setwd("C:/Users/Helena/Documents/eerste_master/SMWA/Project_test/data_scrape")

######################
## Step 1: Scraping ##
######################

#############
## Spotify ##
#############

#############
## Youtube ##
#############


########################
## Step 2: Wordclouds ##
########################

########################
## Step 3: Wordgraphs ##
########################

#############################
## Step 4: Topic modelling ##
#############################

#############################
## Step 5: Word embeddings ##
#############################

###########################
## Step 6: Word networks ##
###########################

################################
## Step 7: Sentiment analysis ##
################################

##################################################################
####################### PART 2: PREDICTIVE #######################
##################################################################

################## Functions ####################

#Checks whether a variable is binary or not
is_binary <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L && all(x[1:2] == 0:1)
}

################## Read data ####################

## Save the training data
#save(Metallicafull, file = "metallicafull.RData")

## Load the training data by using the command:
load("metallicafull.RData")

## Save the test data
#save(Metallica_NewAlbum, file = "MetallicaNewAlbum.RData")

## Load the test data by using the command:
load("MetallicaNewAlbum.RData")

## Create Basetable
train_val_basetable <- Metallicafull
test <- Metallica_NewAlbum

################## Exploratory data analysis ####################

## Overview ----------------------------------------------------------------

check_cols <- sapply(train_val_basetable, is.numeric)

numeric_cols <- colnames(train_val_basetable[,check_cols])
numeric_cols
# the data contains 17 numeric columns

category_cols <- colnames(train_val_basetable[,!check_cols])
category_cols
#the data contains 41 categorical columns

## Numeric columns ---------------------------------------------------------

# Check if there are binary columns
binary_cols<-vapply(train_val_basetable[,numeric_cols], is_binary, logical(1))
binary_cols
#only mode is a binary variable
# 1 represents major, 0 represents minor

### Visual exploration

#We will first display histograms to see the distribution of the data 
for (column in numeric_cols){
  hist(train_val_basetable[[column]], xlab = column, main = column)
}

#Then, we display box plot to see the quantiles of the distribution
#and if there are a lot of outliers etc
for (column in numeric_cols){
  boxplot(train_val_basetable[[column]], xlab = column, main = column)
}

### Statistical exploration 

#we will make use if the summary function, which displays for each column,
#the minimum and maximum values, the quantiles, the mean and median as well
#as the number of missing values

for (column in numeric_cols){
  print(column)
  print(summary(train_val_basetable[[column]]))
  print("---------------------------------------------------")
}

## Categorical columns ---------------------------------------------------

### Statistical exploration
#this shows all the different unique values for each column
for (column in category_cols){
  print(column)
  print(unique(train_val_basetable[[column]]))
  print("---------------------------------------------------")
}


################## Split data ####################
#A seed is set for reproducing purposes
set.seed(1)

split_sample <- sample(nrow(train_val_basetable), 0.8*nrow(train_val_basetable))

training <- train_val_basetable[split_sample, ]
validation <- train_val_basetable[-split_sample, ]

#separate dependent and independent variables
training_X <- subset(training, select = -c(track.popularity))
training_y <- training$track.popularity

validation_X <- subset(validation, select = -c(track.popularity))
validation_y <- validation$track.popularity

str(training_X)
str(training_y)

################## Clean data ####################

## Take subset of only the usefull columns
## Note: we also deleted track.album as a variable because we want to predict the popularity of Metallicas newest album
training_X <- subset(training_X, select = c(track.id, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, track.duration_ms, track.name, track.track_number))
validation_X <- subset(validation_X, select = c(track.id, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, track.duration_ms, track.name, track.track_number))
test_X <- subset(test, select = c(track.id, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, track.duration_ms, track.name, track.track_number))

# Multiply the duration column to get duration in minutes
training_X$track.duration_ms <- training_X$track.duration_ms * 0.000016666666666666667
validation_X$track.duration_ms <- validation_X$track.duration_ms * 0.000016666666666666667
test_X$track.duration_ms <- test_X$track.duration_ms * 0.000016666666666666667
## Change the name of duration column
names(training_X)[names(training_X) == "track.duration_ms"] <- "duration_min"
names(validation_X)[names(validation_X) == "track.duration_ms"] <- "duration_min"
names(test_X)[names(test_X) == "track.duration_ms"] <- "duration_min"



## Parsing dates: no longer necessary after removing track.album.release_date as a variable

## Save your current locale
# original_locale <- Sys.getlocale(category = "LC_TIME")
## Change it to US
#Sys.setlocale(category = "LC_TIME", locale = "English_United States")

## Transform the album_release_date into a posixct format for further manipulations
# training_X$track.album.release_date <- as.POSIXct(training_X$track.album.release_date, format = "%Y-%m-%d")
# validation_X$track.album.release_date <- as.POSIXct(validation_X$track.album.release_date, format = "%Y-%m-%d")
# test_X$track.album.release_date <- as.POSIXct(test_X$track.album.release_date, format = "%Y-%m-%d")

## Change back to the original locale
# Sys.setlocale(category = "LC_TIME", locale = original_locale)

#################################
## Step 1: Feature engineering ##
#################################

###################### Encoding of categorical data ######################

## One-hot encoding
library(dummy)

#readability: a one in the column mode represents a major chord, a zero represents a minor chord
names(training_X)[names(training_X) == "mode"] <- "mode_major"
names(validation_X)[names(validation_X) == "mode"] <- "mode_major"
names(test_X)[names(test_X) == "mode"] <- "mode_major"

training_X$mode_major <- as.character(training_X$mode_major)
validation_X$mode_major <- as.character(validation_X$mode_major)
test_X$mode_major <- as.character(test_X$mode_major)

# Here are the categorical variables: only key is categorical, so this line is redundant
#cols_to_encode <- c("key")

# We retain all the possible categories
training_X$key <- as.character(training_X$key)
cats <- categories(training_X["key"])

# apply on train set and exclude reference category  ----- reference category: key 4 (E)
dummy_train <- dummy(training_X["key"], object = cats)
dummy_train <- subset(dummy_train, select = -c(key_4))
# apply on validation set and exclude reference categories
dummy_val <- dummy(validation_X["key"], object = cats)
dummy_val <- subset(dummy_val, select = -c(key_4))
# apply on test set and exclude reference categories
dummy_test <- dummy(test_X["key"], object = cats)
dummy_test <- subset(dummy_test,select = -c(key_4))

# merge with overall training set
training_X_encode <- subset(training_X, select = -c(key))
training_X_encode <- cbind(training_X_encode, dummy_train)
# merge with overall validation set
validation_X_encode <- subset(validation_X, select = -c(key))
validation_X_encode <- cbind(validation_X_encode, dummy_val)
# merge with overall test set
test_X_encode <- subset(test_X, select = -c(key))
test_X_encode <- cbind(test_X_encode, dummy_test)

#################################
## Step 1: Saving Data         ##
#################################

## Save data as train_set_X, val_set_X and test_set_X
save(training_X_encode, file = "train_set_X.RData")
save(validation_X_encode, file = "val_set_X.RData")
save(test_X_encode, file = "test_set_X.RData")

## Save popularity as train_set_y and val_set_y
save(training_y, file = "train_set_y.RData")
save(validation_y, file = "val_set_y.RData")


#####################
## Step 2: Predict ##
#####################

######################################
## Load in Data                     ##
######################################
load("train_set_X.RData")
load("val_set_X.RData")
load("test_set_X.RData")

load("train_set_y.RData")
load("val_set_y.RData")

## standardization

# Numeric columns in training_X_encode
check_cols <- sapply(training_X_encode, is.numeric)
numeric_cols <- colnames(training_X_encode[,check_cols]) 
numeric_cols

# do not scale the one-hot encoded columns
num.cols <- sapply(training_X_encode, is.numeric)
num.cols[names(num.cols) %in% c("track.id")] <- FALSE
num.cols

scale_cols <- c(num.cols)


# apply on training set
mean_train <- colMeans(training_X_encode[, scale_cols])
sd_train <- sapply(training_X_encode[, scale_cols], sd)
training_X_encode[, scale_cols] <- scale(training_X_encode[, scale_cols], center = TRUE, scale = TRUE)

#apply on validation test
validation_X_encode[, scale_cols] <- scale(validation_X_encode[, scale_cols], center = mean_train, scale = sd_train)

# apply on test set
test_X_encode[, scale_cols] <- scale(test_X_encode[, scale_cols], center = mean_train, scale = sd_train)

# check the distributions
colMeans(training_X_encode[, scale_cols])
sapply(training_X_encode[, scale_cols], sd)

colMeans(validation_X_encode[, scale_cols])
sapply(validation_X_encode[, scale_cols], sd)

colMeans(test_X_encode[, scale_cols])
sapply(test_X_encode[, scale_cols], sd)

######################################
## Baseline Model: Lasso Regression ##
######################################
train_X <- subset(training_X_encode, select = -c(track.id, track.name))
val_X <- subset(validation_X_encode, select = -c(track.id, track.name))

library(glmnet)
library(tidyr)
y <- unlist(training_y)
colnamesList = colnames(train_X)
x <- data.matrix(train_X[, colnamesList])

cv_model <- cv.glmnet(x, y, alpha = 1) # 10-fold cross-validation
plot(cv_model)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #1.488625

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

# Predictor variables as data.matrix
y1 <- unlist(validation_y)
colnamesList1 = colnames(val_X)
x1 <- data.matrix(val_X[, colnamesList1])

# Predicting the response of the validation set
y_predicted <- predict(best_model, s = best_lambda, newx = x1)

# Calculate SST and SSE
sst <- sum((y1 - mean(y1))^2)
sse <- sum((y_predicted - y1)^2)

# Calculate R-Squared
rsq <- 1 - sse/sst
rsq # 0.1940116

# Calculate RMSE
RMSE <- sqrt(mean((y_predicted - y1)^2))
RMSE # 9.438426

# Duplicate test_X
test_X2 <- test_X

# Predictor variables as data.matrix
test_X <- subset(test_X_encode, select = -c(track.id, track.name))

colnamesList2 = colnames(test_X)
x2 <- data.matrix(test_X[, colnamesList2])

# Combine the training and validation sets
x_full <- rbind(x, x1)
y_full <- c(y, y1)

# Convert predictor variables to data.matrix
x_full <- data.matrix(x_full)

# Train the model on the full dataset with the optimal lambda value
best_model <- glmnet(x_full, y_full, alpha = 1, lambda = best_lambda)

# Prediction based on the test set features
y_predicted <- predict(best_model, s = best_lambda, newx = x2)

# Create a dataframe of the predictions
prediction_lasso <- data.frame(track.name = test_X2$track.name, track.popularity = y_predicted)
colnames(prediction_lasso) <- c('track.name','track.popularity')

# View the predictions
view(prediction_lasso)

# Save the predictions as "prediction_lasso"
save(prediction_lasso, file = "prediction_lasso.RData")


##################
## Linear model ##
##################

## Linear Regression Model
lm_model <- lm(y ~ ., data = as.data.frame(x))

# Predict on validation set
val_y_pred <- predict(lm_model, newdata = as.data.frame(x1))

# Calculate SST and SSE
sst <- sum((y1 - mean(y1))^2)
sse <- sum((val_y_pred - y1)^2)

# Calculate R-Squared
rsq <- 1 - sse/sst
rsq # 0.1176606

# Calculate RMSE
RMSE <- sqrt(mean((val_y_pred - y1)^2))
RMSE # 9.875361

# Combine the training and validation sets
x_full <- rbind(x, x1)
y_full <- c(y, y1)

# Train the model on the full dataset
lm_model <- lm(y_full ~ ., data = as.data.frame(x_full))

# Predict on test set
test_y_pred <- predict(lm_model, newdata = as.data.frame(x2) )

# Combine predictions with track names into a dataframe
prediction_lm <- data.frame(track.name = test_X_encode$track.name, track.popularity = test_y_pred)

# Save predictions as "prediction_lm"
save(prediction_lm, file = "prediction_lm.RData")

################
## XGBoosting ##
################

## XGBoost Model
library(xgboost)

# Convert data to DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(x), label = y)
dval <- xgb.DMatrix(data = as.matrix(x1), label = y1)
dtest <- xgb.DMatrix(data = as.matrix(x2))

# Set parameters
params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  early_stopping_rounds = 10,
  watchlist = list(train = dtrain, val = dval),
  verbose = FALSE
)

# Predict on validation set
val_y_pred <- predict(xgb_model, newdata = as.matrix(x1))

# Calculate SST and SSE
sst <- sum((y1 - mean(y1))^2)
sse <- sum((val_y_pred - y1)^2)

# Calculate R-Squared
rsq <- 1 - sse/sst
rsq # 0.6576931

# Calculate RMSE
RMSE <- sqrt(mean((val_y_pred - y1)^2))
RMSE # 6.150961

# Combine training and validation sets
train_val_X <- rbind(x, x1)
train_val_y <- c(y, y1)

# Convert data to DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(train_val_X), label = train_val_y)

# Set parameters
params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  verbose = FALSE
)

# Predict on test set
test_y_pred <- predict(xgb_model, newdata = as.matrix(x2))

# Combine predictions with track names into a dataframe
prediction_xgb <- data.frame(track.name = test_X_encode$track.name, track.popularity = test_y_pred)

# Save predictions as "prediction_xgb"
save(prediction_xgb, file = "prediction_xgb.RData")

###################
## Random Forest ##
###################

## Random Forest Model
library(randomForest)

# Train model
rf_model <- randomForest(
  x = x,
  y = y,
  ntree = 1000,
  mtry = sqrt(ncol(x)),
  importance = TRUE,
  na.action = na.roughfix
)

# Predict on validation set
val_y_pred <- predict(rf_model, newdata = x1)

# Calculate SST and SSE
sst <- sum((y1 - mean(y1))^2)
sse <- sum((val_y_pred - y1)^2)

# Calculate R-Squared
rsq <- 1 - sse/sst
rsq # 0.3772861

# Calculate RMSE
RMSE <- sqrt(mean((val_y_pred - y1)^2))
RMSE # 8.296205

# Combine training and validation data
x_trainval <- rbind(x, x1)
y_trainval <- c(y, y1)

# Train model on training and validation data
rf_model <- randomForest(
  x = x_trainval,
  y = y_trainval,
  ntree = 1000,
  mtry = sqrt(ncol(x_trainval)),
  importance = TRUE,
  na.action = na.roughfix
)

# Predict on test set
test_y_pred <- predict(rf_model, newdata = x2)

# Combine predictions with track names into a dataframe
prediction_rf <- data.frame(track.name = test_X_encode$track.name, track.popularity = test_y_pred)

# Save predictions as "prediction_rf"
save(prediction_rf, file = "prediction_rf.RData")


###################
## Comparison    ##
###################

#load predictions
load("prediction_lasso.RData")
load("prediction_lm.RData")
load("prediction_rf.RData")
load("prediction_xgb.RData")

#change the column name of track.popularity according to the model used to make the predictions
names(prediction_lm)[names(prediction_lm) == "track.popularity"] <- "popularity.lm"
names(prediction_rf)[names(prediction_rf) == "track.popularity"] <- "popularity.rf"
names(prediction_xgb)[names(prediction_xgb) == "track.popularity"] <- "popularity.xgb"
names(prediction_lasso)[names(prediction_lasso) == "track.popularity"] <- "popularity.lasso"

#merge all dataframes
library(tidyverse)
prediction_overview <- list(prediction_lasso, prediction_lm, prediction_xgb, prediction_rf)
prediction_overview %>% reduce(inner_join, by='track.name')
prediction_overview <- data.frame(prediction_overview)
prediction_overview <- subset(prediction_overview, select = -c(track.name.1, track.name.2, track.name.3))

#only keep unique rows
prediction_overview <- distinct(prediction_overview)

# Save predictions as a csv file named "prediction_overview"
write.csv(prediction_overview, "C:/Users/irisc/OneDrive/Documenten/GitHub/SMWA/src/Performance/prediction_overview.csv", row.names = FALSE)

#save(prediction_overview, file = "prediction_overview.RData")