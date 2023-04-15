###################################################################
####################### PART 1: DESCRIPTIVE #######################
###################################################################

## Clean the environment
rm(list=ls()) 

## Install and load packages
if (!require("pacman")) install.packages("pacman", quiet=TRUE) ; require("pacman")
p_load(rtoot, httpuv, tidyverse, lubridate, rvest, caret, dplyr)

## Set working directory
setwd("C:/Users/irisc/OneDrive/Documenten/GitHub/SMWA/src/Performance")

######################
## Step 1: Scraping ##
######################

##############
## Mastodon ##
##############

## Set up authentication
## Only need to run this once
#auth_setup()

## Search for 50 toots using the metallica hashtag
toots <- get_timeline_hashtag("#metallica", limit = 50, retryonratelimit = TRUE) ##Only 6?

## Preview toot data
glimpse(toots)

############
## Reddit ##
############

#############
## Spotify ##
#############

#############
## Youtube ##
#############

#############
## Last.fm ##
#############

#############
##  ...    ##
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

## Load the training data by using the command:
load("metallicafull.RData")

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
training_X <- subset(training_X, select = c(track.id, track.album.release_date, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, track.duration_ms, track.name, track.track_number))
validation_X <- subset(validation_X, select = c(track.id, track.album.release_date, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, track.duration_ms, track.name, track.track_number))
test_X <- subset(test, select = c(track.id, track.album.release_date, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, track.duration_ms, track.name, track.track_number))

# Multiply the duration column to get duration in minutes
training_X$track.duration_ms <- training_X$track.duration_ms * 0.000016666666666666667
validation_X$track.duration_ms <- validation_X$track.duration_ms * 0.000016666666666666667
test_X$track.duration_ms <- test_X$track.duration_ms * 0.000016666666666666667
## Change the name of duration column
names(training_X)[names(training_X) == "track.duration_ms"] <- "duration_min"
names(validation_X)[names(validation_X) == "track.duration_ms"] <- "duration_min"
names(test_X)[names(test_X) == "track.duration_ms"] <- "duration_min"



## Parsing dates

# Save your current locale
original_locale <- Sys.getlocale(category = "LC_TIME")
# Change it to US
Sys.setlocale(category = "LC_TIME", locale = "English_United States")

# Transform the album_release_date into a posixct format for further manipulations
training_X$track.album.release_date <- as.POSIXct(training_X$track.album.release_date, format = "%Y-%m-%d")
validation_X$track.album.release_date <- as.POSIXct(validation_X$track.album.release_date, format = "%Y-%m-%d")
test_X$track.album.release_date <- as.POSIXct(test_X$track.album.release_date, format = "%Y-%m-%d")

# Change back to the original locale
Sys.setlocale(category = "LC_TIME", locale = original_locale)

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


#####################
## Step 2: Predict ##
#####################

######################################
## Baseline Model: Lasso Regression ##
######################################
train_X <- subset(training_X_encode, select = -c(track.id))
val_X <- subset(validation_X_encode, select = -c(track.id))

library(glmnet)
library(tidyr)
y <- unlist(training_y)
colnamesList = colnames(train_X)
x <- data.matrix(train_X[, colnamesList])

cv_model <- cv.glmnet(x, y, alpha = 1) # 10-fold cross-validation
plot(cv_model)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda #1.783141

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
rsq # 0.2544634

# Calculate RMSE
RMSE <- sqrt(mean((y_predicted - y1)^2))
RMSE # 9.077571

# Predictor variables as data.matrix
test_X <- subset(test_X_encode, select = -c(track.id))

colnamesList2 = colnames(test_X)
x2 <- data.matrix(test_X[, colnamesList2])

# Prediction based on the test set features
y2 <- predict(best_model, s = best_lambda, newx = x2)

### Prediction as a dataframe
y2_df <- data.frame(track.name = test_X$track.name, track.popularity = y2)
colnames(y2_df) <- c('track.name','track.popularity')

## Save y2_df as "prediction_lasso"
save(y2_df, file = "prediction_lasso.RData")

##################
## Linear model ##
##################
lm_model <- lm(training_y ~., data = train_X)
predictions_lm <- predict(lm_model, val_X)
mae_lm <- sum(abs(predictions_lm - validation_y))/nrow(val_X) * std[[10]]
mae_lm

################
## XGBoosting ##
################

library(xgboost)

## Train on training set, predict on validation set
xgb_model_test <- xgboost(data = as.matrix(train_X), label = as.matrix(training_y),
                     max.depth = 4, nthread = 8, nrounds = 200, eval_metric = "mae")

predictions_XGBoost_test <- predict(xgb_model_test, as.matrix(validation_X))
mae_XGBoost_test <- sum(abs(predictions - validation_y))/nrow(validation_X) * std[[10]]
mae_XGBoost_test

## Retrain on training + validation set, predict on test set
xgb_model_full <- xgboost(data = as.matrix(train_val_X), label = as.matrix(train_val_y),
                     max.depth = 4, nthread = 8, nrounds = 200, eval_metric = "mae")

predictions_XGBoost_full <- predict(xgb_model_full, as.matrix(train_val_y))
mae_XGBoost_full <- sum(abs(predictions - train_val_y))/nrow(train_val_X) * std[[10]]
mae_XGBoost_full

###################
## Random Forest ##
###################

# Set training parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "mae"

# Set seed for reproducibility
set.seed(7)

# Determine number of variables to be considered per split
mtry <- sqrt(ncol(train_val_basetable))

# Create tuning grid for random forest
tunegrid <- expand.grid(.mtry=mtry)

# Train random forest model with training and validation data
rf_model <- train(training_y~., data=training_X, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)

# Print training results
print(rf_model)

# Calculate predictions for validation set and mean absolute error
predictions <- predict(rf_model, validation_X)
mae_rf <- sum(abs(predictions - validation_y))/nrow(validation_X) * std[[10]]
mae_rf


##############
## ........ ##
##############
