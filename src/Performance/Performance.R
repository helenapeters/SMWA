###################################################################
####################### PART 1: DESCRIPTIVE #######################
###################################################################

## Clean the environment
rm(list=ls()) 

## Install and load packages
if (!require("pacman")) install.packages("pacman", quiet=TRUE) ; require("pacman")
p_load(rtoot, httpuv, tidyverse, lubridate, rvest, caret)

## Set working directory
setwd("C:/Users/Helena/Documents/eerste_master/SMWA/Project_test/data_scrape")

######################
## Step 1: Scraping ##
######################

##############
## Mastodon ##
##############

## Set up authentication
## Only need to run this once
#auth_setup()

## Search for 50 toots using the amelielens hashtag
toots <- get_timeline_hashtag("#amelielens", limit = 50, retryonratelimit = TRUE) ##Only 6?

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

#################################
## Step 1: Feature engineering ##
#################################

#########################################
## Step 2: Text and sentiment analysis ##
#########################################

#####################
## Step 3: Predict ##
#####################

## Read data

## Normalize the data

## Data splitting

## Basetable

train_val_basetable <- 

##################
## Linear model ##
##################

lm_model <- lm(sentiment ~., data = train_val_basetable)
predictions_lm <- predict(lm_model, X_test)
mae_lm <- sum(abs(predictions - Y_test))/nrow(X_test) * std[[10]]
mae_lm

################
## XGBoosting ##
################

library(xgboost)

## Train on training set, predict on validation set
xgb_model_test <- xgboost(data = as.matrix(X_train), label = as.matrix(Y_train),
                     max.depth = 4, nthread = 8, nrounds = 200, eval_metric = "mae")

predictions_XGBoost_test <- predict(xgb_model_test, as.matrix(X_val))
mae_XGBoost_test <- sum(abs(predictions - Y_val))/nrow(X_val) * std[[10]]
mae_XGBoost_test

## Retrain on training + validation set, predict on test set
xgb_model_full <- xgboost(data = as.matrix(X_train_val), label = as.matrix(Y_train_val),
                     max.depth = 4, nthread = 8, nrounds = 200, eval_metric = "mae")

predictions_XGBoost_full <- predict(xgb_model_full, as.matrix(Y_test))
mae_XGBoost_full <- sum(abs(predictions - Y_test))/nrow(X_test) * std[[10]]
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
rf_model <- train(sentiment~., data=train_val_basetable, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)

# Print training results
print(rf_model)

# Calculate predictions for test set and mean absolute error
predictions <- predict(rf_model, X_test)
mae_rf <- sum(abs(predictions - Y_test))/nrow(X_test) * std[[10]]
mae_rf


##############
## ........ ##
##############
