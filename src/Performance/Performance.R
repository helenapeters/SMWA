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

## Search for 50 toots using the amelielens hashtag
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

# ## Save the data as "metallica2"
# save(metallica2, file = "metallica2.RData")
# 
# ## Save the data as "metallica3"
# save(metallica3, file = "metallica3.RData")

## Load the data by using the command:
#load("metallica2.RData")

## Load the data by using the command:
#load("metallica3.RData")

## Load the data by using the command:
load("metallicafull.RData")

################## Merge data ####################

### Delete duplicates
#metallica_no_popularity <- metallica2[!duplicated(metallica2$track_name), ]

## Change the name of track.id column
#names(metallica3)[names(metallica3) == "track.id"] <- "track_id"

## Create the full dataframe
#metallica_full <- merge(x = metallica_no_popularity, y = metallica3, by= 'track_id')

## Create Basetable
train_val_basetable <- Metallicafull

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

#####Still need to add test set!!!!!!!!!!

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

## Take subset -- Mss trackid nog verwijderen aangezien we al trackname hebben
training_X <- subset(training_X, select = c(track.id, track.album.release_date, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, track.duration_ms, track.name, track.track_number))
validation_X <- subset(validation_X, select = c(track.id, track.album.release_date, danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, track.duration_ms, track.name, track.track_number))


# Multiply the duration column to get duration in minutes
training_X$track.duration_ms <- training_X$track.duration_ms * 0.000016666666666666667
validation_X$track.duration_ms <- validation_X$track.duration_ms * 0.000016666666666666667
## Change the name of duration column
names(training_X)[names(training_X) == "track.duration_ms"] <- "duration_min"
names(validation_X)[names(validation_X) == "track.duration_ms"] <- "duration_min"


#----------------Tot hier runt het-----------------------------------------------------



## Parsing dates   ----------Klopt nog niet-------------

# Save your current locale
original_locale <- Sys.getlocale(category = "LC_TIME")
# Change it to US
Sys.setlocale(category = "LC_TIME", locale = "English_United States")

# Transform the album_release_date into a posixct format for further manipulations
training_X$album_release_date <- as.POSIXct(training_X$album_release_date, format = "%B %e %Y")
validation_X$album_release_date <- as.POSIXct(validation_X$album_release_date, format = "%B %e %Y")
#test_X$album_release_date <- as.POSIXct(test_X$album_release_date, format = "%B %e %Y")

# Change back to the original locale
Sys.setlocale(category = "LC_TIME", locale = original_locale)

#################################
## Step 1: Feature engineering ##
#################################

###################### Encoding of categorical data ######################

## One-hot encoding
library(dummy)

#
names(training_X)[names(training_X) == "mode"] <- "mode_major"
names(validation_X)[names(validation_X) == "mode"] <- "mode_major"
training_X$mode_major <- as.character(training_X$mode_major)
validation_X$mode_major <- as.character(validation_X$mode_major)

# Here are the categorical variables
#cols_to_encode <- c("key")



# We retain all the possible categories
training_X$key <- as.character(training_X$key)
cats <- categories(training_X["key"])

# apply on train set and exclude reference categories  ----- reference category: album Load and key 4 (E)
dummy_train <- dummy(training_X["key"], object = cats)
dummy_train <- subset(dummy_train, select = -c(key_4))
# apply on validation set and exclude reference categories
dummy_val <- dummy(validation_X["key"], object = cats)
dummy_val <- subset(dummy_val, select = -c(key_4))
# apply on test set and exclude reference categories
#dummy_test <- dummy(test_X[, cols_to_encode], object = cats)
#dummy_test <- subset(Load, major, E major, FALSE))

# merge with overall training set
training_X_encode <- subset(training_X, select = -c(key))
training_X_encode <- cbind(training_X_encode, dummy_train)
# merge with overall validation set
validation_X_encode <- subset(validation_X, select = -c(key))
validation_X_encode <- cbind(validation_X_encode, dummy_val)
# merge with overall test set
#test_X <- subset(test_X, select = -c(album_name, mode_name, key_mode, explicit))
#test_X <- cbind(test_X, dummy_test)


#####################
## Step 2: Predict ##
#####################


##################
## Linear model ##
##################
train_X <- subset(training_X_encode, select = -c(track.id, track.name))
val_X <- subset(validation_X_encode, select = -c(track.id, track.name))

lm_model <- lm(training_y ~., data = train_X)
predictions_lm <- predict(lm_model, val_X)
mae_lm <- sum(abs(predictions_lm - validation_y))/nrow(val_X) * std[[10]]
mae_lm

################
## XGBoosting ##
################

library(xgboost)

## Train on training set, predict on validation set
xgb_model_test <- xgboost(data = as.matrix(training_X), label = as.matrix(training_y),
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
