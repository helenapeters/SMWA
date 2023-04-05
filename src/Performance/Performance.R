###################################################################
####################### PART 1: DESCRIPTIVE #######################
###################################################################

## Clean the environment
rm(list=ls()) 

## Install and load packages
if (!require("pacman")) install.packages("pacman", quiet=TRUE) ; require("pacman")
p_load(rtoot, httpuv, tidyverse, lubridate, rvest)

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

##############
## Boosting ##
##############

##############
## ........ ##
##############
