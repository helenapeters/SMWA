##################################################################
############################# PART 1 #############################
##################################################################

## Clean the environment
rm(list=ls()) 

## Install and load packages
if (!require("pacman")) install.packages("pacman", quiet=TRUE) ; require("pacman")
p_load(rtoot, httpuv, tidyverse, lubridate)

## Set working directory
setwd("C:/Users/Helena/Documents/eerste_master/SMWA/Project_test/data_scrape")

###########################
## Step 1: get followers ##
###########################

## Set up authentication
## Only need to run this once
#auth_setup()

# Look up the account for Orvaline and look at the best match
my_name <- "Orvaline"
orvaline <- search_accounts(my_name)[1, ]

# This results in a account object with all kinds of information
glimpse(orvaline)

# Retrieve all her followers
firstdegree <- get_account_followers(
  orvaline$id,
  limit = orvaline$followers_count,
  retryonratelimit = TRUE
)

########################################
## Step 2: get followers of followers ##
########################################

seconddegree <- list()
for (i in 1:length(firstdegree)) ##nrow instead of length?
  seconddegree[[i]] <- search_accounts(firstdegree$username[i])[1, ]
seconddegree <- bind_rows(seconddegree)

#this should be 374:
nrow(seconddegree)  ##not correct!

# Now extract the followers-of-followers
seconddegreefollowers <- list()
for (i in 1:nrow(seconddegree)) {
  cat('... Scraping: ', seconddegree$username[i], '\n')
  seconddegreefollowers[[i]] <- get_account_followers(
    seconddegree$id[i], limit = seconddegree$followers_count[i], retryonratelimit = TRUE
  )
}

#####################################
## Step 3: create adjacency matrix ##
#####################################

##############################################
## Step 4: compute degree for all followers ##
##############################################

##################################################
## Step 5: rank followers based on their degree ##
##################################################

##################################################################
############################# PART 2 #############################
##################################################################
