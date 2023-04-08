##################################################################
############################# PART 1 #############################
##################################################################

## Clean the environment
rm(list=ls()) 

## Install and load packages
if (!require("pacman")) install.packages("pacman", quiet=TRUE) ; require("pacman")
p_load(rtoot, httpuv, tidyverse, lubridate)

## Set working directory
setwd("C:/Users/irisc/OneDrive/Documenten/GitHub/SMWA/src/Targeting")

###########################
## Step 1: get followers ##
###########################

## Set up authentication
## Only need to run this once
auth_setup()

## Look up the account for Orvaline and look at the best match
my_name <- "Orvaline"
orvaline <- search_accounts(my_name)[1, ]

## This results in a account object with all kinds of information
glimpse(orvaline)

## Retrieve all her followers
firstdegree <- get_account_followers(
  orvaline$id,
  limit = orvaline$followers_count,
  retryonratelimit = TRUE
)

## Save the data as "firstdegree"
save(firstdegree, file = "firstdegree.RData")

## Load the data by using the command:
load("firstdegree.RData")

## Access the loaded data using the saved variable name:
firstdegree

########################################
## Step 2: get followers of followers ##
########################################

## Get followers of followers
seconddegreefollowers <- list()
#l <- list()
for (i in 1:nrow(firstdegree)) {
  cat('... Scraping: ', firstdegree$username[i], '\n')
  seconddegreefollowers[[i]] <- get_account_followers(
    firstdegree$id[i], limit = firstdegree$followers_count[i], retryonratelimit = TRUE
  )
  #l[[i]] <- seconddegreefollowers[[i]] %>% pull(username)
}

## Now we have all the followers of followers
## Let's add the first degree followers to that list
seconddegreefollowers[[length(seconddegreefollowers)+1]] <- firstdegree

## Save the data as "firstdegree"
save(seconddegreefollowers, file = "seconddegreefollowers.RData")

## Load the data by using the command:
load("seconddegreefollowers.RData")

## Access the loaded data using the saved variable name:
seconddegreefollowers

# let's extract all the usernames of the followers
followers <- list()
for (i in 1:length(seconddegreefollowers)){
  tryCatch({
    followers[[i]] <- seconddegreefollowers[[i]] %>% pull(username)
  }, error=function(e){})
}
names(followers) <- c(firstdegree$username,orvaline$username)

#let's have a look
glimpse(followers)

#####################################
## Step 3: create adjacency matrix ##
#####################################

## Install and load the text mining package to preprocess the data
p_load(SnowballC, tm, igraph)

## Transform the list to a character vector
## Each element in the vector contains all the followers of a user

mm <- do.call("c", lapply(followers, paste, collapse=" "))

## Transform that vector using the tm package to structure the unstructured data
myCorpus <- Corpus(VectorSource(mm))

## Inspect the result
inspect(myCorpus) # Takes long to run

## This creates a matrix, in which the rows are our followers and the columns are followers of followers
## This thus resembles an incidence matrix
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

## We can also look at the actual matrix
inspect(userfollower) 

## create adjacency matrix
p_load(igraph)
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower) #error: cannot allocate vector of size 58.0 Gb

## matrix A might be too large
if (ncol(A) > 500) A <- A[1:500,1:500]

#make a network object
p_load(statnet)
net <- network(A, matrix.type="adjacency")

## Save the network object as "net"
save(net, file = "net.RData")

## Load the data by using the command:
load("net.RData")

##############################################
## Step 4: compute degree for all followers ##
##############################################

## Calculate the degree of all nodes in the network
degree_of_all <- degree(net, mode = "all")

## Extract the degree of only the followers in the seconddegreefollowers data frame
followers_idx <- match(tolower(seconddegreefollowers$screen_name), tolower(colnames(A)))
fol_degree <- degree_of_all[followers_idx]

##################################################
## Step 5: rank followers based on their degree ##
##################################################

## Create a data frame with the user names, degree, and ground truth rank
ground_truth <- data.frame(user = seconddegree$screen_name, degree = fol_degree)
ground_truth$ground_truth_rank <- rank(fol_degree)

## Save the groundtruth as "ground_truth"
rlist::list.save(ground_truth, file = "ground_truth.RData")

## Load the data by using the command:
ground_truth <- rlist::list.load("ground_truth.RData")

##################################################################
############################# PART 2 #############################
##################################################################

##############################################
## Step 1: get all followers of orvaline    ##
##############################################
load("firstdegree.RData")
firstdegree

##############################################
## Step 2: get all followers of followers   ##
##############################################
load("seconddegreefollowers.RData")
seconddegreefollowers

##############################################
## Step 3: for each follower of follower    ##
##    - compute adj matrix                  ##
##    - compute degree of followers         ##
##    - rank followers based on degree      ##
##    - compute Spearman's correlation      ##
##############################################

## Create data frame that will store the results of the correlation calculations
results <- data.frame(matrix(NA, nrow = (nrow(userfollower) - 1), ncol = 2))
colnames(results) <- c("followers_of_followers", "spearman_correlation")

for (i in 2:(nrow(userfollower) - 1)) {
  # Compute adjacency matrix A using i followers-of-followers
  A <- matrix(nrow = nrow(userfollower), ncol = nrow(userfollower))
  for (j in 1:nrow(userfollower)) {
    followers <- userfollower[j, ]
    for (k in 1:i) {
      followers <- unique(c(followers, unlist(userfollower[followers, ])))
    }
    A[j, followers] <- 1
  }
  
  # Convert A to data frame and set column names
  df <- as.data.frame(A)
  colnames(df) <- names(userfollower)
  
  # Compute the degree for all your followers
  degree <- sna::degree(df)
  
  # Rank all your followers based on their degree and store in current ranking
  current_ranking <- data.frame(vertex_names = names(degree), vertex_degree = degree)
  current_ranking <- current_ranking[order(-current_ranking$vertex_degree), ]
  
  # Compute Spearman's rank-based correlation
  spearman_correlation <- cor(groundtruth_df_users$vertex_degree, current_ranking$vertex_degree, method = "spearman")
  
  # Store both i and correlation
  results[i - 1, 1] <- i
  results[i - 1, 2] <- spearman_correlation
}

#######################################################
## Step 4: plot follower-of-follower and correlation ##
#######################################################

plot(results$followers_of_followers, results$spearman_correlation, type = "l",
     xlab = "Size of network used to compute degree", ylab = "Rank-based correlation")

