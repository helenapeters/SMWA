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


##############################################
## Step 4: compute degree for all followers ##
##############################################
## Calculate the degree of each user in the network
degree_of_all <- degree(net, gmode="graph")


##################################################
## Step 5: rank followers based on their degree ##
##################################################

#extract the degree of only our followers
count = 1
ground_truth <- data.frame()
followers_idx <- integer(length = length(seconddegreefollowers$username))

for(i in seconddegreefollowers$username){
  print(i)
  
  followers_idx[count] <- grep(pattern = tolower(toString(i)), x = colnames(A))
  degree <- degree_of_all[ grep(pattern = tolower(toString(i)), x = colnames(A))]
  
  frame = data.frame( user=i, degree = degree)
  ground_truth <- rbind(ground_truth, frame)
  
  count <- count + 1
}

# create rank
ground_truth$ground_truth_rank <- rank(ground_truth$degree)



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

#function that creates adj matrix using N follower-of-followers
compute_adj_matrix <- function(N){
  basic_adj_matrix <- seconddegreefollowers
  
  mm <- do.call("c", lapply(basic_adj_matrix, paste, collapse=" "))
  myCorpus <- Corpus(VectorSource(mm))
  userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))
  B <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)
  
  #substract usernames
  followers_of_followers_names <- colnames(B)
  
  # nieuwe vector vec nodig om uit te sampelen
  vec <- 1:length(followers_of_followers_names)
  vec <- vec[ -followers_idx ]
  
  # sample N random indices out of the remaining list of indices
  random_sample <- sample(vec, size = N)
  ind_sample_adjacency <- c( followers_idx, random_sample)
  
  return( B[ind_sample_adjacency, ind_sample_adjacency] )
}

storage <- data.frame()


for (i in 2:(length(degree_of_all)-length(followers_idx))) {
  print( i )
  # with degree_of_all equal to the number of follower-of-followers
  
  # compute adjacency matrix A using i followers-of-followers
  mat <- compute_adjacency_matrix(i)
  
  # compute the degree for all your followers
  degree_everyone <- degree(network(mat), gmode="graph")
  
  followers_degree <- degree_everyone[1:length(followers_idx)]
  
  current_ranking <- data.frame( 
    user = followers, 
    degree = followers_degree
  )
  current_ranking$rank <- rank(current_ranking$degree)
  
  
  # compute spearman's rank-based correlation
  correlation = cor(ground_truth$ground_truth_rank, current_ranking$rank, method="spearman")
  
  # store both i and correlation
  frame = data.frame( iteration=i, correlation = correlation)
  storage <- rbind(storage, frame)
}



#######################################################
## Step 4: plot follower-of-follower and correlation ##
#######################################################
p_load(tidyverse)

ggplot(data = storage, aes(x = iteration, y = correlation)) +
  geom_point() +
  labs(x="Network Size", y="Spearman's rank-based correlation")

