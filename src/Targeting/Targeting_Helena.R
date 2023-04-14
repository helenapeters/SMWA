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

## Look up the account for Dirk Van den Poel and look at the best match
my_name <- "Dirk Van den Poel"
dirk <- search_accounts(my_name)[1, ]

## This results in a account object with all kinds of information
glimpse(dirk)

# ## Retrieve all his followers
# firstdegree <- get_account_followers(
#   dirk$id,
#   limit = dirk$followers_count,
#   retryonratelimit = TRUE
# )
# 
# ## Save the data as "firstdegree"
# save(firstdegree, file = "firstdegree.RData")

## Load the data by using the command:
load("firstdegree.RData")

## Access the loaded data using the saved variable name:
firstdegree

########################################
## Step 2: get followers of followers ##
########################################

# ## Get followers of followers
# seconddegreefollowers <- list()
# #l <- list()
# for (i in 1:nrow(firstdegree)) {
#   cat('... Scraping: ', firstdegree$username[i], '\n')
#   seconddegreefollowers[[i]] <- get_account_followers(
#     firstdegree$id[i], limit = firstdegree$followers_count[i], retryonratelimit = TRUE
#   )
#   #l[[i]] <- seconddegreefollowers[[i]] %>% pull(username)
# }
# 
# ## Now we have all the followers of followers
# ## Let's add the first degree followers to that list
# seconddegreefollowers[[length(seconddegreefollowers)+1]] <- firstdegree
# 
# ## Save the data as "seconddegreefollowers"
# save(seconddegreefollowers, file = "seconddegreefollowers.RData")

## Load the data by using the command:
load("seconddegreefollowers.RData")

## Access the loaded data using the saved variable name:
#seconddegreefollowers

# let's extract all the usernames of the followers
followers <- list()
for (i in 1:length(seconddegreefollowers)){
  tryCatch({
    followers[[i]] <- seconddegreefollowers[[i]] %>% pull(username)
  }, error=function(e){})
}
names(followers) <- c(firstdegree$username,dirk$username)

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
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower) 

## matrix A might be too large
#if (ncol(A) > 500) A <- A[1:500,1:500]

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
degree_of_all <- degree(net, gmode = "graph")

## Extract the degree of only Orvaline her followers
followers_idx <- match(tolower(firstdegree$username), tolower(colnames(A)))
fol_degree <- degree_of_all[followers_idx] #has a lot of NA values

##################################################
## Step 5: rank followers based on their degree ##
##################################################

## Create a data frame with the user names, degree, and ground truth rank
ground_truth <- data.frame(user = firstdegree$username, degree = fol_degree)
ground_truth$ground_truth_rank <- rank(fol_degree)

## Save the groundtruth as "ground_truth"
rlist::list.save(ground_truth, file = "ground_truth.RData")

## Load the data by using the command:
#ground_truth <- rlist::list.load("ground_truth.RData")

##################################################################
############################# PART 2 #############################
##################################################################

#######################################
## Step 1: get all followers of dirk ##
#######################################
load("firstdegree.RData")
#firstdegree

##############################################
## Step 2: get all followers of followers   ##
##############################################
load("seconddegreefollowers.RData")
#seconddegreefollowers

##############################################
## Step 3: for each follower of follower    ##
##    - compute adj matrix                  ##
##    - compute degree of followers         ##
##    - rank followers based on degree      ##
##    - compute Spearman's correlation      ##
##############################################

# ## This function generates an adjacency matrix based on N followers-of-followers
# compute_adj_matrix <- function(N){
#   basic_adj_matrix <- followers
# 
#   mm <- do.call("c", lapply(basic_adj_matrix, paste, collapse=" "))
#   myCorpus <- Corpus(VectorSource(mm))
#   userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))
#   B <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)
# 
#   # Subtract usernames by assigning the column names of matrix B to the variable "followers_of_followers_names"
#   followers_of_followers_names <- colnames(B)
# 
#   # Create a new vector for sampling by setting "vec" to a sequence of integers from 1 to the length of "followers_of_followers_names"
#   vec <- 1:length(followers_of_followers_names)
# 
#   # Sample N random indices from the remaining indices in the "vec" vector and store them in "random_sample"
#   random_sample <- sample(vec, size = N)
#   ind_sample_adjacency <- c( followers_idx, random_sample)
# 
#   return( B[ind_sample_adjacency, ind_sample_adjacency] )
# }
# 
# storage <- data.frame()
# 
# 
# for (i in 2:length(degree_of_all)) {
#   print( i )
#   # "degree_of_all" corresponds to the number of follower-of-followers in matrix A.
# 
#   # Compute the adjacency matrix A using i levels of followers-of-followers
#   mat <- compute_adj_matrix(i)
# 
#   # Calculate the degree of every follower
#   degree_everyone <- degree(network(mat), gmode="graph")
# 
#   followers_degree <- degree_everyone[1:length(followers_idx)]
# 
#   current_ranking <- data.frame(
#     user = followers_idx,
#     degree = followers_degree
#   )
# 
#   current_ranking$rank <- rank(current_ranking$degree)
# 
#   # Calculate the Spearman's rank-based correlation coefficient between the ground truth ranking and the current ranking
#   correlation = cor(ground_truth$ground_truth_rank, current_ranking$rank, method="spearman")
# 
#   # Create a data frame called "frame" with two columns: "iteration" and "correlation"
#   frame = data.frame( iteration=i, correlation = correlation)
# 
#   storage <- rbind(storage, frame)
# }
# 
# ## Save the data as "storage"
# save(storage, file = "storage.RData")

## Load the data by using the command:
load("storage.RData")

## Access the loaded data using the saved variable name:
#storage

#######################################################
## Step 4: plot follower-of-follower and correlation ##
#######################################################

p_load(tidyverse)

plot <- ggplot(data = storage, aes(x = iteration, y = correlation)) +
  geom_point() +
  labs(x="Network Size", y="Spearman's rank-based correlation")

############### Plot network ###############

## Create a graph object based on the adjacency matrix & remove loop edges
g <- graph.adjacency(A, weighted=TRUE,
                     mode ='undirected') %>% simplify()

## Set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- igraph::degree(g)
## Plot the Graph
set.seed(3952)
## Prepare graph
layout <- layout.auto(g)
## Give the graph lots of room

mar <- par()$mar ## Store this for later
par(mar=rep(0, 4))
plot(g, layout=layout, vertex.label=NA,
     edge.curved=TRUE,vertex.size=3,
     vertex.color=c("green","red","blue")[ifelse(V(g)$name %in%
                                                   names(igraph::degree(g)[tail(order(igraph::degree(g)),5)]) ==TRUE,1,
                                                 ifelse(V(g)$name %in%
                                                          names(igraph::degree(g)[tail(order(igraph::betweenness(g)),10)]) ==TRUE,2,3))])

# The top 5 vertices based on degree are in green
# The top 10 vertices based on betweenness (and not based on degree) are in red
# All the other vertices are in blue


