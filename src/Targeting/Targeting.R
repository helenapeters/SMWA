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

########################################
## Step 2: get followers of followers ##
########################################

## Get followers of followers
seconddegreefollowers <- list()
for (i in 1:nrow(firstdegree)) {
  cat('... Scraping: ', firstdegree$username[i], '\n')
  seconddegreefollowers[[i]] <- get_account_followers(
    firstdegree$id[i], limit = firstdegree$followers_count[i], retryonratelimit = TRUE
  )
}

## Now we have all the followers of followers
## Let's add the first degree followers to that list
seconddegreefollowers[[length(seconddegreefollowers)+1]] <- firstdegree

## Save the data as "network"
rlist::list.save(seconddegreefollowers, file = "network.RData") # It takes too long to scrape everytime

## Load the data by using the command:
seconddegreefollowers <- rlist::list.load("network.RData")

#####################################
## Step 3: create adjacency matrix ##
#####################################

## Install and load the text mining package to preprocess the data
p_load(SnowballC, tm, igraph)

## Transform the list to a character vector
## Each element in the vector contains all the followers of a user

mm <- do.call("c", lapply(seconddegreefollowers, paste, collapse=" "))

## Transform that vector using the tm package to structure the unstructured data
myCorpus <- Corpus(VectorSource(mm))

## Inspect the result
#inspect(myCorpus) # Takes long to run

## This creates a matrix, in which the rows are our followers and the columns are followers of followers
## This thus resembles an incidence matrix
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

## We can also look at the actual matrix
inspect(userfollower)

## Compute the adjacency matrix using matrix multiplication.
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower) #Error: cannot allocate vector of size 96.3 Gb

## Look at its dimensions
dim(A)

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


##############################################
## Step 4: compute degree for all followers ##
##############################################

## Sort network based on degree
groundtruth_sorted <- V(g)[order(V(g)$degree, decreasing = T)]

## Make dataframe
groundtruth_df <- data.frame(vertex_names = groundtruth_sorted$label, vertex_degree = groundtruth_sorted$degree)

## Save the groundtruth as "groundtruth_df"
rlist::list.save(groundtruth_df, file = "groundtruth_df.RData")

## Load the data by using the command:
groundtruth_df <- rlist::list.load("groundtruth_df.RData")

## Create an adjacency matrix with 1/0 links between rows
adj_matrix <- matrix(nrow = nrow(userfollower), ncol = nrow(userfollower))

## Set the elements of the adjacency matrix based on whether the column user follows the row user
for (i in 1:nrow(adj_matrix)) {
  row_user <- names(userfollower)[i]
  for (j in 1:ncol(adj_matrix)) {
    col_user <- names(userfollower)[j]
    adj_matrix[i, j] <- as.numeric(col_user %in% userfollower[[row_user]])
  }
}

## Convert the adjacency matrix to a data frame and set the row and column names
df <- data.frame(adj_matrix)
colnames(df) <- rownames(df) <- names(userfollower)

## Calculate the degree of each user in the network
degree <- rowSums(df)

##################################################
## Step 5: rank followers based on their degree ##
##################################################

## Create a data frame with the user names and their degrees, sorted by degree in decreasing order
groundtruth_df_users <- data.frame(vertex_names = rownames(df), vertex_degree = degree)
groundtruth_df_users <- groundtruth_df_users[order(-degree),]

## Rename the columns in the data frame
colnames(groundtruth_df_users) <- c('vertex_names', 'vertex_degree')

## Save the groundtruth as "groundtruth_df_users"
rlist::list.save(groundtruth_df_users, file = "groundtruth_df_users.RData")

##################################################################
############################# PART 2 #############################
##################################################################
