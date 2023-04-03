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

seconddegree <- list()
for (i in 1:length(firstdegree)) ##nrow instead of length?
  seconddegree[[i]] <- search_accounts(firstdegree$username[i])[1, ]
seconddegree <- bind_rows(seconddegree)

## This should be 374:
nrow(seconddegree)  ##not correct!

## Now extract the followers-of-followers
seconddegreefollowers <- list()
for (i in 1:nrow(seconddegree)) {
  cat('... Scraping: ', seconddegree$username[i], '\n')
  seconddegreefollowers[[i]] <- get_account_followers(
    seconddegree$id[i], limit = seconddegree$followers_count[i], retryonratelimit = TRUE
  )
}

## Now we have all the followers of followers
## Let's add the first degree followers to that list
seconddegreefollowers[[length(seconddegreefollowers)+1]] <- firstdegree

## Let's extract all the usernames of the followers
followers <- list()
for (i in 1:length(seconddegreefollowers))
  followers[[i]] <- seconddegreefollowers[[i]] %>% pull(username)  #Fout bij username
names(followers) <- c(seconddegree$username,orvaline$username)

## Let's have a look
glimpse(followers)

## Transform that list to a character vector of length 5
## Each element in the vector contains all the followers of a user

mm <- do.call("c", lapply(followers, paste, collapse=" "))

#####################################
## Step 3: create adjacency matrix ##
#####################################

## Install and load the text mining package to preprocess the data
p_load(SnowballC, tm, igraph)

## Transform that vector using the tm package to structure the unstructured data
myCorpus <- Corpus(VectorSource(mm))

## Inspect the result
inspect(myCorpus)

## This creates a matrix, in which the rows are our sources of interest and the columns are the friends
## This thus resembles an incidence matrix
userfollower <- DocumentTermMatrix(myCorpus, control = list(wordLengths = c(0, Inf)))

## We can also look at the actual matrix
inspect(userfollower)

## Compute the adjacency matrix using matrix multiplication.
A <- t(as.matrix(userfollower)) %*% as.matrix(userfollower)

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

##################################################
## Step 5: rank followers based on their degree ##
##################################################

##################################################################
############################# PART 2 #############################
##################################################################
