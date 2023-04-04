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

## Save the data as"network" so we don't have to scrape anymore.
rlist::list.save(seconddegreefollowers, file = "network.RData") 

## If you wish to load the data use the command
#rlist::list.load("netwerk.RData") so eg:
seconddegreefollowers <- rlist::list.load("network.RData")


## Transform that list to a character vector of length 5
## Each element in the vector contains all the followers of a user

mm <- do.call("c", lapply(seconddegreefollowers, paste, collapse=" "))

#####################################
## Step 3: create adjacency matrix ##
#####################################

## Install and load the text mining package to preprocess the data
p_load(SnowballC, tm, igraph)

## Transform that vector using the tm package to structure the unstructured data
myCorpus <- Corpus(VectorSource(mm))

## Inspect the result
inspect(myCorpus)

## This creates a matrix, in which the rows are our followers and the columns are followers of followers
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
