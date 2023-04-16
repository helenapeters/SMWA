#scraping the comments from specific youtube videos
if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(tidyverse,SnowballC, slam, tm,tictoc,qdap)
p_load(udpipe,textplot,ggraph,tidytext,wordcloud,wordcloud2)
p_load(httr,rtweet,tidyverse,textclean,textstem,sentimentr,lexicon,textcat)
install.packages("tuber")
install.packages('https')
library(tuber)

myclientid='24403699099-gvcon4ph9qbvotffogimd6fg932m1t6f.apps.googleusercontent.com'
clientsecret='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'
yt_oauth(myclientid,clientsecret,token="")

LuxAeterna=get_all_comments('1OeC9CGtWcM')
load('SeventyTwoSeasons.RData')
lastcommentseng2 <- head(LuxAeterna,2000)
lastcommentseng2 <- head(IfDarknessHadASon,2000)
LuxAeterna2 <- head(LuxAeterna,2000)

#get a list of videos from a specific channel
a <- list_channel_resources(filter = c(channel_id = "UCbulh9WdLtEXiooRcYK7SWw"), part="contentDetails")
# Uploaded playlists:
playlist_id <- a$items[[1]]$contentDetails$relatedPlaylists$uploads

# Get videos on the playlist
vids <- get_playlist_items(filter= c(playlist_id=playlist_id)) 

# Video ids
vid_ids <- as.vector(vids$contentDetails.videoId)

# Function to scrape stats for all vids
get_all_stats <- function(id) {
  get_stats(id)
} 

# Get stats and convert results to data frame 
res <- lapply(vid_ids, get_all_stats)
res_df <- do.call(rbind, lapply(res, data.frame))

head(res_df)

# Load the required libraries
library(tm)
library(SnowballC)




text <- LuxAeterna
text <- text %>% 
  pull(textOriginal) %>% 
  str_replace_all("<.*?>", "")
lang <- textcat(text)
text <- text[lang == "english"]

#First, use the textclean package to detect emojis and emoticons
#Not all emojis will be detected, so we will later have to delete the remaining ones

text_clean <- text %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_contraction() %>%
  replace_internet_slang() %>% 
  replace_kern() %>% 
  replace_word_elongation()

#Clean the rest of the posts
cleanText <- function(text) {
  clean_texts <- text %>%
    str_replace_all("<.*>", "") %>% # remove remainig emojis
    str_replace_all("&amp;", "") %>% # remove &
    str_replace_all("(RT|via)((?:\\b\\W*@\\w+)+)", "") %>% # remove retweet entities
    str_replace_all("@\\w+", "") %>% # remove at people replace_at() also works
    #str_replace_all("(?:\\s*#\\w+)+\\s*$", "") %>% #remove hashtags in total
    str_replace_all('#', "") %>% #remove only hashtag 
    str_replace_all("[[:punct:]]", "") %>% # remove punctuation
    str_replace_all("[[:digit:]]", "") %>% # remove digits
    str_replace_all("http\\w+", "") %>% # remove html links replace_html() also works
    str_replace_all("[ \t]{2,}", " ") %>% # remove unnecessary spaces
    str_replace_all("^\\s+|\\s+$", "") %>% # remove unnecessary spaces
    str_to_lower()
  return(clean_texts)
}

text_clean_all <- cleanText(text_clean)

#Finally, apply lemmatization with the textstem package
#First, you create a dictionary from the text
#For large corpora you can use built-in dictionaries
lemma_dictionary_hs <- make_lemma_dictionary(text_clean_all,
                                             engine = 'hunspell')
text_final <- lemmatize_strings(text_clean_all, 
                                dictionary = lemma_dictionary_hs)

#################
### WORDCLOUD ###
#################

# Create a document term matrix
text_df <- tibble(doc= 1:length(text_clean_all), 
                  text = text_clean_all)

#Next, make a word frequency table
freq <- text_df %>%
  unnest_tokens(word, text) %>% #tokenize (split documents into terms)
  anti_join(tibble(word = stopwords('en'),
                   lexixon = 'tm')) %>% #Remove stopwords dutch stopworks
  anti_join(stop_words) %>% #Remove also the english ones
  count(doc,word, name = "freq", sort = TRUE)

#Cast dtm from this word count table
dtm <- freq %>%
  cast_dtm(doc, word, freq)
# Print the top 10 terms
head(freq, 10)

# load libraries
library(tm)

# import DTM data

# convert DTM to matrix and transpose
dtm_matrix <- as.matrix(dtm)
tdm_matrix <- t(dtm_matrix)

v <- sort(rowSums(tdm_matrix),decreasing=TRUE)
d <- tibble(word = names(v),freq=v) #or: data.frame(word = names(v),freq=v) 
d <- na.omit(d)

#using the dplyr to delete the meaningless names
p_load(dplyr)
library(dplyr)
d <- d %>% filter(!(word %in% c("metallica", "song",'album','the','this','and','null','NA')))

wordcloud2(d)

#################
### WORDGRAPH ###
#################

#Make a function to create the adjacency matrix
create_adjacency_matrix <- function(object, probs=0.99){
  
  #object= output from function create_document_term_matrix (a document by term matrix)
  #probs= select only vertexes with degree greater than or equal to quantile given by the value of probs
  
  cat("Create adjacency matrix \n")
  #create adjacency matrix
  p_load(sna)
  
  mat <- as.matrix(object)
  mat[mat >= 1] <- 1 #change to boolean (adjacency) matrix
  Z <- t(mat) %*% mat
  
  cat("Apply filtering \n")
  ind <- sna::degree(as.matrix(Z),cmode = "indegree") >= quantile(sna::degree(as.matrix(Z),cmode = "indegree"),probs=0.99)
  #ind <- sna::betweenness(as.matrix(Z)) >= quantile(sna::betweenness(as.matrix(Z)),probs=0.99)
  
  
  Z <- Z[ind,ind]        
  
  cat("Resulting adjacency matrix has ",ncol(Z)," rows and columns \n")
  dim(Z)
  list(Z=Z,termbydocmat=object,ind=ind)
}

adj_mat <- create_adjacency_matrix(dtm)
#Look at the result
adj_mat[[1]][1:5,1:5]

set.seed(1)

#Again make a function to make plotting easier
plot_network <- function(object){
  #object: output from the create_adjacency_matrix function
  
  #create graph from adjacency matrix
  p_load(igraph)
  g <- graph.adjacency(object$Z, weighted=TRUE, mode ='undirected')
  g <- simplify(g)
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- igraph::degree(g)
  
  layout <- layout.auto(g)
  opar <- par()$mar; par(mar=rep(0, 4)) #Give the graph lots of room
  #adjust the widths of the edges and add distance measure labels
  #use 1 - binary (?dist) a proportion distance of two vectors
  #The binary distance (or Jaccard distance) measures the dissimilarity
  #so 1 is perfect and 0 is no overlap (using 1 - binary)
  edge.weight <- 7  #a maximizing thickness constant
  z1 <- edge.weight*(1-dist(t(object$termbydocmat)[object$ind,], 
                            method="binary"))
  E(g)$width <- c(z1)[c(z1) != 0] #remove 0s: these won't have an edge
  clusters <- spinglass.community(g)
  cat("Clusters found: ", length(clusters$csize),"\n")
  cat("Modularity: ", clusters$modularity,"\n")
  plot(g, layout=layout, vertex.color=rainbow(4)[clusters$membership],
       vertex.frame.color=rainbow(4)[clusters$membership] )
}

plot_network(adj_mat)

#######################
##   topicmodelling  ##
#######################

p_load(wordcloud, tm, topicmodels, topicdoc, tidytext, textclean)

ldas <- list()
j <- 0
for (i in 2:10) {
  j <- j+1
  print(i)
  #We set a seed for the LDA algorithm such that the results are predictable and comparable
  #This uses the VEM optimization algorithm as defined by the inventor (Blei)
  #You can also choose to perform Gibbs sampling (method option)
  ldas[[j]] <- LDA(x = dtm, k = i, control = list(seed = 1234))
}

#Test the performance using the AIC (smaller is better)
(AICs <- data.frame(k = 2:10, 
                    aic = map_dbl(ldas, AIC)))
(K <- AICs$k[which.min(AICs$aic)])

topicmodel <- LDA(x = dtm, k = K, control = list(seed = 1234))

(topic_term <- tidy(topicmodel, matrix = 'beta'))
topic_term <- na.omit(top_terms)
top_terms <- topic_term %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, desc(beta))
top_terms

#Plot the top terms per topic
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Make a worldcloud per topic
p_load(reshape2, BiocManager, RColorBrewer)

topic_term %>%
  group_by(topic) %>%
  top_n(50, beta) %>%
  acast(term ~ topic, value.var = "beta", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(3, "Set2"))

##################################
#####      wordembedding     #####
##################################


p_load(word2vec,text2vec,Rtsne,scales,ggrepel,tidyverse,tm)


reviews <- str_to_lower(text_clean_all)

set.seed(1234)
model <- word2vec(x = reviews, 
                  type = "skip-gram", 
                  dim = 15, 
                  iter = 50, 
                  window = 5, 
                  stopwords = stopwords())

embedding <- as.matrix(model)
head(embedding)

lookslike <- predict(model, c("album", "metallica"), type = "nearest", top_n = 5)
lookslike

wv <- predict(model, newdata = c("album", "metallica", "song"), type = "embedding")
wv <- wv["album", ] - wv["metallica", ] + wv["song", ]
predict(model, newdata = wv, type = "nearest", top_n = 3)


#####################LDA######################

corpus1 <- Corpus(VectorSource(lastcommentseng3$textOriginal))
dtm1 <- DocumentTermMatrix(corpus1)
inspect(dtm1)

lda_basic <- LDA(dtm1, 
                 control = list(seed = 33), 
                 k = 5)

terms(lda_basic, 20)

######################################
#####      Sentimentanalysis     #####
######################################

p_load(httr,rtweet,tidyverse,textclean,textstem,sentimentr,lexicon,textcat)


#Extract sentiment
sentiment <- text_final %>% 
  get_sentences() %>% 
  sentiment_by()

#Check the commentss and sentiment score
sentiment %>% highlight()

#read in dictionary
#read_csv is an optimized read.csv function from the tidyverse
dictionary <- read_csv("dictionary.csv")
#Let's have a look
dictionary
#VALENCE: 1= sad, 9= happy
#AROUSAL: 1=calm, 9=excited
#DOMINANCE: 1=controlled, 9=in control

#Now recode all columns so that neutral equals 0, -4= negative, 4=positive
dictionary <- dictionary %>% 
  mutate(across(where(is.numeric),function(x) x-5))

#Let's have a look at the dictionary
dictionary
p_load(skimr)
skim(dictionary)
text1 <- LuxAeterna %>% 
  pull(textOriginal) %>% 
  str_replace_all("<.*?>", "")
created <- LuxAeterna %>% pull(publishedAt)

#Focus on English
p_load(textcat)
lang <- textcat(text1)
text1 <- text1[lang == "english"]
created <- created[lang == "english"]


#Also delete the ones for which we don't have a time stamp
text1 <- text1[!is.na(created)]


scorecomments <- numeric(length(text1))


for (i in 1:length(text1)){
  
  #transform everything to lower case
  text1 <- str_to_lower(text1)
  
  #Split up the comments in words
  commentssplit <- str_split(text1[i]," ")[[1]] 
  
  #find the positions of the words in the comments in the dictionary
  m <- match(commentssplit, dictionary$Word)
  
  #which words are present in the dictionary?
  present <- !is.na(m)
  #commentssplit[present]
  
  #of the words that are present, select their valence
  wordvalences <- dictionary$VALENCE[m[present]]
  
  #compute the mean valence of the comments
  scorecomments[i] <- mean(wordvalences, na.rm=TRUE)
  
  #handle the case when none of the words is in the dictionary
  if (is.na(scorecomments[i])) scorecomments[i] <- 0 else scorecomments[i] <- scorecomments[i]
  
}

head(scorecomments)
mean(scorecomments)
sd(scorecomments)
hist(scorecomments)

#lets look at the sentiment score and the comments content
bind_cols(scorecomments,text1) %>% View()


#Group in minutes and take the average per minute
#handle time zone
p_load(lubridate)
#format is "%Y-%m-%d %H:%M:%S", so use ymd_hms option of lubridate
#more info on https://lubridate.tidyverse.org/ 
time <- ymd_hms(created, format="%Y-%m-%d %H:%M:%S",tz="UTC")
attributes(time)$tzone <- "CET"
#Look at the result and compare with original
head(created)
head(time)
#Remove the trailing NA value
time <- na.omit(time)

#get minutes and hour for commentss
breaksday <- date(time)
breakshour <- hour(time)
breaksmin <- minute(time)
head(breaksday)
#Compute mean per hour and minute
scores <- tibble(scorecomments,
                 dayhour =  paste(breaksday, sep = ""))
scores <- scores %>% 
  group_by(dayhour) %>% 
  summarise(sentiment=mean(scorecomments))

lim <- max(abs(scores$sentiment))

#Plot sentiment by time
ggplot(scores, aes(y = sentiment, x = dayhour, group = 1)) +
  geom_line() +
  geom_hline(yintercept = 0, col = "red") +
  ylim(c(-lim,lim)) +
  labs( y = "sentiments", x = "Time (date)",
        title = "Sentiment per day and hour"
  )

