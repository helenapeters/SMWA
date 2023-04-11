# Sentiment Analysis If Darkness Had A Son

#===========LEXICON BASED APPROACH=========================



# Load original dictionary
dictionary <- read.csv("data/dictionary.csv")

#VALENCE: 1= sad, 9= happy
#AROUSAL: 1=calm, 9=excited
#DOMINANCE: 1=controlled, 9=in control

#Now recode all columns so that neutral equals 0, -4= negative, 4=positive
dictionary <- dictionary %>% 
  mutate(across(where(is.numeric),function(x) x-5))


#Pull the text and the date of creation
text <- IfDarknessHadASon %>% 
  pull(textOriginal) %>% 
  str_replace_all("<.*?>", "")
created <- IfDarknessHadASon %>% pull(updatedAt)



#determine sentiment
#Focus on English
p_load(textcat)
lang <- textcat(text)
text <- text[lang == "english"]
created <- created[lang == "english"]

#Also delete the ones for which we don't have a time stamp
text <- text[!is.na(created)]


scoretweet <- numeric(length(text))

for (i in 1:length(text)){
  
  #transform everything to lower case
  text <- str_to_lower(text)
  
  #Split up the tweet in words
  tweetsplit <- str_split(text[i]," ")[[1]] 
  
  #find the positions of the words in the Tweet in the dictionary
  m <- match(tweetsplit, dictionary$Word)
  
  #which words are present in the dictionary?
  present <- !is.na(m)
  #tweetsplit[present]
  
  #of the words that are present, select their valence
  wordvalences <- dictionary$VALENCE[m[present]]
  
  #compute the mean valence of the tweet
  scoretweet[i] <- mean(wordvalences, na.rm=TRUE)
  
  #handle the case when none of the words is in the dictionary
  if (is.na(scoretweet[i])) scoretweet[i] <- 0 else scoretweet[i] <- scoretweet[i]
  
}


head(scoretweet)
mean(scoretweet)
sd(scoretweet)
hist(scoretweet)

#lets look at the sentiment score and the tweet content
bind_cols(scoretweet,text) %>% View()

#Group in minutes and take the average per minute
p_load(lubridate)

time <- ymd_hms(created, format="%Y-%m-%d %H:%M:%S",tz="UTC")
attributes(time)$tzone <- "CET"
head(created)
head(time)
#Remove the trailing NA value
time <- na.omit(time)

#get minutes and hour for tweets
breakshour <- hour(time)
breaksmin <- minute(time)

#Compute mean per hour and minute
scores <- tibble(scoretweet,
                 hourminute =  paste(breakshour, breaksmin, sep = ":"))
scores <- scores %>% 
  group_by(hourminute) %>% 
  summarise(sentiment=mean(scoretweet))

lim <- max(abs(scores$sentiment))

#Plot sentiment by time
ggplot(scores, aes(y = sentiment, x = hourminute, group = 1)) +
  geom_line() +
  geom_hline(yintercept = 0, col = "red") +
  ylim(c(-lim,lim)) +
  labs( y = "Valence", x = "Time (hour:minute)",
        title = "Sentiment per hour and minute"
  ) 

#Or in base R
plot(1:length(scores$sentiment), 
     rev(scores$sentiment), 
     xaxt="n",
     type="l",
     ylab="Valence",
     xlab="Time (hour:minute)",
     main="Sentiment", 
     ylim=c(-lim,lim))

axis(1,at=1:nrow(scores), 
     labels=rev(unique(substr(time,12,16))))

#==================== SENTIMENTR===========================

p_load(tidyverse,textclean, textstem, sentimentr, lexicon)
# sentiment per sentence
text %>% get_sentences() %>% sentiment()
# the average sentiment per row 
sentiment_row_avg <- text %>% get_sentences() %>% sentiment_by(averaging.function = average_weighted_mixed_sentiment)
# the mean of the sentiment per row
sentiment_row_mean <- text %>% get_sentences() %>% sentiment_by(averaging.function = average_mean)
# the sentiment per sentence with emoticons and word elongations removed
sentiment_sentence_cleaned <- text %>% replace_emoticon() %>% replace_word_elongation() %>% get_sentences() %>% sentiment()

#=================================== VADER================================

# clean text
text_clean <- text %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_contraction() %>%
  replace_internet_slang() %>% 
  replace_kern() %>% 
  replace_word_elongation()

cleanText <- function(text) {
  clean_texts <- text %>%
    str_replace_all("<.*>", "") %>%                         # remove remainig emojis
    str_replace_all("&amp;", "") %>%                        # remove &
    str_replace_all("(RT|via)((?:\\b\\W*@\\w+)+)", "") %>%  # remove retweet entities
    str_replace_all("@\\w+", "") %>%                        # remove @ people, replace_tag() also works
    str_replace_all('#', "") %>%                            #remove only hashtag, replace_hash also works
    str_replace_all("[[:punct:]]", "") %>%                  # remove punctuation
    str_replace_all("[[:digit:]]", "") %>%                  # remove digits
    str_replace_all("http\\w+", "") %>%                     # remove html links replace_html() also works
    str_replace_all("[ \t]{2,}", " ") %>%                   # remove unnecessary spaces
    str_replace_all("^\\s+|\\s+$", "") %>%                  # remove unnecessary spaces
    str_trim() %>% 
    str_to_lower()
  return(clean_texts)
}
text_clean <- cleanText(text_clean)
# lemmatization 
lemma_dictionary_hs <- make_lemma_dictionary(text_clean, engine = 'hunspell')
text_final <- lemmatize_strings(text_clean, dictionary = lemma_dictionary_hs)
# sentiment
sentiment <- text_final %>% get_sentences() %>% sentiment_by()
sentiment %>% highlight()
