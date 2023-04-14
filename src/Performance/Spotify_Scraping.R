## install & load the necessary packages to use spotify API ##

install.packages("devtools")
library(devtools)
devtools::install_github('charlie86/spotifyr')
install.packages('spotifyr')
library(spotifyr)
library(tidyverse)
library(knitr) #will be used to make tables later#

install.packages('ggjoy')
install.packages("ggplot2", dependencies = TRUE)

library(spotifyr)
library(ggjoy)     # useful for plot
library(ggplot2)   # useful for plot
library(tidyverse) # makes possible the use of %>%
library(knitr)     # library to appear data results in a better way
library(lubridate)

## Authentication ##

Sys.setenv(SPOTIFY_CLIENT_ID = '5c8a7470f0bd4668b7bd05cb14f93598')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '8d20402514664db2ae8b526629325fed')

access_token <- get_spotify_access_token()

## Test to see authentication worked ##
library(spotifyr)

library(tidyverse)
library(knitr)
library(lubridate)
install.packages('ggridges')
library(ggridges)

get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()


## descriptive analysis on metallica ##

## 1. data frame of all their songs

metallica <- get_artist_audio_features('metallica')

## 2. make a subset with their original numbers to avoid duplicates, live versions etc

## following albums still need to be included: 
## new album (72 seasons), Hardwired...To Self-Destruct, Kill'Em All(remasterd)


metallica2 <- subset(metallica, metallica$album_name %in% c('Master Of Puppets (Remastered)', 
                                                             'Ride The Lightning (Remastered)' ,
                                                             '...And Justice for All (Remastered)',
                                                             'Metallica (Remastered 2021)',
                                                             'Load',
                                                             'Reload',
                                                             'St. Anger',
                                                             'Death Magnetic',
                                                             'Lulu',
                                                             'Hardwired...To Self-Destruct') )

## 3. find our artist's 5  most joyable, danceable, loud & energetic songs
joy <- metallica2
joy %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(5) %>% 
  kable()

dance <- metallica2
dance %>% 
  arrange(-danceability) %>% 
  select(track_name, danceability) %>% 
  head(5) %>% 
  kable()

loud <- metallica2
loud %>% 
  arrange(-loudness) %>% 
  select(track_name, loudness) %>% 
  head(5) %>% 
  kable() 

energy <- metallica2
energy %>% 
  arrange(-energy) %>% 
  mutate(duration_min = duration_ms /1000) %>%
  select(track_name, energy) %>% 
  head(5) %>% 
  kable() 

## 4.1 mean variables per album

Var1 <- metallica2 %>%
  group_by(album_name) %>%
  summarise(year = mean(album_release_year),
            tracks = n(),
            mean_duration = mean(duration_ms/1000),
            mean_tempo = mean(tempo),
            mean_danceability = mean(danceability),
            mean_energy = mean(energy),
            mean_valence = mean(valence),
            mean_instrumentalness = mean(instrumentalness)) %>%
  arrange(year) 
Var1

## 4.2 plot all the mean variables

ggplot(Var1) +
  geom_line(aes(year, mean_duration), size = 1, color = "darkgrey") +
  geom_point(aes(year, mean_duration, fill = reorder(album_name, year)), size = 4, shape = 21) +
  labs(x = "Year", y = " Duration in sec", fill = "Album") +
  theme(text = element_text(size = 14))

ggplot(Var1) +
  geom_line(aes(year, mean_danceability), size = 1, color = "darkgrey") +
  geom_point(aes(year, mean_danceability, fill = reorder(album_name, year)), size = 4, shape = 21) +
  labs(x = "Year", y = " mean_danceability", fill = "Album") +
  theme(text = element_text(size = 14))


ggplot(Var1) +
  geom_line(aes(year, mean_tempo), size = 1, color = "darkgrey") +
  geom_point(aes(year, mean_tempo, fill = reorder(album_name, year)), size = 4, shape = 21) +
  labs(x = "Year", y = " mean_tempo", fill = "Album") +
  theme(text = element_text(size = 14))



ggplot(Var1) +
  geom_line(aes(year, mean_energy), size = 1, color = "darkgrey") +
  geom_point(aes(year, mean_energy, fill = reorder(album_name, year)), size = 4, shape = 21) +
  labs(x = "Year", y = " mean_energy", fill = "Album") +
  theme(text = element_text(size = 14))

## 5 plot the distributions per ablum
ggplot(metallica2, aes(x = valence, y = album_name)) + 
  geom_joy() +
  theme_joy() +
  ggtitle(paste0("Joyplot"), subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")


## plot popularity per songs

## 1.
playlist <- get_playlist_tracks('37i9dQZF1DZ06evO1sJmec')
##https://open.spotify.com/playlist/37i9dQZF1DZ06evO1sJmec?si=b41286fb8a574a1f

##2 
ggplot(playlist, aes(track.popularity, reorder(track.name, track.popularity))) +
  labs(x = "Popularity", y = "Songs") +
  geom_point(size = 2, shape = 21)











## to do?

##make wordgraph with the most used keys?



 metallica2 %>%
 group_by(album_name) %>%
  ggplot(aes(album_name, danceability)) + geom_col() + coord_flip()
  



