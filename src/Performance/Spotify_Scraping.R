## install & load the necessary packages to use spotify API ##

install.packages("devtools")
library(devtools)
devtools::install_github('charlie86/spotifyr')
install.packages('spotifyr')
library(spotifyr)
library(tidyverse)
library(knitr) 
install.packages('ggjoy')
install.packages("ggplot2", dependencies = TRUE)
library(ggjoy)     
library(ggplot2)  
library(lubridate)
install.packages('ggridges')
library(ggridges)
install.packages('kableExtra')
library(kableExtra)


## Authentication ##

Sys.setenv(SPOTIFY_CLIENT_ID = '5c8a7470f0bd4668b7bd05cb14f93598')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '8d20402514664db2ae8b526629325fed')

access_token <- get_spotify_access_token()

## Test to see authentication worked ##

get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()


## descriptive analysis on metallica ##

## 1. data frame of all their songs

metallica <- get_artist_audio_features('metallica')

## 2. Data cleaning by removing duplicates and adjusting wrong information


metallica2 <- subset(metallica, metallica$album_name %in% c('Master Of Puppets (Remastered)', 
                                                             'Ride The Lightning (Remastered)' ,
                                                             '...And Justice for All (Remastered)',
                                                             'Metallica (Remastered 2021)',
                                                             'Load',
                                                             'Reload',
                                                             'St. Anger',
                                                             'Death Magnetic',
                                                             'Lulu',
                                                             'Hardwired…To Self-Destruct',
                                                            '72 Seasons'
                                                             ) )
metallica_clean <- metallica2[!duplicated(metallica2$track_name), ]

metallica_clean$album_release_year[metallica_clean$album_name == 'Metallica (Remastered 2021)'] <- 1991

ls(metallica_clean)

## 3. find our artist's 5  most joyable, fastest, danceable & energetic songs
joy <- metallica_clean
joy %>% 
  arrange(-valence) %>% 
  select(track_name, album_name, valence) %>% 
  head(5) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = c(1), color = "black",bold = TRUE)

dance <- metallica_clean
dance %>% 
  arrange(-danceability) %>% 
  select(track_name, album_name, danceability) %>% 
  head(5) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = c(1), color = "black",bold = TRUE)

tempo <- metallica_clean
tempo %>% 
  arrange(-tempo) %>% 
  select(track_name, album_name, tempo) %>% 
  head(5) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = c(1), color = "black",bold = TRUE)

energy <- metallica_clean
energy %>% 
  arrange(-energy) %>% 
  select(track_name, album_name, energy) %>% 
  head(5) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = c(1), color = "black",bold = TRUE)



## compare albums and finding trends

## 5.1 mean variables per album

Var1 <- metallica_clean %>%
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

## 5.2 plot all the mean variables in chronological order


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

ggplot(Var1) +
  geom_line(aes(year, mean_valence), size = 1, color = "darkgrey") +
  geom_point(aes(year, mean_valence, fill = reorder(album_name, year)), size = 4, shape = 21) +
  labs(x = "Year", y = " mean_valence", fill = "Album") +
  theme(text = element_text(size = 14))


## 6 popularity

## 6.1 making a new dataframe that has the variable popularity``


metallica_playlist <- get_playlist_tracks('75B0sP0qhjbzIVidWtg6GI')
features <- get_track_audio_features(metallica_playlist$track.id)
metallicap_pop <- metallica_playlist %>%
  left_join(features, by=c('track.id' = 'id'))
`
## 6.2 checking the popularity of the tracks

ggplot(metallicap_pop, aes(track.popularity, reorder(track.name, track.popularity))) +
  labs(x = "Popularity", y = "Songs") +
  geom_point(size = 2, shape = 21)

## 6.3 plotting population density
pop_density <- density(metallicap_pop$track.popularity)
plot(pop_density, main="Popularity Distribution",
xlab="Popularity",
ylab="Density")







## Extra: creating the datasets for predictive part

Metallica_old_album  <- get_playlist_tracks('75B0sP0qhjbzIVidWtg6GI')
features2 <- get_track_audio_features(Metallica_old_album$track.id)
Metallica_NewAlbum <- Metallica_old_album %>%
  left_join(features2, by=c('track.id' = 'id'))


Metallica_newalbum  <- get_playlist_tracks('78GQOH7x89VsH8dIzJyCB7')
features3 <- get_track_audio_features(Metallica_new_album$track.id)
Metallica_NewAlbum <- Metallica_new_album %>%
  left_join(features2, by=c('track.id' = 'id'))


## Save the data
save(Metallica_NewAlbum, file = "Metallica_NewAlbum.RData") ## this will later be the test set
save(Metallica_OldAlbums, file = "metallica_OldAlbums.RData") ## this will later be the training set




