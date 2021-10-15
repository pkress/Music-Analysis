##################-
### Author: Peter Kress
### Date:  Fri Oct 15 16:15:43 2021
### Purpose: Make Pitchfork top 200 playlist
##################-


##################-
# Initialize Workspace ----
##################-

setwd(dirname(rstudioapi::getActiveDocumentContext()[["path"]]))
setwd("..")

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, magrittr, stringr, ggplot2
       , spotifyr)

`%p%` = paste0

##################-
# Read in Data ----
##################-

html_data = xml2::read_html("~/Documents/Personal Projects/Spotify Playlist Creation/pitchfork_top_list.html") %>% 
  xml2::as_list()
cleaned_data = lapply(html_data$html$body$div
  , \(x){
    dat = x$div$div$a$div
    artist = dat$h2[[1]]
    album = dat$div[[1]]
    return(list(artist = artist, album = album))
    }) %>% 
  rbindlist() %>% 
  .[
  , c(.(album = album), tstrsplit(artist, "(?<=\\d)\\. ", perl = T))
  ] %>% 
  setnames(c("album", "rank", "artist"))

##################-
# Make playlist of albums ----
##################-

# Sys.setenv("SPOTIFY_CLIENT_ID" = "client")
# Sys.setenv("SPOTIFY_CLIENT_SECRET" = "secret")
get_tracks = function(album){
  album_id = spotifyr::search_spotify(album, type = "album") %>% 
    setDT()
  if (!"id" %in% names(album_id)) {
    return(NULL)
  }
  album_id_val = album_id[1, id]
  album_tracks = spotifyr::get_album_tracks(album_id_val, limit = 50) %>% 
    setDT()
}
add_album_to_playlist = function(album, playlist){
  album_tracks = get_tracks(album)
  if (is.null(album_tracks)) {
    cat("Error with: ", album, "\nSkipping to next album\n")
    return(NULL)
  }
  add_tracks_to_playlist(playlist_id = playlist, uris = album_tracks$id)
}

lapply(cleaned_data[order(as.numeric(rank)), album], add_album_to_playlist, playlist = "3theEAk7MEjK3s1EH6U7WA")

