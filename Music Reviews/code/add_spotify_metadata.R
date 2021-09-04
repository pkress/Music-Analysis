##################-
### Author: Peter Kress
### Date:  Sat Aug  7 17:08:26 2021
### Purpose: Pull metadata from spotify API to google sheet
##################-


##################-
### Initialize Workspace ----
##################-

setwd(dirname(rstudioapi::getActiveDocumentContext()[["path"]]))
setwd("..")

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, magrittr, stringr, ggplot2
       , spotifyr, googlesheets4)

`%p%` = paste0

##################-
### Set spotifyr parameters ----
##################-

sp_client = rstudioapi::askForPassword("Client ID")
sp_secret = rstudioapi::askForPassword("Client Secret")
Sys.setenv(SPOTIFY_CLIENT_ID = sp_client)
Sys.setenv(SPOTIFY_CLIENT_SECRET = sp_secret)

review_response_sheet = "https://docs.google.com/spreadsheets/d/1jGTVQpdzbn97j7C4Fw6VWgeWBlSk7lGQfUMGEBQqmyY"
review_storage_sheet = "https://docs.google.com/spreadsheets/d/1oSKe73qycEborK_ZLnfL9524oz41CPwWHw1rdpD6IWs"

access_token <- get_spotify_access_token()

##################-
### Functions to retrieve release data ----
##################-

## Set useful values for function
uri_names = c("header", "type", "sp_id")

get_release = function(uri){
  ## Function to get spotify API data for the given URI
  
  uri_split = tstrsplit(uri, ":") %>% 
    setNames(.,uri_names)
  ## Check that uri is of expected format
  stopifnot(length(uri_split)==3 
            & uri_split$type%chin%c('album', 'track', 'artist', 'playlist'))
  get_type = get("get_"%p%uri_split$type)
  release = get_type(uri_split$sp_id)
  release$release_type = uri_split$type
  return(release)
}

parse_album = function(album, get_top_track = T){
  ## Function to extract data from album releases
  
  album_dt = data.table(
    album_name = album$name
    , album_artist = unique(album$artists$name)[1]
    , album_other_artists = paste(unique(album$artists$name)[-c(1)], collapse = ";&; ")
    , album_label = album$label
    , album_type = album$album_type
    , genres = album$genres
    , popularity = album$popularity
    , release_date = album$release_date
    , tracks = album$total_tracks
    , duration = sum(album$tracks$items$duration_ms)
  )
  
  if(get_top_track) {
    top_track = which.max(sapply(album$tracks$items$id, function(id){get_track(id)[["popularity"]]}))
    album_dt$top_track = album$tracks$items$name[top_track]
  } else {
    album_dt$top_track = "Not Retrieved"
  }
  
  return(album_dt)
}

parse_track = function(track){
  ## Function to extract track data from track releases
  
  track_dt = data.table(
    track_name = track$name
    , album_name = track$album$name
    , artist_name = track$artists$name[1]
    , other_artists = paste(unique(track$artists$name)[-c(1)], collapse = ";&; ")
    , duration = track$duration_ms
    , popularity = track$popularity
    , release_date = track$album$release_date
    , track_number = track$track_number
    , album_tracks = track$album$total_tracks
  )
  
  return(track_dt)
}

parse_uri = function(uri){
  ## Function to extract release data from track and album URIs
  
  if(grepl('^https://', uri)){
    url = uri
    uri_type = gsub("^https://open.spotify.com/", "", url) %>% 
      str_split("/") %>% 
      .[[1]] %>% 
      .[1]
    uri_id = gsub("^https://open.spotify.com/", "", url) %>% 
      str_split("/") %>% 
      .[[1]] %>% 
      .[2] %>% 
      str_split("\\?") %>% 
      .[[1]] %>% 
      .[1]
    uri = paste("spotify", uri_type, uri_id, sep = ":")
  }
  
  release = get_release(uri)
  
  if(!release$release_type%chin%c("album", "track")){
    stop("Only Track and Album releases are currently supported")
  }
  
  parse_func = get("parse_"%p%release$release_type)
  
  parsed_out = parse_func(release)
  parsed_out$relase_type = release$release_type
  
  return(parsed_out)
  
}

extract_review_data = function(reviews){
  ## Take in dt of reviews to process
  
  review_releases = lapply(reviews$`Spotify URI`, get_release)
  
  release_types = sapply(review_releases, `[[`, "release_type")
  
  release_albums = lapply(review_releases[release_types=="album"], parse_album) %>% 
    rbindlist() %>% 
    cbind(reviews[release_types=="album"])
  release_tracks = lapply(review_releases[release_types=="track"], parse_track) %>% 
    rbindlist() %>% 
    cbind(reviews[release_types=="track"])
  
  outdata = list(albums = release_albums, tracks = release_tracks)
  
  return(outdata)
}

##################-
### Update Reviews Data ----
##################-

prev_albs = read_sheet(review_storage_sheet, sheet = "albums") %>%
  setDT() 
prev_tracks = read_sheet(review_storage_sheet, sheet = "tracks") %>% 
  setDT()

last_response = fifelse(nrow(prev_albs) + nrow(prev_tracks)>0
                        , max(as.POSIXct(c(prev_albs$Timestamp, prev_tracks$Timestamp)))
                        , as.POSIXct(0, origin = "1970-01-01"))

form_responses = read_sheet(review_response_sheet) %>% 
  setDT()

new_reviews = form_responses[
  as.POSIXct(Timestamp) > last_response + 1/1E3
]

new_review_data = extract_review_data(new_reviews)

for(nd in c("albums", "tracks")){
  if(nrow(new_review_data[[nd]])>0) {
    sheet_append(review_storage_sheet, sheet = nd, data = new_review_data[[nd]])
  }
}

### Last Edited:  Sat Aug  7 19:52:36 2021
