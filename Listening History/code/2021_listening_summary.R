###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###   DRAFT
###   PRIVILEGED AND CONFIDENTIAL
###   PREPARED AT THE REQUEST OF COUNSEL
###
###   CASE NUMBER:  ----
###   PURPOSE:  ----
###   AUTHOR: Peter Kress ----
###
###   AUDITED: NO
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

############################-
### Initialize Workspace ----
############################-

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load(data.table, magrittr, googlesheets4)
##################-
# Data Pulling Functions ----
##################-
summarize_artists = function(data){
  outdata = data[
    order(mbid, date.uts)
    , .(first = as.POSIXct(first(date..text), format = "%d %b %Y, %R") # First play
        , last = as.POSIXct(last(date..text), format = "%d %b %Y, %R") # Most recnt play
        , count = .N )
    , .(artist.name, artist.mbid, album..text, album.mbid, mbid, name)
  ][
    , `:=`(album_first = min(first)
           , album_last = max(last)
           , album_count = sum(count)
           , album_distinct = uniqueN(paste0(mbid, name))
           , album_top = paste0(unique(name[count==max(count)]), collapse = "; ")
           , album_bottom = paste0(unique(name[count==min(count)]), collapse = "; ")
    )
    , .(album..text, album.mbid)
  ][
    , `:=`(artist_first = min(first)
           , artist_last = max(last)
           , artist_count = sum(count)
           , artist_distinct_albums = uniqueN(paste0(album.mbid, album..text))
           , artist_distinct = uniqueN(paste0(mbid, name))
           , artist_top = paste0(unique(name[count==max(count)]), collapse = "; ")
           , artist_bottom = paste0(unique(name[count==min(count)]), collapse = "; ")
           , artist_top_album = paste0(unique(album..text[album_count==max(album_count)]), collapse = "; ")
           , artist_bottom_album = paste0(unique(album..text[album_count==min(album_count)]), collapse = "; "))
    , .(artist.name, artist.mbid)
  ][
    order(-artist_count, -album_count, -count)
  ]
  return(outdata)
}

##################-
# Pull in data ----
##################-

user_data = fread('/Users/pkress/Documents/GitHub/Music-Reviews/Listening History/intermediate/listening_history_pkress2.csv')

yearly_data = lapply(c(2018:2021) %>% setNames(.,.), \(x){user_data[year(as.POSIXct(date..text, format = "%d %b %Y, %R"))==x]})
yearly_sum = lapply(yearly_data, summarize_artists)
overall_sum = summarize_artists(user_data)

yearly_sum[[4]]

albums = \(x){
  x[, .(artist.name, artist.mbid, album..text, album.mbid
        , album_first, album_last, album_count, album_distinct
        , album_top, album_bottom)
    ] %>% 
    unique()
}
artists = \(x){
  x[, unique(.SD), .SDcols = patterns("artist")]
}

overall_albums = albums(overall_sum)
overall_artists = artists(overall_sum)

yearly_albums = lapply(yearly_sum, albums) %>% 
  rbindlist(idcol = "year") %>% 
  rbind(.,overall_albums[, year:="overall"])
yearly_artists = lapply(yearly_sum, artists) %>% 
  rbindlist(idcol = "year") %>% 
  rbind(.,overall_artists[, year:="overall"])

top_20_artists = overall_artists
