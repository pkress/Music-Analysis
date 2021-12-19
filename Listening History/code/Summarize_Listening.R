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
p_load(data.table, ggplot2, magrittr
       , urltools, jsonlite, magrittr, googlesheets4, rvest, xml2)


############################-
### Functions to summarize listening  ----
############################-

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

## Function to pull down a users data and add to google sheets
summarize_user_func = function(user){
  
  #which sheet holds the data
  history_sheet = gs4_find('Listening History')
  #read in currently saved data
  user_data = tryCatch({
    read_sheet(history_sheet$id, sheet = user) %>%
    setDT()
    }
    , error = function(e){
      cat("\nNo data for user. ", e$message, "\n")
      NULL
    })
  
  art_sum = summarize_artists(user_data)
  
  sheet_write(art_sum, history_sheet$id, sheet = paste0(user, "_artist_summary"))
  return(art_sum)
}

get_art_sum = function(user){
  history_sheet = gs4_find('Listening History')
  #read in currently saved data
  user_data = tryCatch({
    read_sheet(history_sheet$id, sheet = paste0(user, "_artist_summary")) %>%
      setDT()
  }
  , error = function(e){
    cat("\nNo data for user. ", e$message, "\n")
    NULL
  })
}

############################-
### Pull data  ----
############################-
users = c("pkress2") %>% 
  setNames(.,.)

## Retrieve and save artist summaries
save_art_sums = lapply(users, summarize_user_func) 

############################-
### Explore data summaries  ----
############################-

art_sums = lapply(users, get_art_sum)

## combine each user's summary into a data.table
art_dt = rbindlist(art_sums, idcol = "user")

## add share of plays by artist
art_dt[
  , share_plays := artist_count/sum(count)
  , .(user)
]

## look at my top artists
pk_top_art = art_dt[
  user=="pkress2"
  , unique(.SD)
  , .SDcols = c("artist_count","share_plays","user", "artist.name")
  ][
  order(-artist_count)
  ][
  1:25
  , unique(.SD)
  , .SDcols = c("artist.name", "share_plays", "user")
  ]

fwrite(pk_top_art, "output/tables/pk_top_artists.csv")

## Create summary plot
top_artist_plot = pk_top_art %>% 
  ggplot()+
  geom_segment(aes(x = reorder(artist.name, share_plays), xend = reorder(artist.name, share_plays)
                   , y = 0, yend = share_plays)
             , size = 2, color = "light blue")+
  geom_point(aes(x = reorder(artist.name, share_plays), y = share_plays)
             , size = 4, color = "dark blue")+
  facet_grid(cols = vars(user))+
  scale_y_log10()+
  labs(x = "Artist Name", y = "Share of User Plays"
       , title = "Artist Share of User Plays by User")+
  scale_y_continuous(labels = function(x){paste0(round(100*x, 1), "%")})+
  coord_flip()+
  theme_bw()

ggsave(plot = top_artist_plot, filename = "intermediate/top_artist_plot.pdf", height = 8, width = 6)
