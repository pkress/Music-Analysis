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
p_load(data.table, urltools, jsonlite, magrittr, googlesheets4, rvest, xml2)

############################-
### Functions for pulling data ----
############################-

build_tracks_fetch_query <- function(user, page, tracktype = "recenttracks"
                                     , baseurl = 'http://ws.audioscrobbler.com/2.0/?') {
  baseurl = param_set(baseurl, 'method', paste0('user.get', tracktype))
  baseurl = param_set(baseurl, 'api_key', "397e5ce8d72d0a07ea54f5e805741ade")
  baseurl = param_set(baseurl, 'user', user)
  baseurl = param_set(baseurl, 'page', as.character(page))
  baseurl = param_set(baseurl, 'limit', '200')
  baseurl = param_set(baseurl, 'format', 'json')
  baseurl = param_set(baseurl, 'extended', '1')

  return(fromJSON(baseurl))
}

fetch_all_user_scrobbles <- function(user, max_pages = 0, last_pull = 0) {
  cat("\nPulling for ", user, fifelse(last_pull==0, " since inception.\n"
                                    , paste0("since the last pull at "
                                             , as.POSIXct(last_pull, origin = "1970-01-01"), "\n")
                                    ))
  response <- tryCatch({build_tracks_fetch_query(user, 1)}
                       , error = function(e){
                         print(paste("Error in url. Error message:", e, ". Trying again."))
                         Sys.sleep(5)
                         tryCatch({build_tracks_fetch_query(user, 1)}
                                  , error = function(e){
                                    print(paste("Error in url. Error message:", e, "\nMoving to next artist."))
                                    NULL
                                    })
                         })
  if (is.null(response)) {
    return(NULL)
  } else {
    num_pages <- as.numeric(response$recenttracks$`@attr`$totalPages)
  
    total_pages <- ifelse(max_pages!=0, min(max_pages, num_pages), num_pages)
  
    for (page in 1:total_pages) {
      print(paste0("Exporting page ", page, "/", total_pages))
      response <- tryCatch({build_tracks_fetch_query(user, page)}
                           , error = function(e){
                             print(paste("Error in url. Error message:", e, ". Trying again."))
                             Sys.sleep(5)
                             tryCatch({build_tracks_fetch_query(user, page)}
                                      , error = function(e){
                                        print(paste("Error in url. Error message:", e, ". Trying again."))
                                      })
                           })
      out = as.data.table(response$recenttracks$track)
      if (page==1) all_tracks = out
      else all_tracks = rbind(all_tracks, out)
      if (all_tracks[!is.na(date.uts), any(date.uts<last_pull)]) {
        cat("Retrieved data\n")
        all_tracks = all_tracks[date.uts>last_pull]
        return(all_tracks)
      }
    }
    return(all_tracks)
  }
}

## Function to pull down a users data and add to google sheets
add_user_func = function(user){
  
  #which sheet holds the data
  history_sheet = gs4_find('Listening History')
  #read in currently saved data
  prev_data = tryCatch({
    read_sheet(history_sheet$id, sheet = user) %>%
    setDT()
    }
    , error = function(e){
      cat("\nNo data for user. ", e$message, "\n")
      NULL
    })
  #identify the last saved track
  last_track = fifelse(is.null(prev_data), 0, prev_data[, as.numeric(max(date.uts))])
  #pull tracks since last track
  
  tracks = fetch_all_user_scrobbles(user, max_page = 0, last_pull = last_track)
  #remove "image" columns (which are cols of dataframes)
  if (is.null(tracks)){
    return(NULL)
  } else{
    tracks[, `:=`(artist.image = NULL, image = NULL, X.attr.nowplaying = NULL)]
    all_data = rbindlist(list(tracks, prev_data), use.names = T, fill = T)
    #append the new data to the sheet
    fwrite(all_data, paste0("intermediate/listening_history_", user, ".csv"))
    sheet_write(all_data, history_sheet$id, sheet = user)
    return(all_data)
  }
}

############################-
### Add last.fm data for users of interest ----
############################-

uservec = c("pkress2") %>% #, "mghaight", "nrmclean", "nbennett17", "dakatcher", "adsmithrose", "jeffdillenbeck", "Keriann-Reeves") %>% 
  setNames(.,.)
all_data = lapply(uservec, add_user_func)
