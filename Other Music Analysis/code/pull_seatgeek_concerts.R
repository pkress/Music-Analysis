##################-
### Author: Peter Kress
### Date:  Wed Aug 25 22:34:12 2021
### Purpose: Purpose
##################-


##################-
# Initialize Workspace ----
##################-

setwd("~/Documents/Personal Projects/Music Reviews/")

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, magrittr, stringr, ggplot2
       , googlesheets4, jsonlite)

`%p%` = paste0

##################-
# Pull Concert Data ----
##################-
seatgeek_params = list(
  baseurl = "https://api.seatgeek.com/2/"
  , client_secret = Sys.getenv(sg_secret)
  , client_id = Sys.getenv(sg_id))

location_params = list(
  type = "events"
  , range = "12mi"
  , lat = 41.799320
  , lon = -87.585083
  , per_page = 250
)

get_venue_params = list(
  type = "venues"
  , city = "chicago"
  , per_page = 125
)

make_url = function(params){
  url = params$baseurl%p%params$type%p%"?"
  keyparams = params[!grepl("type|baseurl", names(params))]
  keys = lapply(1:length(keyparams), \(x) fifelse(x>1, "&", "")%p%names(keyparams)[x]%p%"="%p%keyparams[[x]])
  for (i in 1:length(keys)) {
    url = url%p%keys[i]
  }
  return(url)
}

get_data = function(params){
  url = make_url(params)
  print(url)
  data = fromJSON(url, simplifyDataFrame = T)
}

venues = get_data(params = c(seatgeek_params, get_venue_params))

venuelist = as.data.table(venues$venues)
desired_list = paste(c("Jay Pritzker Pavilion", "House of Blues"
                       , "Empty Bottle", "Thalia Hall", "Vic Theatre"
                       , "Huntington Bank Pavilion"
                       , "Bottom Lounge", "Metro"
                       , "Riviera", "Radius", "Hideout")
                     , collapse = "|")
desired_venues = venuelist[grepl(desired_list, name), .(name, name_v2, id)][, unique(.SD)]

event_params = list(
  type = "events"
  , venue.id = paste(desired_venues$id, collapse = ",")
  , per_page = 100
)

upcoming_events = get_data(params = c(seatgeek_params, event_params))

events = upcoming_events$events
events$performers = NULL
setDT(events)


performers = upcoming_events$events$performers


perf = lapply(1:length(performers), function(i){
  e = performers[[i]]
  e$ev_id = events$id[i]
  e$images = NULL
  e$taxonomies = NULL
  e$stats = e$stats[[1]]
  if (!is.null(e$genres)) {
    e$genres = lapply(e$genres, \(x)x [, c("id", "name", "slug", "primary")])
  }
  data.table(e)
}) %>% 
  rbindlist(fill = T)
  


upcoming_concerts = merge(events, perf, by.x = "id", by.y = "ev_id", all = T)

### Last Edited:  Wed Aug 25 22:34:12 2021
