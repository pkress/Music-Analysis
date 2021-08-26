##################-
### Author: Peter Kress
### Date:  Sun Aug  8 13:07:48 2021
### Purpose: Submit Reviews to AOTY
##################-


##################-
### Initialize Workspace ----
##################-

setwd(dirname(rstudioapi::getActiveDocumentContext()[["path"]]))
setwd("..")

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, magrittr, stringr, ggplot2
       , RSelenium, googlesheets4)

`%p%` = paste0

##################-
### Pull Data from Sheets ----
##################-

review_storage_sheet = "https://docs.google.com/spreadsheets/d/1oSKe73qycEborK_ZLnfL9524oz41CPwWHw1rdpD6IWs"

albs = read_sheet(review_storage_sheet, sheet = "albums") %>%
  setDT() 

##################-
### Create functions for navigating AOTY ----
##################-

log_in = function(){
  ## Function to log into existing AOTY account using facebook
  signIn = remDr$findElement(using = "xpath", value = '//*[@id="content"]/div[8]')
  remDr$mouseMoveToLocation(webElement = signIn)
  remDr$click()
  
  fb_signin = remDr$findElement(using = "xpath", value = '//*[@id="centerContent"]/div/div[1]/a[1]/div')
  remDr$mouseMoveToLocation(webElement = fb_signin)
  remDr$click()
  
  
  email = remDr$findElement(using = "name", value = "email")
  if(email$showErrorClass()$status!=0) return()
  fb_user = rstudioapi::askForPassword(prompt = "Enter Facebook Username or Phone Number")
  email$sendKeysToElement(sendKeys = list(fb_user))
  
  pw = remDr$findElement(using = "name", value = "pass")
  fb_pw = rstudioapi::askForPassword(promp = "Enter Facebook Password")
  pw$sendKeysToElement(sendKeys = list(fb_pw))
  
  remDr$sendKeysToActiveElement(list("\t", "\t", "\n"))
  
  Sys.sleep(0.5)
  
  email = remDr$findElement(using = "name", value = "email")
  if(email$showErrorClass()$status==0) {
    warning("Log in failed - Try Again!")
    remDr$navigate("https://www.albumoftheyear.org")
    return()
  }
}

log_out = function(){
  ## Function to log out of AOTY account
  account = remDr$findElement(using = "xpath", value = '//*[@id="content"]/div[8]')
  remDr$mouseMoveToLocation(webElement = account)
  remDr$click()
  
  logout = remDr$findElement(using = "xpath", value = '//*[@id="accountLinks"]/div[8]/a')
  remDr$mouseMoveToLocation(webElement = logout)
  remDr$click()
}

find_album = function(albumname, artistname){
  search_album = gsub(" ", "%20", albumname)
  searchURL = "https://www.albumoftheyear.org/search/albums/?q="%p%search_album
  remDr$navigate(url = searchURL)
  
  albumResults = remDr$findElements(using = "class", value = "albumTitle")
  albumTitles = sapply(albumResults, function(x){unlist(x$getElementText())})
  
  artistResults = remDr$findElements(using = "class", value = "artistTitle")
  artistTitles = sapply(artistResults, function(x){unlist(x$getElementText())})
  
  best_match_alb = which.min(stringdist(titles, albumname))
  best_match_art = which.min(stringdist())
  ## Get link to the best match
  #best_result = albumResults[[best_match]]$getElementAttribute("href")
  
  remDr$mouseMoveToLocation(webElement = best_result)
  remD
}

##################-
### Navigate AOTY ----
##################-

search

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox"
)

remDr$open()

remDr$navigate("https://www.albumoftheyear.org")

log_out()
log_in()


remDr$

##################-
### Submit Reviews to aoty ----
##################-


### Last Edited:  Sun Aug  8 13:07:48 2021