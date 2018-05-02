library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",port = 4445L)
remDr$open()

#Navigating to the Netflix.com
remDr$navigate("http://www.netflix.com/")

#Login element
loginElem <- remDr$findElement(using = "css", "[href = '/login']")
loginElem$clickElement()
Sys.sleep(1)

#userId and password elements
nameelement <- remDr$findElement(using = "css", "[name = 'email']")
psdelement <- remDr$findElement(using = "css", "[name = 'password']")
nameelement$sendKeysToElement(list("********"))
psdelement$sendKeysToElement(list("******", key = "enter"))
Sys.sleep(1)

#Using to click the second user
profileClick <- remDr$findElement("css selector", " .profile:nth-child(2) .profile-icon")
profileClick$clickElement()
Sys.sleep(1)

#Navigating to the viewing activity
netflixUrl <- remDr$navigate("http://www.netflix.com/viewingactivity")

#To load the all viewing transactions as netflix didnot bring all the data
#so I have to scroll till the end to get the next set of records, for 
#now, I am scrolling 20 times but that can be configurable
for(i in 1:20){
  webElem <- remDr$findElement("css", "body")
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(5)
  
}

#Getting all the titles
titledata <- remDr$findElements(using = 'css selector', ".title a")

#Getting all the Watching dates
dateData <-  remDr$findElements(using = 'css selector', ".nowrap") 

titlesdf <- map(titledata, ~ .x$getElementText()) %>% map_chr(1)
datesdf  <- map(dateData, ~ .x$getElementText()) %>% map_chr(1)

#Getting all the href or the links
titleshrefdf <- map(titledata, ~ .x$getElementAttribute("href")) %>% map_chr(1)

#Converting them to the data frame
titleshrefdf <- as_data_frame(titleshrefdf)
dates_df <- as_data_frame(datesdf)
titles_df <- as_data_frame(titlesdf)

#Combining all of them to the single dataframe
combined_df <- cbind(dates_df, titles_df,titleshrefdf)
names(combined_df)[1] <- paste("WatchingDate")
names(combined_df)[2] <- paste("Titles")

#Writing to the csv file
write.csv(combined_df, "netflix_user4.txt")