library(readr)
library(httr)
library(curl)
library(jsonlite)

#Get the Pattern that is the word used to separate the seasons and titles
getPattern <- function(title){
  pattern <- NA
  if(str_detect(title, "Season")){
    pattern <- "Season"
  }else{
    if(str_detect(title, "Volume")){
      pattern <- "Volume"
    }else{
      if(str_detect(title, "Series")){
        pattern <- "Series"
      }
    }
  }
  return(pattern)
}



#Function to clean the titles of the shows
cleanTitles <- function(title, pattern){
  if(str_detect((title), pattern)){
    titleArr = str_split((title), pattern)
    return(titleArr)
  }else{
    return(NA) 
  }
}

#removing double quotes
removeDoubleQuotes <- function(string){
  if(!is.na(string)){
    string <- gsub("\"", "", string, fixed=TRUE)
  }
  return(string)
}

#removing end colons if there are any for the string passed
removeEndColons <- function(string){
  string <- str_trim(string)
  if(!is.na(string)){
    if(str_detect(string, ":$")){
      string <- str_replace(string, ":$", "")
    }
  }
  return(string)
}

#Call to omdb Api to get the details for the given title
getShowData <- function(title){
  url <- paste0("http://www.omdbapi.com/?apikey=", api.key.omdb, "&t=", title, "&r=json")
  json_result <- url %>% curl() %>% readLines()
  return(prettify(json_result) %>% fromJSON())
}

#Removing the mins from the runtime to make it numeric
cleanRuntime <- function(runtime){
  if(!is.na(runtime)){
    if(str_detect(runtime, "min")){
      runtime <- str_remove(runtime, "min")
      runtime <- str_trim(runtime)
      return(as.numeric(runtime))
    }else{
      if(is.numeric(runtime)){
        return(runtime)
      }
    }
  }
  return(NA)
  
}

#Clean the year column to save the table in SQL
cleanYear <- function(year){
  year <- iconv(year, to = "UTF-8")
  if(!is.na(year)){
    if(str_detect(year, "Ã¢â¬â")){
      year <- str_replace(year, "Ã¢â¬â", "-")
    }
    if(str_detect(year, "-$")){
      year <- str_replace(year, "-$", "")
    }
  }
  return(year)
}

#Convert to UTF8 for any string
convertToUTF8 <- function(str){
  str <- iconv(str, to = "UTF-8")
  return(str)
}

#Main function for the setting episodes and titles for the user viewing history
setEpisodesTitles <- function(netflix_user1_data){
  for(i in 1:nrow(netflix_user1_data)){
    title <- netflix_user1_data$Titles[i]
    pattern <- getPattern(title)
    
    if(!is.na(pattern)){
      titleArr <- cleanTitles(title, pattern)
      if(!is.na(titleArr)){
        if(length(titleArr[[1]]) > 1){
          if(titleArr[[1]][2] == ""){
            netflix_user1_data$season[i] <- NA
            netflix_user1_data$Episodes[i] <- NA
          }else{
            netflix_user1_data$Titles[i] <- removeEndColons(titleArr[[1]][1])
            if(str_detect(titleArr[[1]][2], ":")){
              seasonArr <- str_split(titleArr[[1]][2], ":")
              netflix_user1_data$season[i] <- paste0(pattern, seasonArr[[1]][1])
              netflix_user1_data$Episodes[i] <- seasonArr[[1]][2]
            }
            
          }
          
        }
      }
    }else{
      if(str_detect(title, ":")){
        strArr <- str_split(title, ":")
        
        if(length(strArr[[1]]) >= 3){
          if(tolower(str_trim(strArr[[1]][1])) == tolower(str_trim(strArr[[1]][2]))){
            netflix_user1_data$Titles[i] <- strArr[[1]][1]
            netflix_user1_data$season[i] <- paste0("Season", " 1")
            if(length(strArr[[1]]) == 4){
              netflix_user1_data$Episodes[i] <- paste0(strArr[[1]][3], ":", strArr[[1]][4])
            }else{
              netflix_user1_data$Episodes[i] <- strArr[[1]][3]
            }
            
          }else{
            newTitle <- paste0(strArr[[1]][1], " 2")
            if(tolower(str_trim(newTitle)) == tolower(str_trim(strArr[[1]][2]))){
              netflix_user1_data$Titles[i] <- strArr[[1]][1]
              netflix_user1_data$season[i] <- paste0("Season", " 2")
              if(length(strArr[[1]]) == 4){
                netflix_user1_data$Episodes[i] <- paste0(strArr[[1]][3], ":", strArr[[1]][4])
              }else{
                netflix_user1_data$Episodes[i] <- strArr[[1]][3]
              }
            }else{
              netflix_user1_data$season[i] <- NA
              netflix_user1_data$Episodes[i] <- NA
            }
          }
        }else{
          netflix_user1_data$season[i] <- NA
          netflix_user1_data$Episodes[i] <- NA
        }
      }else{
        netflix_user1_data$season[i] <- NA
        netflix_user1_data$Episodes[i] <- NA
      }
      
    }
  }  
  return(netflix_user1_data)
}

#Main function for getting the show details
settingShowDetails <- function(showDetailsDf){
  
  #Cleanin the title just for the omdb api to get the data for the title
  for(i in 1:nrow(showDetailsDf)){
    
    title <- showDetailsDf$Titles[i]
    #Trimming the title to remove any spaces
    title <- str_trim(title)
    #If the title contains any colon, remove it
    if(str_detect(title, ":")){
      title <- str_replace_all(title, ":", "")
    }
    #Replace all the spaces with '+'
    if(str_detect(title, "\\s")){
      title <- str_replace_all(title, "\\s", "+")
    }
    #Don't consider any paranthesis
    if(str_detect(title, "\\(")){
      titleArr <- str_split(title, "\\(")
      title <- titleArr[[1]][1]
    }
    #Call OMDB api
    showDetails <- getShowData(title)
    if(showDetails$Response == "False"){
      showDetailsDf <- settingShowDetails_ColNA(i, showDetailsDf)
    }else{
      if(length(showDetails) > 2){
        showDetailsDf <- settingShowDetails_ColValues(i, showDetailsDf, showDetails)
      }
    }
  }
  return(showDetailsDf)
}

#if OMDB api doesnot have the data, set it to NA
settingShowDetails_ColNA <- function(i, showDetailsDf){
  showDetailsDf$Year[i] <- NA
  showDetailsDf$Rated[i] <- NA
  showDetailsDf$Released[i] <- NA
  showDetailsDf$Runtime[i] <- NA
  showDetailsDf$Genre[i] <- NA
  showDetailsDf$Director[i] <- NA
  showDetailsDf$Writer[i] <- NA
  showDetailsDf$Actors[i] <- NA
  showDetailsDf$Plot[i] <- NA
  showDetailsDf$Language[i] <- NA
  showDetailsDf$Country[i] <- NA
  showDetailsDf$imdbRating[i] <- NA
  showDetailsDf$imdbVotes[i] <- NA
  showDetailsDf$imdbID[i] <- NA
  showDetailsDf$Type[i] <- NA
  return(showDetailsDf)
}

#Setting the showdetails column 
settingShowDetails_ColValues <- function(i, showDetailsDf, showDetails){
  showDetailsDf$Year[i] <- showDetails$Year
  showDetailsDf$Rated[i] <- showDetails$Rated
  showDetailsDf$Released[i] <- showDetails$Released
  showDetailsDf$Runtime[i] <- showDetails$Runtime
  showDetailsDf$Genre[i] <- showDetails$Genre
  showDetailsDf$Director[i] <- showDetails$Director
  showDetailsDf$Writer[i] <- showDetails$Writer
  showDetailsDf$Actors[i] <- showDetails$Actors
  showDetailsDf$Plot[i] <- showDetails$Plot
  showDetailsDf$Language[i] <- showDetails$Language
  showDetailsDf$Country[i] <- showDetails$Country
  showDetailsDf$imdbRating[i] <- showDetails$imdbRating
  showDetailsDf$imdbVotes[i] <- showDetails$imdbVotes
  showDetailsDf$imdbID[i] <- showDetails$imdbID
  showDetailsDf$Type[i] <- showDetails$Type
  return(showDetailsDf)
}