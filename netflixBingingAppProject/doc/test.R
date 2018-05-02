netflix_user1_data <- read_csv("/data/netflix_user2.txt", col_types = cols(WatchingDate = col_date(format = "%m/%d/%y"), X1 = col_skip(), value = col_skip()), locale = locale(encoding = "ISO-8859-1"))

netflix_user1_data <- netflix_user1_data %>%
  mutate(season = "")
netflix_user1_data <- netflix_user1_data %>%
  mutate(Episodes = "")

netflix_user1_data$Titles <- sapply(netflix_user1_data$Titles, removeDoubleQuotes)

for(i in 1:nrow(netflix_user1_data)){
  title <- netflix_user1_data$Titles[i]
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
            netflix_user1_data$Titles[i] <- strArr[[1]][1]
            netflix_user1_data$season[i] <- strArr[[1]][2]
            if(length(strArr[[1]]) == 4){
              netflix_user1_data$Episodes[i] <- paste0(strArr[[1]][3], ":", strArr[[1]][4])
            }else{
              netflix_user1_data$Episodes[i] <- strArr[[1]][3]
            }
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

removeDoubleQuotes <- function(string){
  if(!is.na(string)){
    string <- gsub("\"", "", string, fixed=TRUE)
  }
  return(string)
}
