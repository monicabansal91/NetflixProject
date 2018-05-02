#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
#library(RMySQL)
library(lubridate)
library(plotly)
library(ggthemes)

#For now, I have commented the SQL code, now I am reading the data directly 
#from the cleaned file

# mydb = dbConnect(MySQL(), 
#                  user = 'root', 
#                  password = '****', 
#                  dbname = 'netflix_data', 
#                  host = 'localhost')
# 
# user1History <- dbReadTable(conn = mydb, name = 'user1_data')
# showDetails <- dbReadTable(conn = mydb, name = 'show_data')

#Creating the data frame 
#To deploy on shinyapp.io, I have put the files in the same folder.
user1History <- read_csv("user1_data.csv") 
showDetails  <- read_csv("show_data.csv") 

#Joining the table 
ShowHistoryJoinTable <- showDetails %>%
  inner_join(user1History)

#Checking that the type is marked correctly and 
#if there is a discrepancy then marked Runtime NA
#for that show
for(i in 1:nrow(ShowHistoryJoinTable)){
  if(!is.na(ShowHistoryJoinTable$Type[i])){
    if(ShowHistoryJoinTable$Type[i] == "movie"){
      if(!is.na(ShowHistoryJoinTable$season[i])){
        ShowHistoryJoinTable$Runtime[i] <- NA
      }
    }
  }
}

#Creating a data frame for Genre as Genre is a list in the data frame 
#and we need to separate the Genres to get the individual Genres
#so, I have used unnest
ShowHistoryJoinTable$Genre <- as.list(ShowHistoryJoinTable$Genre)
genreDf <- ShowHistoryJoinTable %>%
  transform(Genre = str_split(Genre, ",")) %>%
  unnest(Genre)

#Triming the Genre column in genreDf 
genreDf$Genre <- sapply(genreDf$Genre, str_trim)

#To get the list of only series type
seriesTitlesData <- ShowHistoryJoinTable %>%
  filter(Type == "series")


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Netflix Binging App",
             tabPanel("Series Bing watching", 
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput('watchingDateRange',
                                         label = 'watching Date Range',
                                         start = min(user1History$WatchingDate), end = max(user1History$WatchingDate)
                          ),
                          sliderInput("EpisodesCountInput", "Episodes Count per Show", min = 1, 
                                      max = 20, value = c(2,10)),
                          sliderInput("RuntimeInput", "Runtime in mins per show", min = 10,
                                      max = 500, value = c(100, 300)),
                          selectInput("TitlesInput","Show Titles",
                                      c(seriesTitlesData$Titles, "All"), selected = "All")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Runtime Plot", plotOutput("runtimePlot")),
                            tabPanel("Episode Count Plot", plotOutput("episodePlot"))
                            
                          )
                          
                        )
                      ) 
             ),
             tabPanel("Genres Tab",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput('genrewatchingDateRange',
                                         label = 'Watching Date Range',
                                         start = min(user1History$WatchingDate), end = max(user1History$WatchingDate)
                          ),
                          sliderInput("genreRuntimeInput", "Runtime in mins", min = 10,
                                      max = 1000, value = c(100, 300)),
                          selectInput("GenreInput","Show Genres",
                                      c(genreDf$Genre, "All"), selected = "All")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("All Genre Plot", plotOutput("genrePlot")),
                            tabPanel("Specific Genre Plot", plotOutput("specificGenrePlot")),
                            tabPanel("Genre Vs Time", plotOutput("genreTimePlot"))
                          )
                        )
                      )
             ),
             tabPanel("Season Binging",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput('seasonwatchingDateRange',
                                         label = 'Watching Date Range',
                                         start = min(user1History$WatchingDate), end = max(user1History$WatchingDate)
                          ),
                          selectInput("seasonTitlesInput","Show Titles",
                                      seriesTitlesData$Titles, selected = "Friends")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Season Plot", plotOutput("seasonPlot"))
                            
                          )
                        )
                      )
             ),
             tabPanel("Summary Tab",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput('summarywatchingDateRange',
                                         label = 'Watching Date Range',
                                         start = min(user1History$WatchingDate), end = max(user1History$WatchingDate)
                          ),
                          sliderInput("summaryRuntimeInput", "Runtime in mins", min = 10,
                                      max = 1000, value = c(100, 300)
                          ),
                          radioButtons("TypeInput", "Show Type",  
                                       choices = c("Movies", "Series", "Both"), selected = "Both"),
                          selectInput("summaryTitlesInput","Titles",
                                      c(ShowHistoryJoinTable$Titles, "All"), selected = "All")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Summary Plot", plotOutput("summaryPlot"))
                            
                          )
                        )
                      )
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Function to get the data based on the two parameters
  getData <- function(title, dateRange){
    startDate <- ymd(dateRange[[1]])
    endDate <- ymd(dateRange[[2]])
    if(title == "All"){
      df <- ShowHistoryJoinTable
    }else{
      df <- ShowHistoryJoinTable %>%
        filter(Titles == title)
    }
    df <- df %>%
      filter(WatchingDate >= startDate , WatchingDate <= endDate) 
    return(df)
  }
  
  #Function to get the data for Runtime Plot
  getRuntimeData <- function(title, dateRange, runtime, episodeCount){
    df <- getData(title, dateRange)
    df <- df %>%
      filter(!is.na(Runtime), Type != "movie") %>%
      select(WatchingDate, Runtime, Titles) %>%
      group_by(WatchingDate, Titles) %>%
      summarise(
        totalTime = sum(Runtime),
        episodesCount = n()
      ) %>%
      filter(totalTime >= runtime[[1]], totalTime <= runtime[[2]]) %>%
      filter(episodesCount >= episodeCount[[1]], episodesCount <= episodeCount[[2]])
    return(df)
  }
  #Function to get the data for Episodes Plot
  getEpisodesData <- function(title, dateRange, runtime, episodeCount){
    df <- getData(title, dateRange)
    df <- df %>%
      filter(!is.na(Runtime), Type != "movie") %>%
      select(WatchingDate, Runtime, Titles) %>%
      group_by(WatchingDate, Titles) %>%
      summarise(
        totalTime = sum(Runtime),
        episodesCount = n()
      ) %>%
      filter(totalTime >= runtime[[1]], totalTime <= runtime[[2]]) %>%
      filter(episodesCount >= episodeCount[[1]], episodesCount <= episodeCount[[2]])
    
    return(df)
  }
  
  totalRuntimePlot <- reactive({
    getRuntimeData(input$TitlesInput, input$watchingDateRange, input$RuntimeInput, input$EpisodesCountInput)
  })
  
  totalEpisodePlot <- reactive({
    getEpisodesData(input$TitlesInput, input$watchingDateRange, input$RuntimeInput, input$EpisodesCountInput)
  })
  
  #Function to get the data for Genre Plot
  genrePlot <- reactive({
    startDate <- input$genrewatchingDateRange[[1]]
    endDate <- input$genrewatchingDateRange[[2]]
    df <- genreDf %>%
      filter(!is.na(Runtime)) %>%
      select(WatchingDate, Runtime, Genre) %>%
      group_by(WatchingDate, Genre) %>%
      summarise(
        totalTime = sum(Runtime)
      ) %>%
      filter(totalTime >= input$genreRuntimeInput[[1]], totalTime <= input$genreRuntimeInput[[2]]) %>%
      filter(WatchingDate >= startDate , WatchingDate <= endDate) 
    return(df)
  })
  
  #Function to get the data for specific Genre Plot
  specificGenrePlot <- reactive({
    startDate <- input$genrewatchingDateRange[[1]]
    endDate <- input$genrewatchingDateRange[[2]]
    df <- genreDf %>%
      filter(!is.na(Runtime), Genre == input$GenreInput) %>%
      group_by(WatchingDate, Titles) %>%
      summarise(
        totalTime = sum(Runtime)
      ) %>%
      filter(totalTime >= input$genreRuntimeInput[[1]], totalTime <= input$genreRuntimeInput[[2]]) %>%
      filter(WatchingDate >= startDate , WatchingDate <= endDate) 
    return(df)
  })
  
  #Function to get the data for GenreTime Plot
  genreTimePlot <- reactive({
    if(input$GenreInput == "All"){
      df <- genreDf %>%
        filter(!is.na(Genre), !is.na(Type))
      return(df)
    }else{
      df <- genreDf %>%
        filter(!is.na(Genre), !is.na(Type), Genre == input$GenreInput)
      return(df)
    }
    
  })
  
  #Function to get the data for Season Plot
  seasonPlot <- reactive({
    df <- getData(input$seasonTitlesInput, input$seasonwatchingDateRange)
    df <- df %>%
      group_by(WatchingDate, season) %>%
      summarise(
        episodesCount = n()
      )
    return(df)
  })
  
  #Function to get the data for Summary Plot
  summaryPlot <- reactive({
    
    typeInput <- switch(input$TypeInput,
                        Movies = "movie",
                        Series = "series",
                        Both   = "both"
    )
    df <- getData(input$summaryTitlesInput, input$summarywatchingDateRange)
    df <- df %>%
      filter(!is.na(Runtime)) %>%
      select(WatchingDate, Runtime, Type) %>%
      group_by(WatchingDate, Type) %>%
      summarise(
        totalTime = sum(Runtime)
      ) %>%
      filter(totalTime >= input$summaryRuntimeInput[[1]], totalTime <= input$summaryRuntimeInput[[2]])
    if(typeInput != "both"){
      df <- df %>%
        filter(Type == typeInput)
    }
    return(df) 
    
  })
  
  #output Plots for Runtime
  output$runtimePlot <- renderPlot({
    totalRuntimePlot() %>%
      ggplot(mapping = aes(WatchingDate, totalTime)) +
      geom_bar(aes(fill = Titles), position = position_stack(), stat = "identity") +
      #geom_text(aes(label=totalTime), vjust=-0.3, size=3.5) +
      ggtitle("") +
      xlab("Watching Date") +
      ylab("Runtime in Minutes")
    
  })
  
  #output Plots for Episode
  output$episodePlot <- renderPlot({
    totalEpisodePlot() %>%
      ggplot(mapping = aes(WatchingDate, episodesCount)) +
      geom_bar(aes(fill = Titles), position = position_stack(), stat = "identity") +
      xlab("Watching Date") +
      ylab("Episodes Count")
    
  })
  
  #output Plots for Genre
  output$genrePlot <- renderPlot({
    genrePlot() %>%
      ggplot(mapping = aes(WatchingDate, totalTime)) +
      geom_point(aes(color = Genre)) +
      facet_wrap(~Genre, nrow = 5) +
      xlab("Watching Date") +
      ylab("Runtime in Minutes") +
      theme_bw()
  })
  
  #output Plots for Specific Genre
  output$specificGenrePlot <- renderPlot({
    specificGenrePlot() %>%
      ggplot(mapping = aes(WatchingDate, totalTime)) +
      geom_bar(aes(fill = Titles), position = position_stack(), stat = "identity") +
      xlab("Watching Date") +
      ylab("Runtime in Minutes") 
  })
  
  #output Plots for GenreTime
  output$genreTimePlot <- renderPlot({
    genreTimePlot() %>%
      ggplot(mapping = aes(WatchingDate, Genre)) +
      geom_point(aes(color = Type, size = Runtime), alpha = 0.4) +
      xlab("Watching Date") +
      ylab("Genre") 
  })
  
  #output Plots for Season
  output$seasonPlot <- renderPlot({
    seasonPlot() %>%
      ggplot(mapping = aes(WatchingDate, episodesCount)) +
      geom_bar(aes(fill = season), position = position_stack(), stat = "identity") +
      xlab("Watching Date") +
      ylab("Episodes Count")
  })
  
  #output Plots for Summary
  output$summaryPlot <- renderPlot({
    summaryPlot() %>%
      ggplot(mapping = aes(WatchingDate, totalTime)) +
      geom_bar(aes(fill = Type), position = position_stack(), stat = "identity") +
      xlab("Watching Date") +
      ylab("Runtime in Minutes")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

