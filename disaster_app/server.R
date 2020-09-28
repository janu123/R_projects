library(shiny)
library(twitteR)
library(dplyr)
library(ggmap)
library(readxl)
library(plotly)
library(leaflet)
library(crosstalk)
library(gtrendsR)
library(reshape2)
library(ggplot2)
consumer.key <- "kvCW1rb3PTOOu8G4aNkdQ8Lz3"
consumer.secret <- "tbwqUg8Kob3CngEcyJEbX7M6PRa3bpw9owoaDalZbx1w4twCHB"
access.token <- "875599623900889088-zbxyoG6bg4Mu6GhsJQKJPMj6ABfCYhG"
token.secret <- "DoNELrgx9JO51DEeQ0kdjWJng71npKdRdYlRJJzz0MT1g"
setup_twitter_oauth(consumer.key, consumer.secret, access.token, token.secret)
tweets <- searchTwitter('#gajacyclone',n=100000,lang="en",
                        geocode='10.7,78.7,1000mi')
strip_retweets(tweets)                                  
tweets.df <-twListToDF(tweets)
users <- lookupUsers(tweets.df$screenName)
users_df <- twListToDF(users)
users_df<-users_df %>% filter(!location=="")

location<-users_df$location
location<-gsub("Chennai","Chennai",location)
location<-gsub("India, Tamil Nadu, Vellore","Vellore",location)
location<-gsub("Salem, India","Salem",location)
location<-gsub("Tiruchirapalli, India","Tiruchirapalli",location)
location<-gsub("Coimbatore, India","Coimbatore",location)
location<-gsub("Lusaka | Zambia | Africa","Zambia",location)
location<-gsub("Sivakasi, India","Sivakasi",location)
location<-gsub("Pondicherry, India","Pondicherry",location)
location<-gsub("Bengaluru South, India","Bengaluru",location)
location<-gsub("Hyderabad, India","Hyderabad",location)
location<-gsub("Dharmapuri, India","Dharmapuri",location)
location<-gsub("Nagercoil ,India","Nagercoil",location)
location<-gsub("Pattukkottai, India","Pattukottai",location)
location<-gsub("Madurai North, India","Madurai",location)
location<-gsub("Nagapattinam, India","Nagapattinam",location)
location<-gsub("Theni , India","Theni",location)
users_df$location<-location
coords <- read_excel("coords.xlsx")
users_df<-merge(users_df,coords,by.x = "location")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$geotweets <- renderLeaflet({
    map <- leaflet(users_df) %>% 
      addTiles() %>% 
      addCircleMarkers(data=users_df, lng=~long , lat=~lat, radius=8 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red")
  })
  output$gt<-renderPlotly({
    keywords=c("gajapuyal","Gajacyclone")
    #set the geographic area: DE = Germany
    country=c('IN')
    #set the time window
    time=("2018-11-01 2018-11-30")
    #set channels 
    channel='web'
    trends = gtrends(keywords, gprop =channel,geo=country, time = time )
    #select only interst over time 
    time_trend=trends$interest_over_time
    ggplot(data=time_trend, aes(x=date, y=hits,group=keyword,col=keyword))+
      geom_line()+xlab('Time')+ylab('Relative Interest')+ theme_bw()+
      theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+ggtitle("Google Search Volume")
  })
  output$Analysis<-renderPlot({
    plot_ly(users_df, x = ~statusesCount, y = ~listedCount) %>% 
      add_markers(alpha = 0.5) %>%
      highlight("plotly_selected", dynamic = TRUE)
    })
  })
  
