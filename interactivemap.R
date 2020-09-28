
library(sf)
india <-st_read("C:/Users/Janani Selvaraj/Documents/New folder (4)/IND_adm/IND_adm2.shp")
plot(india)
head(india)
tamilnadu<-filter(india, NAME_1=="Tamil Nadu")
plot(tamilnadu)

library(leaflet)
leaflet(tamilnadu) %>% addPolygons(fillColor = tamilnadu$NAME_3,
                                   weight = 2,
                                   opacity = 1,
                                   color = "white",
                                   dashArray = "3",
                                   fillOpacity = 0.7,
                                   highlight = highlightOptions(
                                     weight = 5,
                                     color = "#666",
                                     dashArray = "",
                                     fillOpacity = 0.7,
                                     bringToFront = TRUE),
                                   labelOptions = labelOptions(
                                     style = list("font-weight" = "normal", padding = "3px 8px"),
                                     textsize = "15px",
                                     direction = "auto")) 

qpal <- colorQuantile(rev(viridis::viridis(5)),
                      merged$Males,n=5)
leaflet(merged, options =
         leafletOptions(attributionControl = FALSE, minzoom=1.5)) %>%
  addPolygons(
    label=~stringr::str_c(
      NAME_2, ' ',
      formatC(Males, big.mark = ',', format='d')),
    labelOptions= labelOptions(direction = 'auto'),
    weight=1,color='#333333', opacity=1,
    fillColor = ~qpal(Males), fillOpacity = 1,
    highlightOptions = highlightOptions(
      color='#000000', weight = 2,
      bringToFront = TRUE, sendToBack = TRUE)
  ) %>% addLegend(
    "topright", pal = qpal, values = ~Males,
    title = htmltools::HTML("Male Population"),
    opacity = 1 )
data1<-workers_census %>% filter(TRU=="Total")%>%filter(`Age-Group`=="5-9")
merged<-merge(tamilnadu,data1,by.x="NAME_2",by.y="Area")
