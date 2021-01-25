library(readr)
library(chron)
library(lubridate)
library(dplyr)
library(ggplot2)

# Read data
urlfile="https://raw.githubusercontent.com/jrobledob/Shiny-APP-Weather-CARIBIA/main/Data/weather.csv"
weather<-read_csv2(url(urlfile), na = c("", "NA", "NULL"))
weather$TiempoSys<- strptime(weather$TiempoSys,format='%d/%m/%Y %H:%M')

#---------------
input<- list()
input$frequency<- "hour"
input$variable<- "Temperatura"
#---------------



weather1<- weather %>%
  group_by(Date = floor_date(TiempoSys, input$frequency)) %>%
  summarize_at(.vars = input$variable, .funs = mean)
weather1$Date<- as.POSIXct(weather1$Date)



selectedData <-function(){
  weather1<- weather %>%
    group_by(Date = floor_date(TiempoSys, input$frequency)) %>%
    summarize_at(.vars = input$variable, .funs = mean)
  weather1$Date<- as.POSIXct(weather1$Date)
  print(weather1)
  weather1
}



ggplot(selectedData(),aes(x = Date,y = get(input$variable))) + 
  geom_point(aes(colour = get(input$variable))) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = median(unlist(selectedData()[,which(colnames(selectedData())==input$variable)]))) + 
  geom_smooth(color = "red",size = 1) +
  scale_y_continuous(limits = c(min(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])),max(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])))) +
  ggtitle (paste("average temperature by",input$frequency)) +
  xlab("Date") +  ylab ("Average Temperature ( ºC )")
