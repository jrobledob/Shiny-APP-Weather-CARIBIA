library(readr)
library(chron)
library(lubridate)
library(dplyr)
library(ggplot2)
library(officer)
library(export)

# Read data


#urlfile="https://raw.githubusercontent.com/jrobledob/Shiny-APP-Weather-CARIBIA/main/Data/weather.csv"
#weather<-read_csv2(url(urlfile))


weather<- read.csv2("./Data/weather.csv", encoding = "UTF-8")
weather$Fecha<- as.POSIXct(weather$Fecha)

#---------------
input<- list()
input$frequency<- "week"
varibles<- dput(colnames(weather)[2:ncol(weather)])
input$variable<- varibles[16]
input$start<- as.POSIXct("2011-10-11")
input$end<- as.POSIXct("2021-03-02")
#---------------








selectedData <-function(){
  weather1<- weather %>%
    filter(Fecha>=input$start&Fecha<=input$end)%>%
    group_by(Date = floor_date(Fecha, input$frequency)) %>%
    summarize_at(.vars = input$variable, .funs = mean)
  print(weather1)
  weather1
}



ggplot(selectedData(),aes(x = Date,y = get(input$variable))) + 
  geom_point(aes(colour = get(input$variable))) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = median(unlist(selectedData()[,which(colnames(selectedData())==input$variable)]), na.rm = T)) + 
  geom_smooth(color = "red",size = 1) +
  scale_y_continuous(limits = c(min(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])),max(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])))) +
  ggtitle (paste(input$variable, "promedio por", input$frequency, "de \n", input$start, "a", input$end)) +
  xlab("Fecha") +  ylab(paste(input$variable, "promedio"))
graph2ppt(file="./ggplot2_plot.pptx", width=6, height=5) 




selectedData <-function(){
  weather1<- weather %>%
    filter(Fecha>=input$start&Fecha<=input$end)%>%
    group_by(Date = floor_date(Fecha, input$frequency)) %>%
    summarize_at(.vars = input$variable, .funs = sum, na.rm=T)
  print(weather1)
  weather1
}


ggplot(selectedData(),aes(x = Date,y = get(input$variable))) + 
  geom_bar(stat="identity", aes(colour = get(input$variable))) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = median(unlist(selectedData()[,which(colnames(selectedData())==input$variable)]), na.rm = T)) + 
  scale_y_continuous(limits = c(min(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])),max(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])))) +
  ggtitle (paste("Sumatoria de", input$variable,"por", input$frequency, "de \n", input$start, "a", input$end)) +
  xlab("Fecha") +  ylab(paste(input$variable, "acumulada por", input$frequency))
