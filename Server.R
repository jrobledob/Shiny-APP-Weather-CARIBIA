library(readr)
library(tidyverse)
library(chron)
# Read data
urlfile="https://raw.githubusercontent.com/jrobledob/Shiny-APP-Weather-CARIBIA/main/Data/weather.csv"
weather<-read_csv2(url(urlfile), na = c("", "NA", "NULL"))
weather$TiempoSys<- strptime(weather$TiempoSys,format='%d/%m/%Y %H:%M')