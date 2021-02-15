library(lubridate)
library(plyr)
library(dplyr)

#Cleaning of weather Data

#Cleaning 2011 - 2013----
#Delete non-importatn columns
#Reading Data
WDB2011_2013<- read.csv2("./Data/Raw_Data/2011_2013.csv")
WDB2011_2013<- WDB2011_2013[-1,-c(1,2)]
#Introduce correct input in absent values 
WDB2011_2013[WDB2011_2013=="---"]<- "NA"
WDB2011_2013[WDB2011_2013=="------"]<- "NA"
WDB2011_2013[WDB2011_2013<0]<- "NA"
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Out.Temp", "Hi.Temp", "Low.Temp", "Out.Hum", 
                      "Dew.Point", "Wind.speed..m.s.", "Wind.Run", 
                      "Hi.Speed", "Wind.Chill", "Heat.Index", "THW.Index", 
                      "THSW.Index", "PresiÃ³n.BaromÃ.trica..mb.", "Rain", "LLUVIA.CORREGIDO..mm.", 
                      "Rain.Rate", "Solar.Rad", "Solar.Energy", "Hi.Solar.Rad", "UV.Index", 
                      "UV.Dose", "Hi.UV", "HEAT.D.D", "Cool.D.D", "In.Temp", "In.Hum", 
                      "In.Dew", "In.Heat", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
                      "Wind.Tx", "ISS.Recept", "Arc..Int.")
for (i in 1:length(numeric_variables)) {
  WDB2011_2013[,numeric_variables[i]]<- gsub(",",".",WDB2011_2013[,numeric_variables[i]])
  WDB2011_2013[,numeric_variables[i]]<- as.numeric(WDB2011_2013[,numeric_variables[i]])
}
#Reformat date and time (which are characters) to POSIXct
WDB2011_2013$Time<- gsub("(.)\\,?[Mm]\\,?","\\1m",WDB2011_2013$Time)
WDB2011_2013$Date<- as.character(interaction(WDB2011_2013$Date, WDB2011_2013$Time, sep = " "))
WDB2011_2013<- WDB2011_2013[,-2]
WDB2011_2013$Date<- parse_date_time(WDB2011_2013$Date,"%d/%m/%Y %I:%M:%S %p")
colnames(WDB2011_2013)<- c("Fecha", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
  "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
  "Dirección del viento predominante", "Sensación térmica", "Índice de calor", "Índice THW", "Índice THSW", 
  "Presión Barométrica (mBar)", "Lluvia (mm)", "Lluvia corregida (mm)", 
  "Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", "Índice UV", 
  "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento", "Temperatura interna (°C)", "Humedad relativa interna (%)", 
  "Punto de rocio en el interior (°C)", "Índice de calor en el interior", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
  "Wind.Tx", "ISS.Recept", "Arc..Int.")
DB_weather_Caribia<- WDB2011_2013










#Cleaning 2014----
#Delete non-importatn columns
#Reading Data
WDB2014<- read.csv2("./Data/Raw_Data/2014.csv")
WDB2014<- WDB2014[-1,-c(1,2)]
#Introduce correct input in absent values 
WDB2014[WDB2014=="---"]<- "NA"
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Temp", "Hi", "Low", "Out", "Dew", "Wind", 
                      "Wind.2", "Hi.1", "Wind.3", "Heat", "THW", "THSW", "X.3", 
                      "X.4", "Rain", "Solar", "Solar.1", "Hi.Solar", "UV", "UV.1", 
                      "Hi.3", "Heat.1", "Cool", "In", "In.1", "In.2", "In.3", "X.5", 
                      "Wind.4", "Wind.5", "ISS", "Arc.")
for (i in 1:length(numeric_variables)) {
  WDB2014[,numeric_variables[i]]<- gsub(",",".",WDB2014[,numeric_variables[i]])
  WDB2014[,numeric_variables[i]]<- as.numeric(WDB2014[,numeric_variables[i]])
}
#Reformat date and time (which are characters) to POSIXct
WDB2014$X.1<- as.character(interaction(WDB2014$X.1, WDB2014$X.2, sep = " "))
WDB2014<- WDB2014[,-2]
WDB2014$X.1<- parse_date_time(WDB2014$X.1,"%d/%m/%Y %H:%M:%S")
colnames(WDB2014)<- c("Fecha", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
                           "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                           "Dirección del viento predominante", "Sensación térmica", "Índice de calor", "Índice THW", "Índice THSW", 
                           "Presión Barométrica (mBar)", "Lluvia (mm)","Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", "Índice UV", 
                           "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento", "Temperatura interna (°C)", "Humedad relativa interna (%)", 
                           "Punto de rocio en el interior (°C)", "Índice de calor en el interior","ET", "Wind.Samp", 
                           "Wind.Tx", "ISS.Recept", "Arc..Int.")

DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2014)








































c("Date", "Out.Temp", "Hi.Temp", "Low.Temp", "Out.Hum", "Dew.Point", 
  "Wind.speed..m.s.", "Wind.direction", "Wind.Run", "Hi.Speed", 
  "Hi.Dir", "Wind.Chill", "Heat.Index", "THW.Index", "THSW.Index", 
  "PresiÃ³n.BaromÃ.trica..mb.", "Rain", "LLUVIA.CORREGIDO..mm.", 
  "Rain.Rate", "Solar.Rad", "Solar.Energy", "Hi.Solar.Rad", "UV.Index", 
  "UV.Dose", "Hi.UV", "HEAT.D.D", "Cool.D.D", "In.Temp", "In.Hum", 
  "In.Dew", "In.Heat", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
  "Wind.Tx", "ISS.Recept", "Arc..Int.")

c("Fecha", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
  "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
  "Dirección del viento predominante", "Sensación térmica", "Índice de calor", "Índice THW", "Índice THSW", 
  "Presión Barométrica (mBar)", "Lluvia (mm)", "Lluvia corregida (mm)", 
  "Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", "Índice UV", 
  "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento", "Temperatura interna (°C)", "Humedad relativa interna (%)", 
  "Punto de rocio en el interior (|C)", "Índice de calor en el interior", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
  "Wind.Tx", "ISS.Recept", "Arc..Int.")

#Cleaning 2015----
#Delete non-importatn columns
#Reading Data
WDB2015<- read.csv2("./Data/Raw_Data/2015.csv")
WDB2015<- WDB2015[-1,-c(1,2)]
#Introduce correct input in absent values 
WDB2015[WDB2015=="---"]<- "NA"
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Temp", "Hi", "Low", "Out", "Dew", "Wind", 
                      "Wind.2", "Hi.1", "Wind.3", "Heat", "THW", "THSW", "X.3", 
                      "X.4", "Rain", "Solar", "Solar.1", "Hi.Solar", "UV", "UV.1", 
                      "Hi.3", "Heat.1", "Cool", "In", "In.1", "In.2", "In.3", "In.4", 
                      "In.Air", "X.5", "Wind.4", "Wind.5", "ISS", "Arc.")
for (i in 1:length(numeric_variables)) {
  WDB2015[,numeric_variables[i]]<- gsub(",",".",WDB2015[,numeric_variables[i]])
  WDB2015[,numeric_variables[i]]<- as.numeric(WDB2015[,numeric_variables[i]])
}
#Reformat date and time (which are characters) to POSIXct
WDB2015$X.2<- gsub("(.)\\.?[Mm]\\.?","\\1m",WDB2015$X.2)
WDB2015$X.1<- as.character(interaction(WDB2015$X.1, WDB2015$X.2, sep = " "))
WDB2015<- WDB2015[,-2]
WDB2015$X.1<- parse_date_time(WDB2015$X.1,"%d/%m/%Y %I:%M:%S %p")
colnames(WDB2015)<-  c("Fecha", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
                       "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                       "Dirección del viento predominante", "Sensación térmica", "Índice de calor", "Índice THW", "Índice THSW", 
                       "Presión Barométrica (mBar)", "Lluvia (mm)", "Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", "Índice UV", 
                       "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento", "Temperatura interna (°C)", "Humedad relativa interna (%)", 
                       "Punto de rocio en el interior (°C)", "Índice de calor en el interior", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
                       "Wind.Tx", "ISS.Recept", "Arc..Int.")

DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2015)


#Cleaning 2016----
#Delete non-importatn columns
#Reading Data
WDB2016<- read.csv2("./Data/Raw_Data/2016.csv")
WDB2016<- WDB2016[,-c(1,2)]
#Introduce correct input in absent values 
WDB2016[WDB2016=="---"]<- "NA"
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Temp.externa..Â.C.", "Max.temp..Â.C.", "Min.Temp..Â.C.", 
                      "HR..Ext", "Punto.Rocio", "Velocidad.Viento",
                      "Wind.Run", "Hi.Speed", "Wind.Chill", "Heat.Index", 
                      "THW.Index", "THSW.Index", "Presion.Bar..milibar.", "lluvia..mm.", 
                      "Dias.lluvia", "tasa.lluvia", "Radiacion.Solar..W.m2.", "EnergÃ.a.Solar..langleys.", 
                      "Hi.Solar.Rad", "UV.Index", "UV.Dose", "Hi.UV", "Heat.D.D", "Cool.D.D", 
                      "Temp..interior", "HR..int", "Punto.Rocio.int", "Heat.index.int", 
                      "EMC", "In.Air.Density", "ET..mm.unid.tiempo.", "Wind.Samp", 
                      "Wind.TX", "ISS.Recept", "Arc.Int")
for (i in 1:length(numeric_variables)) {
  WDB2016[,numeric_variables[i]]<- gsub(",",".",WDB2016[,numeric_variables[i]])
  WDB2016[,numeric_variables[i]]<- as.numeric(WDB2016[,numeric_variables[i]])
}
#Reformat date and time (which are characters) to POSIXct
WDB2016$Fecha<- as.character(interaction(WDB2016$Fecha, WDB2016$Hora, sep = " "))
WDB2016<- WDB2016[,-2]
WDB2016$Fecha<- parse_date_time(WDB2016$Fecha,"%d/%m/%Y %H:%M:%S")
colnames(WDB2016)<-  c("Fecha", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
                       "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                       "Dirección del viento predominante", "Sensación térmica", "Índice de calor", "Índice THW", "Índice THSW", 
                       "Presión Barométrica (mBar)", "Lluvia (mm)","Días de LLuvia", "Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", "Índice UV", 
                       "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento", "Temperatura interna (°C)", "Humedad relativa interna (%)", 
                       "Punto de rocio en el interior (°C)", "Índice de calor en el interior", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
                       "Wind.Tx", "ISS.Recept", "Arc..Int.")

DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2016)
#Cleaning 2017----
#Delete non-importatn columns
#Reading Data
WDB2017<- read.csv2("./Data/Raw_Data/2017.csv")
WDB2017<- WDB2017[,-c(1,2)]
#Introduce correct input in absent values 
WDB2017[WDB2017=="---"]<- "NA"
WDB2017[WDB2017==""]<- "NA"
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Temp.externa..Â.C.", "Max.temp..Â.C.", "Min.Temp..Â.C.", 
                      "HR..Ext", "Punto.Rocio", "Velocidad.Viento", 
                      "Wind.Run", "Hi.Speed", "Wind.Chill", "Heat.Index", 
                      "THW.Index", "THSW.Index", "Presion.Bar..milibar.", "LLUVIA..mm.", 
                      "tasa.lluvia", "Radiacion.Solar..W.m2.", "EnergÃ.a.Solar..langleys.", 
                      "Hi.Solar.Rad", "UV.Index", "UV.Dose", "Hi.UV", "Heat.D.D", "Cool.D.D", 
                      "Temp..interior", "HR..int", "Punto.Rocio.int", "Heat.index.int", 
                      "EMC", "In.Air.Density", "ET..mm.", "Wind.Samp", "Wind.TX", "ISS.Recept", 
                      "Arc.Int")

for (i in 1:length(numeric_variables)) {
  WDB2017[,numeric_variables[i]]<- gsub(",",".",WDB2017[,numeric_variables[i]])
  WDB2017[,numeric_variables[i]]<- as.numeric(WDB2017[,numeric_variables[i]])
}
#Reformat date and time (which are characters) to POSIXct
WDB2017$Fecha<- as.character(interaction(WDB2017$Fecha, WDB2017$Hora, sep = " "))
WDB2017<- WDB2017[,-2]
WDB2017$Fecha<- parse_date_time(WDB2017$Fecha,"%d/%m/%Y %H:%M:%S")
colnames(WDB2017)<-  c("Fecha", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
                       "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                       "Dirección del viento predominante", "Sensación térmica", "Índice de calor", "Índice THW", "Índice THSW", 
                       "Presión Barométrica (mBar)", "Lluvia (mm)","Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", "Índice UV", 
                       "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento", "Temperatura interna (°C)", "Humedad relativa interna (%)", 
                       "Punto de rocio en el interior (°C)", "Índice de calor en el interior", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
                       "Wind.Tx", "ISS.Recept", "Arc..Int.")

DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2017)

#Cleaning 2018----
#Delete non-importatn columns
#Reading Data
WDB2018<- read.csv2("./Data/Raw_Data/2018.csv")
WDB2018<- WDB2018[,-c(1,2)]
#Introduce correct input in absent values 
WDB2018[WDB2018=="---"]<- "NA"
WDB2018[WDB2018==""]<- "NA"
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Temp.externa..Â.C.", "Max.temp..Â.C.", "Min.Temp..Â.C.", 
                      "HR..Ext", "Punto.Rocio", "Velocidad.Viento..m.s.", 
                      "Wind.Run", "Hi.Speed", "Wind.Chill", "Heat.Index", 
                      "THW.Index", "THSW.Index", "Presion.Bar..milibar.", "LLUVIA..mm.", 
                      "tasa.lluvia", "Radiacion.Solar..W.m2.", "EnergÃ.a.Solar..langleys.", 
                      "Hi.Solar.Rad", "UV.Index", "UV.Dose", "Hi.UV", "Heat.D.D", "Cool.D.D", 
                      "Temp..interior", "HR..int", "Punto.Rocio.int", "Heat.index.int", 
                      "EMC", "In.Air.Density", "ET..mm.", "Wind.Samp", "Wind.TX", "ISS.Recept", 
                      "Arc.Int")
for (i in 1:length(numeric_variables)) {
  WDB2018[,numeric_variables[i]]<- gsub(",",".",WDB2018[,numeric_variables[i]])
  WDB2018[,numeric_variables[i]]<- as.numeric(WDB2018[,numeric_variables[i]])
}
#Reformat date and time (which are characters) to POSIXct
WDB2018$Fecha<- as.character(interaction(WDB2018$Fecha, WDB2018$Hora, sep = " "))
WDB2018<- WDB2018[,-2]
WDB2018$Fecha<- parse_date_time(WDB2018$Fecha,"%d/%m/%Y %H:%M:%S")
colnames(WDB2018)<-  c("Fecha", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
                       "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                       "Dirección del viento predominante", "Sensación térmica", "Índice de calor", "Índice THW", "Índice THSW", 
                       "Presión Barométrica (mBar)", "Lluvia (mm)","Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", "Índice UV", 
                       "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento", "Temperatura interna (°C)", "Humedad relativa interna (%)", 
                       "Punto de rocio en el interior (°C)", "Índice de calor en el interior", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
                       "Wind.Tx", "ISS.Recept", "Arc..Int.")
DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2018)



#Cleaning 2019_1----
#Delete non-importatn columns
#Reading Data
WDB2019_1<- read.csv2("./Data/Raw_Data/2019_1.csv", encoding= "UTF-8")
WDB2019_1<- WDB2019_1[-c(1:2),]
colnames(WDB2019_1)<- WDB2019_1[1,]
WDB2019_1<- WDB2019_1[-c(1),]
#Introduce correct input in absent values 
WDB2019_1[WDB2019_1=="---"]<- "NA"
WDB2019_1[WDB2019_1==""]<- "NA"
WDB2019_1[WDB2019_1=="--"]<- "NA"
WDB2019_1$`&`<- as.character(interaction(WDB2019_1$`&`, WDB2019_1$Time, sep = " "))
WDB2019_1<- WDB2019_1[,-3]
WDB2019_1$Date<- as.character(interaction(WDB2019_1$Date, WDB2019_1$`&`, sep = " "))
WDB2019_1<- WDB2019_1[,-2]
WDB2019_1$Date<- parse_date_time(WDB2019_1$Date,"%d/%m/%Y %I:%M %p")
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Temp ", "Temp alta ", "Temp. Baja ", 
                      "Hum ", "Punto de rocio ", "Wet Bulb ", "Velocidad del viento - m/s", 
                      "Viento Corriente", "Alta velocidad del viento", 
                      "Viento Frio", "Indice de calor", 
                      "THW Index ", "THSW  indice", "Barometro", "Lluvia", "Tasa de lluvia", 
                      "Rad Solar ", "Energia solar", "Rad Solar Alta ", "ET - mm", 
                      "El indice UV - Index", "Dosis de UV", "Alto indice UV", "Dias-grado de calentamiento", 
                      "Dias-grado de enfriamiento")
for (i in 1:length(numeric_variables)) {
  WDB2019_1[,numeric_variables[i]]<- gsub(",",".",WDB2019_1[,numeric_variables[i]])
  WDB2019_1[,numeric_variables[i]]<- as.numeric(WDB2019_1[,numeric_variables[i]])
}
WDB2019_1<- WDB2019_1[,-which(colnames(WDB2019_1)=="Wet Bulb ")]
WDB2019_1<- WDB2019_1[,-which(colnames(WDB2019_1)=="Viento Frio")]

#Reformat date and time (which are characters) to POSIXct
colnames(WDB2019_1)<-  c("Fecha", "ISS.Recept", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
                         "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                         "Dirección del viento predominante", "Índice de calor", "Índice THW", "Índice THSW", 
                         "Presión Barométrica (mBar)", "Lluvia (mm)","Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)",
                         "ET", "Índice UV", "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento") 
DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2019_1)



#Cleaning 2019_2----
#Delete non-importatn columns
#Reading Data
WDB2019_2<- read.csv2("./Data/Raw_Data/2019_2.csv", encoding= "UTF-8")
WDB2019_2<- WDB2019_2[-c(1:2),]
colnames(WDB2019_2)<- WDB2019_2[1,]
WDB2019_2<- WDB2019_2[-c(1),]
#Introduce correct input in absent values 
WDB2019_2[WDB2019_2=="---"]<- "NA"
WDB2019_2[WDB2019_2==""]<- "NA"
WDB2019_2[WDB2019_2=="--"]<- "NA"
WDB2019_2<- WDB2019_2[,-c(2:3)]
WDB2019_2$Date<- parse_date_time(WDB2019_2$Date,"%d/%m/%Y %H:%M")
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Temp ", "Temp alta ", "Temp. Baja ", 
                      "Hum ", "Punto de rocio ", "Wet Bulb ", "Velocidad del viento - m/s", 
                      "Viento Corriente", "Alta velocidad del viento", 
                      "Viento Frio", "Indice de calor", 
                      "THW Index ", "THSW  indice", "Barometro", "Lluvia", "Tasa de lluvia", 
                      "Rad Solar ", "Energia solar", "Rad Solar Alta ", "ET - mm", 
                      "El indice UV - Index", "Dosis de UV", "Alto indice UV", "Dias-grado de calentamiento", 
                      "Dias-grado de enfriamiento")
for (i in 1:length(numeric_variables)) {
  WDB2019_2[,numeric_variables[i]]<- gsub(",",".",WDB2019_2[,numeric_variables[i]])
  WDB2019_2[,numeric_variables[i]]<- as.numeric(WDB2019_2[,numeric_variables[i]])
}
WDB2019_2<- WDB2019_2[,-which(colnames(WDB2019_2)=="Wet Bulb ")]
WDB2019_2<- WDB2019_2[,-which(colnames(WDB2019_2)=="Viento Frio")]
#Reformat date and time (which are characters) to POSIXct
colnames(WDB2019_2)<-  c("Fecha", "ISS.Recept", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
                         "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                         "Dirección del viento predominante", "Índice de calor", "Índice THW", "Índice THSW", 
                         "Presión Barométrica (mBar)", "Lluvia (mm)","Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)",
                         "ET", "Índice UV", "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento") 
DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2019_2)

#Cleaning 2020_1----
#Delete non-importatn columns
#Reading Data
WDB2020_1<- read.csv2("./Data/Raw_Data/2020_1.csv", encoding= "UTF-8")
WDB2020_1<- WDB2020_1[,-c(1:3)]
#Introduce correct input in absent values 
WDB2020_1[WDB2020_1=="---"]<- "NA"
WDB2020_1[WDB2020_1==""]<- "NA"
WDB2020_1[WDB2020_1=="--"]<- "NA"
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Temp.out", "Hi.Temp", "Low.Temp", "Out.Hum", 
                      "Dew", "Wind.Speed", "Wind.Run", "Hi.Speed", 
                      "Wind.Chill", "Heat.Index", "THW.Index", "THSW.Index", "Bar", 
                      "Rain", "Rain.Rate", "Solar.Rad", "Solar.Energy", "Hi.Solar.Rad", 
                      "UV..Index", "UV..Dose", "Hi..UV", "Heat.D.D", "Cool.D.D", "In.Temp", 
                      "In.Hum", "In.Dew", "In.Heat", "In.EMC", "In.Air.Density", "ET", 
                      "Wind.Samp", "Wind.Tx", "ISS.Recept", "Arc..Int.")
for (i in 1:length(numeric_variables)) {
  WDB2020_1[,numeric_variables[i]]<- gsub(",",".",WDB2020_1[,numeric_variables[i]])
  WDB2020_1[,numeric_variables[i]]<- as.numeric(WDB2020_1[,numeric_variables[i]])
}
#Reformat date and time (which are characters) to POSIXct
WDB2020_1$Fecha<- as.character(interaction(WDB2020_1$Fecha, WDB2020_1$Hora, sep = " "))
WDB2020_1<- WDB2020_1[,-2]
WDB2020_1$Fecha<- parse_date_time(WDB2020_1$Fecha,"%d/%m/%Y %H:%M:%S")

colnames(WDB2020_1)<-  c("Fecha", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
                         "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                         "Dirección del viento predominante", "Sensación térmica", "Índice de calor", "Índice THW", "Índice THSW", 
                         "Presión Barométrica (mBar)", "Lluvia (mm)","Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", "Índice UV", 
                         "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento", "Temperatura interna (°C)", "Humedad relativa interna (%)", 
                         "Punto de rocio en el interior (°C)", "Índice de calor en el interior", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
                         "Wind.Tx", "ISS.Recept", "Arc..Int.")
DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2020_1)

#Change cardinal points to decimal----
dir <- setNames( seq(0, 337.5 , by=22.5), 
                 c("N","NNE","NE", "ENE", "E", "ESE", "SE", "SSE", 
                   "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"))
DB_weather_Caribia$`Dirección del viento`<- dir[DB_weather_Caribia$`Dirección del viento`]
DB_weather_Caribia$`Dirección del viento predominante`<- dir[DB_weather_Caribia$`Dirección del viento predominante`]
#Cleaning 2020_2----
#Delete non-importatn columns
#Reading Data
WDB2020_2<- read.csv2("./Data/Raw_Data/2020_2.csv", encoding= "UTF-8")
WDB2020_2<- WDB2020_2[,-c(1:3)]
#Introduce correct input in absent values 
WDB2020_2[WDB2020_2=="NULL"]<- "NA"
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Barometro", "Temperatura", "VelocidadViento", 
                      "DireccionViento", "HumedadRelativa", "UV", "RadiacionSolar", 
                      "Lluvia")
for (i in 1:length(numeric_variables)) {
  WDB2020_2[,numeric_variables[i]]<- gsub(",",".",WDB2020_2[,numeric_variables[i]])
  WDB2020_2[,numeric_variables[i]]<- as.numeric(WDB2020_2[,numeric_variables[i]])
}
#Reformat date and time (which are characters) to POSIXct
WDB2020_2<- WDB2020_2[, -c(10:16)]
WDB2020_2$TiempoSys<- parse_date_time(WDB2020_2$TiempoSys,"%d/%m/%Y %H:%M")

colnames(WDB2020_2)<-  c("Fecha", "Presión Barométrica (mBar)", "Temperatura (°C)", "Velocidad del Viento (m/s)", 
                         "Dirección del viento","Humedad Relativa (%)", "Índice UV", "Radiación solar (Watts/m2)", 
                          "Lluvia (mm)")
DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2020_2)





#Cleaning 2020_3----
#Delete non-importatn columns
#Reading Data
WDB2020_3<- read.csv2("./Data/Raw_Data/2020_3.csv", encoding= "UTF-8")
WDB2020_3<- WDB2020_3[,-c(1:3)]
#Introduce correct input in absent values 
WDB2020_3[WDB2020_3=="NULL"]<- "NA"
#Reformat numeric variables (which are characters) to numbers
numeric_variables<- c("Barometro", "Temperatura", "VelocidadViento", 
                      "DireccionViento", "HumedadRelativa", "UV", "RadiacionSolar", 
                      "Lluvia")
for (i in 1:length(numeric_variables)) {
  WDB2020_3[,numeric_variables[i]]<- gsub(",",".",WDB2020_3[,numeric_variables[i]])
  WDB2020_3[,numeric_variables[i]]<- as.numeric(WDB2020_3[,numeric_variables[i]])
}
#Reformat date and time (which are characters) to POSIXct
WDB2020_3<- WDB2020_3[, -c(10:16)]
WDB2020_3$TiempoSys<- parse_date_time(WDB2020_3$TiempoSys,"%d/%m/%Y %H:%M")
WDB2020_3<- arrange(WDB2020_3, TiempoSys)
colnames(WDB2020_3)<-  c("Fecha", "Presión Barométrica (mBar)", "Temperatura (°C)", "Velocidad del Viento (m/s)", 
                         "Dirección del viento","Humedad Relativa (%)", "Índice UV", "Radiación solar (Watts/m2)",
                         "Lluvia (mm)")
DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2020_3)






#Cleaning 2020_4----
#Delete non-importatn columns
#Reading Data
WDB2020_4<- read.csv2("./Data/Raw_Data/2020_4.csv", encoding = "UTF-8")
#Introduce correct input in absent values 
WDB2020_4[WDB2020_4=="--"]<- NA
WDB2020_4[WDB2020_4=="------"]<- NA
WDB2020_4[WDB2020_4=="NULL"]<- NA
WDB2020_4[WDB2020_4==""]<- NA
#Reformat numeric variables (which are characters) to numbers
WDB2020_4$Direccion.del.viento<- dir[WDB2020_4$Direccion.del.viento]
WDB2020_4$Alta.Direccion.del.viento<- dir[WDB2020_4$Alta.Direccion.del.viento]


numeric_variables<- c("Barometro...mm.Hg", "Temp...C", 
                      "Temp.Alta...C", "Temp.Baja...C", "Hum....", "Punto.de.rocio..C", 
                      "Bulbo.Humedo...C", "Velocidad.del.viento...m.s", 
                      "Viento.Corriente...m", "Alta.Direccion.del.viento", 
                      "Viento.Frio...C", "indice.de.calor...C", "THW.Index...C", "THSW..indice...C", 
                      "Lluvia...mm", "Tasa.de.lluvia...mm.h", "Rad.Solar...W.m.2", 
                      "Energía.solar...Ly", "Rad.Solar.Alta...W.m.2", "ET...mm", "El.indice.UV", 
                      "Dosis.de.UV...MEDs", "Alto.indice.UV", "Dias.grado.de.calentamiento", 
                      "Dias.grado.de.enfriamiento")
for (i in 1:length(numeric_variables)) {
  WDB2020_4[,numeric_variables[i]]<- gsub(",",".",WDB2020_4[,numeric_variables[i]])
  WDB2020_4[,numeric_variables[i]]<- as.numeric(WDB2020_4[,numeric_variables[i]])
}
WDB2020_4<- WDB2020_4[,-which(colnames(WDB2020_4)=="Bulbo.Humedo...C")]
WDB2020_4<- WDB2020_4[,-which(colnames(WDB2020_4)=="Viento.Frio...C")]
#Reformat date and time (which are characters) to POSIXct
WDB2020_4$X.<- as.character(interaction(WDB2020_4$X., WDB2020_4$Time, sep = " "))
WDB2020_4<- WDB2020_4[,-3]
WDB2020_4$X.U.FEFF.Date<- as.character(interaction(WDB2020_4$X.U.FEFF.Date, WDB2020_4$X., sep = " "))
WDB2020_4<- WDB2020_4[,-2]
WDB2020_4$X.U.FEFF.Date<- parse_date_time(WDB2020_4$X.U.FEFF.Date,"%d/%m/%Y %I:%M %p")

colnames(WDB2020_4)<- c("Fecha", "Presión Barométrica (mBar)","Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", "Humedad Relativa (%)", "Punto de Rocío (°C)", 
                        "Velocidad del Viento (m/s)", "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                        "Dirección del viento predominante", "Índice de calor", "Índice THW", "Índice THSW", 
                        "Lluvia (mm)", "Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", "ET", "Índice UV", 
                        "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", "Grados día de enfriamiento")
DB_weather_Caribia<- rbind.fill(DB_weather_Caribia,WDB2020_4)











