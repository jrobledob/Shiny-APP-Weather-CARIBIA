#Cleaning of weather Data

WDB2011_2013<- read.csv2("./Data/Raw_Data/2011_2013.csv")
#Cleaning 2011 - 2013
WDB2011_2013<- WDB2011_2013[-1,-c(1,2)]
WDB2011_2013[WDB2011_2013=="---"]<- "NA"
WDB2011_2013[WDB2011_2013=="------"]<- "NA"
numeric_variables<- c("Out.Temp", "Hi.Temp", "Low.Temp", "Out.Hum", 
                      "Dew.Point", "Wind.speed..m.s.", "Wind.Run", 
                      "Hi.Speed", "Wind.Chill", "Heat.Index", "THW.Index", 
                      "THSW.Index", "PresiÃ³n.BaromÃ.trica..mb.", "Rain", "LLUVIA.CORREGIDO..mm.", 
                      "Rain.Rate", "Solar.Rad", "Solar.Energy", "Hi.Solar.Rad", "UV.Index", 
                      "UV.Dose", "Hi.UV", "HEAT.D.D", "Cool.D.D", "In.Temp", "In.Hum", 
                      "In.Dew", "In.Heat", "In.EMC", "In.Air.Density", "ET", "Wind.Samp", 
                      "Wind.Tx", "ISS.Recept", "Arc..Int.")
WDB2011_2013[numeric_variables]<- gsub(",",".",WDB2011_2013[numeric_variables])
  
  
  


WDB2011_2013$Out.Temp<- as.numeric(as.character(WDB2011_2013$Out.Temp))













for (i in 1:length(numeric_variables)) {
  WDB2011_2013[,numeric_variables[i]]<- as.numeric(as.character(WDB2011_2013[,numeric_variables[i]]))
}
