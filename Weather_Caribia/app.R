# Import libraries
library(shiny)
library(shinythemes)
library(readr)
library(chron)
library(lubridate)
library(dplyr)
library(ggplot2)



# Read data
#urlfile="https://raw.githubusercontent.com/jrobledob/Shiny-APP-Weather-CARIBIA/main/Data/weather.csv"
#weather<-read_csv2(url(urlfile))
weather<- read.csv2("D:/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/R_scripts/Shiny-APP-Weather-CARIBIA/Data/weather.csv")
weather$Fecha<- as.POSIXct(weather$Fecha)
colnames(weather)<- c("Fecha", "Temperatura (°C)", "Máxima temperatura (°C)", "Mínima temperatura (°C)", 
                      "Humedad Relativa (%)", "Punto de Rocío (°C)", "Velocidad del Viento (m/s)", 
                      "Dirección del viento", "Monto de viento (Km)", "Velocidad del viento mas alta (m/s)", 
                      "Dirección del viento predominante", "Sensación térmica", "Índice de calor", 
                      "Índice THW", "Índice THSW", "Presión Barométrica (mBar)", "Lluvia (mm)", 
                      "Lluvia corregida (mm)", "Intensidad de la lluvia (mm/h)", "Radiación solar (Watts/m2)", 
                      "Energía Solar (Langleys)", "Radiación solar máxima (Watts/m2)", 
                      "Índice UV", "Dosis de Radiación UV", "UV máxima", "Grados día de Calor", 
                      "Grados día de enfriamiento", "Temperatura interna (°C)", "Humedad relativa interna (%)", 
                      "Punto de rocio en el interior (°C)", "Índice de calor en el interior", 
                      "In.EMC", "In.Air.Density", "ET", "Wind.Samp", "Wind.Tx", "ISS.Recept", 
                      "Arc..Int.", "Días de LLuvia", "Bulbo Húmedeo (°C)")
weatherVariables<- colnames(weather[,2:ncol(weather)])



ui <- fluidPage(theme = shinytheme("united"),
                # Page header
                headerPanel('Clima C.I. Caribia'),
                # Input values
                sidebarPanel(
                    HTML("<h3>Criterios de filtro</h3>"),
                    dateInput("start", label = ("Fecha de inicio"), value = "2014-01-01"),
                    dateInput("end", label = ("Fecha de terminación"), value = "2015-01-01"),
                    selectInput("frequency", label = "Espacio de Tiempo:", 
                                choices = list("Cada Hora" = "hour",
                                               "Diario" = "day",
                                               "Semanal"="week",
                                               "Mensual" = "month",
                                               "Bimensual"="bimonth",
                                               "Trimestral" ="quarter",
                                               "Semestral" = "halfyear", 
                                               "Anual"= "year"), 
                                selected = "Cada Hora"),
                    selectInput("variable", label = "Variable", weatherVariables,
                                selected = weatherVariables[1]),
                    downloadLink('downloadPlot', 'Descargar Gráfica'),
                    downloadLink('downloadData', 'Descargar Tabla')),
                mainPanel(
                    plotOutput('plot1')
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
    
    selectedData <- reactive({
        if(input$variable=="Lluvia (mm)"){
            weather1<- weather %>%
                filter(Fecha>=input$start&Fecha<=input$end)%>%
                group_by(Date = floor_date(Fecha, input$frequency)) %>%
                summarize_at(.vars = input$variable, .funs = sum, na.rm=T)
            print(weather1)
            weather1 
        }else{
            weather1<- weather %>%
                filter(Fecha>=input$start&Fecha<=input$end)%>%
                group_by(Date = floor_date(Fecha, input$frequency)) %>%
                summarize_at(.vars = input$variable, .funs = mean, na.rm=T)
            print(weather1)
            weather1
        }
    })
    
    figure<- reactive({
        if(input$variable=="Lluvia (mm)"){
            ggplot(selectedData(),aes(x = Date,y = get(input$variable))) + 
                geom_bar(stat="identity", aes(colour = get(input$variable))) +
                scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = median(unlist(selectedData()[,which(colnames(selectedData())==input$variable)]), na.rm = T)) + 
                scale_y_continuous(limits = c(min(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])),max(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])))) +
                ggtitle (paste("Sumatoria de", input$variable,"por", input$frequency, "de \n", input$start, "a", input$end)) +
                xlab("Fecha") +  ylab(paste(input$variable, "acumulada por", input$frequency))
        }else{
            ggplot(selectedData(),aes(x = Date,y = get(input$variable))) + 
                geom_point(aes(colour = get(input$variable))) +
                scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = median(unlist(selectedData()[,which(colnames(selectedData())==input$variable)]), na.rm = T)) + 
                geom_smooth(color = "red",size = 1) +
                scale_y_continuous(limits = c(min(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])),max(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])))) +
                ggtitle (paste(input$variable, "promedio por", input$frequency, "de \n", input$start, "a", input$end)) +
                xlab("Fecha") +  ylab(paste(input$variable, "promedio"))
        }
    })
    
    output$plot1 <- renderPlot({
        figure()
    })
    
    output$downloadPlot <- downloadHandler(
          filename = function() {
            paste('data-', Sys.Date(), '.pdf', sep='')
          },
          content = function(file) {
              pdf(file)
              print(figure())
              dev.off()
          }
        )
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
            write.csv(as.data.frame(selectedData()), file ,row.names = F)
        }
    )
    
    
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)