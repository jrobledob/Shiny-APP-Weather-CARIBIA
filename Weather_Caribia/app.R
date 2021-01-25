# Import libraries
library(shiny)
library(shinythemes)
library(readr)
library(chron)
library(lubridate)
library(dplyr)
library(ggplot2)


# Read data
urlfile="https://raw.githubusercontent.com/jrobledob/Shiny-APP-Weather-CARIBIA/main/Data/weather.csv"
weather<-read_csv2(url(urlfile), na = c("", "NA", "NULL"))
weather$TiempoSys<- strptime(weather$TiempoSys,format='%d/%m/%Y %H:%M')
weatherVariables<- colnames(weather[,5:ncol(weather)])



ui <- fluidPage(theme = shinytheme("united"),
                # Page header
                headerPanel('Clima C.I. Caribia'),
                # Input values
                sidebarPanel(
                    HTML("<h3>Criterios de filtro</h3>"),
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
                                selected = weatherVariables[1])),
                mainPanel(
                    plotOutput('plot1')
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
    
    
    selectedData <- reactive({
        weather1<- weather %>%
            group_by(Date = floor_date(TiempoSys, input$frequency)) %>%
            summarize_at(.vars = input$variable, .funs = mean)
        weather1$Date<- as.POSIXct(weather1$Date)
        print(weather1)
        weather1
    })
    
    output$plot1 <- renderPlot({
            ggplot(selectedData(),aes(x = Date,y = get(input$variable))) + 
                geom_point(aes(colour = get(input$variable))) +
                scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = median(unlist(selectedData()[,which(colnames(selectedData())==input$variable)]))) + 
                geom_smooth(color = "red",size = 1) +
                scale_y_continuous(limits = c(min(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])),max(unlist(selectedData()[,which(colnames(selectedData())==input$variable)])))) +
                ggtitle (paste("average temperature by",input$frequency)) +
                xlab("Date") +  ylab ("Average Temperature ( ºC )")
    })
    
    

}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)