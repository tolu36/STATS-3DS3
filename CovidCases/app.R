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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Confirmed COVID Cases"),

    # Sidebar with a slider input for number of bins 
    dateInput('starting_date',
              label = 'Starting date in yyyy-mm-dd format',
              value = as.character("2021-02-01"),
              min = "2020-01-22", max = Sys.Date(),
              format = "yyyy-mm-dd",
              startview = 'year', language = 'en', weekstart = 1
    ),
    
    plotOutput(outputId = "time_series_plot" 
          
    ) 
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    output$time_series_plot <- renderPlot({
        
        # You can use the R commands written from (2.1) to (2.6)
        
        covid_data<-as_tibble(read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
        
        covid_data<-dplyr::rename(covid_data,Province = "Province/State", Country = "Country/Region")
        
        canada_data<-dplyr::filter(covid_data,Country=="Canada")
        
        canada_data<-dplyr::filter(canada_data,!Province %in% c("Diamond Princess", "Grand Princess", "Repatriated Travellers"))
        
        canada_data<-tidyr::gather(canada_data, key = "date", value = "confirmed_cases", -Province, -Country, -Lat, -Long)
        
        canada_data<-dplyr::mutate(canada_data,date=as.Date(date,format="%m/%d/%y"))
        
        canada_data <- filter(canada_data, date >= as.Date(as.character(input$starting_date)))
        
        ggplot(canada_data, aes(x=date, y=confirmed_cases, group=Province,color=Province))+ geom_line(aes(color=Province))+xlab("Date")+ylab("Confirmed Cases")
    })
}

# Run the app
shinyApp(ui = ui, server = server)
