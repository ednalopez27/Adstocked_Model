library(shiny)
library(ggplot2)
library(tidyverse)

enlace<-'https://raw.githubusercontent.com/schubertjan/recruitmentCandidateExercise/master/data.csv'
datos<-read.csv2(file=enlace,header = TRUE, sep = ",")
datos$id<-1:nrow(datos)
datos$Media.Campaign<-factor(datos$Media.Campaign, levels = c("1","2","3"))

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Adstock Model"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("rf",
                  "Retention Factor",
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Table", tableOutput("table")),
                  tabPanel("Summary", verbatimTextOutput("summary"))
      ))))

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Generate a plot of the data ----
  
  output$plot <- renderPlot({
    adstock_rate = input$rf
    adstocked_advertising = numeric(length(datos$Media.Spend..USD.))
    adstocked_advertising[1] = datos$Media.Spend..USD.[1]
    
    for(i in 2:length(datos$Media.Spend..USD.)){
      adstocked_advertising[i] = datos$Media.Spend..USD.[i] + 
        adstock_rate*adstocked_advertising[i-1]
    }
    datos$adstocked<-adstocked_advertising
    datos
    
    ggplot(datos, aes(x=adstocked,y=Search.Volume, 
                      colour=Media.Campaign,
                      shape=Media.Campaign))+
      geom_point()+
      geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
      labs(title = "")
    
  })
  
  output$table <- renderTable({
    adstock_rate = input$rf
    adstocked_advertising = numeric(length(datos$Media.Spend..USD.))
    adstocked_advertising[1] = datos$Media.Spend..USD.[1]
    
    for(i in 2:length(datos$Media.Spend..USD.)){
      adstocked_advertising[i] = datos$Media.Spend..USD.[i] + 
        adstock_rate*adstocked_advertising[i-1]
    }
    datos$adstocked<-adstocked_advertising
    datos$efficiently.week<-datos$Media.Spend..USD./datos$Search.Volume
    
    datos$efficiently.acum<-numeric(length(datos$Media.Spend..USD.))
    for(i in 2:length(datos$Media.Spend..USD.)){
      adstocked_advertising[i] = datos$Media.Spend..USD.[i] + 
        adstock_rate*adstocked_advertising[i-1]
    }
  
    datos2<-datos %>% 
      group_by(Media.Campaign) %>% 
      summarise_at(vars(Media.Spend..USD., Search.Volume, adstocked), 
                   list(name=sum))
    datos2$efficiently<-datos2$Search.Volume_name/datos2$adstocked_name
    datos2
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    adstock_rate = input$rf
    adstocked_advertising = numeric(length(datos$Media.Spend..USD.))
    adstocked_advertising[1] = datos$Media.Spend..USD.[1]
    
    for(i in 2:length(datos$Media.Spend..USD.)){
      adstocked_advertising[i] = datos$Media.Spend..USD.[i] + 
        adstock_rate*adstocked_advertising[i-1]
    }
    datos$adstocked<-adstocked_advertising
    datos
    model<-lm(formula=Search.Volume~adstocked + Media.Campaign,datos)
    summary(model)
  })
}

# Create Shiny app ----
shinyApp(ui, server)