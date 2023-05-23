
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### Cargar librerías

library(shiny)
library(flexdashboard)
library(readxl)
library(tidyverse)
library(readr)
library(plotly)
library(ggmap)
library(janitor) #limpiar variables
library(stringi) # limpiar df
library(ggthemes)

#### Llamamos a la base de datos ####

library(readr)
EgreSon1321 <- read_csv("data/Egre/EgreSon1321.csv")
EgreSon1321_L <- read_csv("data/Egre/EgreSon1321_L.csv")

EgreSon1321$X11000.Remuneraciones.al.personal
# Define UI ----

ui <- fluidPage(
  
  titlePanel("Finanzas municipales de Sonora: 2013-2021"),
  
  # sidebarLayout(
  #       sidebarPanel(),
  #       mainPanel(
  #         img(src = "ISAF.jpg", height = 200, width = 400)
  #       )
  #     ),
  
  #selectInput("mun", label = "Año",  choices = c(2013:2021)),
  
  selectInput("X", label = "Municipio",  choices = sort(unique(EgreSon1321$Municipio))),
  
  selectInput("EgreSon1321", label = "Ingresos: Descripción por categoría" ,choices = names(EgreSon1321[,4:49])),
  
  selectInput("Y", label = "Egresos: Descripción de categoría",choices = names(EgreSon1321[,4:49])),
  
  sliderInput(inputId = "range",
              label = "Ejercicio",
              min = 2013,
              max = 2021,
              value = c(2013,2021)),
  
  plotlyOutput(outputId = "distPlot")
  
  
  # verbatimTextOutput("graph"),
  # 
  # tableOutput("table")
  
)


# ui <- fluidPage(
#   titlePanel("Finanzas municipales de Sonora: 2013-2021"),
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       img(src = "ISAF.jpg", height = 200, width = 400)
#     )
#   )
# )


server <- function(input, output) {
  
  output$distPlot<- renderPlotly({
    
    ggplot(data  = subset(EgreSon1321_L, Municipio==input$X & Concepto==input$Y), 
           aes(x= Año, y= Valor)) +
      geom_point(color= "steelblue", size= 2) +
      geom_line(color = alpha("steelblue", 0.5), size= 0.8) +
      scale_x_continuous(limits = c(input$range[1],input$range[2]),
                         breaks = seq(from = input$range[1], to = input$range[2],
                                      by = 1)) +
      scale_y_continuous(label = scales::dollar_format()) +
      theme_light() +
      labs(x = "Ejercicio", y = "Pesos MX", 
           title = paste0(input$X, ": ", input$Y)) + 
      theme(plot.title = element_text(face = "bold")) +
      theme(plot.title=element_text(hjust=0.5))
  }) 
  
  
}

#server <- function(input, output, session) {
# output$graph <- renderPrint({
#   EgreSon1321 <- get(input$EgreSon1321, EgreSon1321)
#   graph(EgreSon1321)
# })

# output$table <- renderTable({
#   EgreSon1321 <- get(input$EgreSon1321, EgreSon1321)
#   EgreSon1321
# })


# server <- function(input, output, session) {
#   # Create a reactive expression
#   EgreSon1321 <- reactive({
#     get(input$EgreSon1321)
#   })
#   
#   output$summary <- renderPrint({
#     # Use a reactive expression by calling it like a function
#     summary(EgreSon1321())
#   })
#   
#   output$table <- renderTable({
#     EgreSon1321()
#   })
# }



# Run the app ----
shinyApp(ui = ui, server = server)

# https://mastering-shiny.org/basic-app.html