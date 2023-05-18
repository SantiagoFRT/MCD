#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library('dplyr') # Para manejo del dataframe
library("readxl") # Para descarga de archivo xls
library("tibble") # Para manejo de dataframe
library("ggplot2")
library(scatterPlotMatrix)
# xls files
data <- read_excel("TB4_2.xlsx")
data2 <- read_excel("TB4_3.xlsx")

# arreglo de nombres por clave
especies <- data2$cv
names(especies) = data2$Planta

# entidades_a_escoger <- catalogo_estatal$cve_ent[-c(1, 34:36)]
# names(entidades_a_escoger) = catalogo_estatal$entidad[-c(1, 34:36)]


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("Matriz de gráficos de correlación para especies de polen"),

    # Sidebar with a slider input for number of bins 
  # selectInput("X", label = "Clave de especies",  choices = c(1:117)),
  # 
  # selectInput("Y", label = "Especie",  choices = sort(names(data[,2:118]))),
  
  sliderInput(inputId = "range",
              label = "Especies",
              min = 1,
              max = 118,
              value = c(1,117)),
  
  scatterPlotMatrixOutput("spMatrix"),
  
  # selectInput("Y", label = "Egresos: Descripción de categoría",choices = names(EgreSon1321[,4:49])),
  # 
  
  # 
  # plotlyOutput(outputId = "distPlot")
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$spMatrix <- renderScatterPlotMatrix({
    
    renderScatterPlotMatrix(data)
  
  })
  
  observeEvent(input$range, {
    scatterPlotMatrix::setCategoricalColorScale("spMatrix", input$range)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
