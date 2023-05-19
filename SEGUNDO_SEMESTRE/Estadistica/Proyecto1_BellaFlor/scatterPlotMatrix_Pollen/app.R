# Exposición de estadística: propuesta a estadística inferencial para el proyecto de Bella Flor
# Santiago Francisco Robles Tamayo
# Sábado 20 de mayo, 2023

#install.packages('rsconnect')
#library('rsconnect')
library(shiny)
library('dplyr') # Para manejo del dataframe
library("readxl") # Para descarga de archivo xls
library("tibble") # Para manejo de dataframe
library("ggplot2")
library(scatterPlotMatrix)

# xls files
#data <- read_excel("TB4_2.xlsx")
library(readr)
data <- read.csv("TB4_2.csv",  encoding = 'UTF-8')

# conectar aplicacion a shiny apps

# library(rsconnect)
# rsconnect::deployApp("app.R")

# if(interactive()) {
#   library(shiny)
#   library(scatterPlotMatrix)
  
# UI ----
  ui <- fluidPage(
    titlePanel("Matriz de gráficos de disperción por especie de polen"),
    
    mainPanel(
      h6("Santiago Francisco Robles Tamayo"),
      h6("Maestría en Ciencia de Datos, UNISON")
    ),
    
    p("Los controles de las barras grises, horizontal y vertical, pueden expandir la dimensión de la matriz o
      seleccionar qué dimensiones visualizar.", ),
   
  
    scatterPlotMatrixOutput("spMatrix")
  )

  # Server ----
  server <- function(input, output, session) {
    output$spMatrix <- renderScatterPlotMatrix({
      
      scatterPlotMatrix(data[,2:118], 
                        #corrPlotType = "Text",
                        # regressionType = 1,
                        #distribType = 1,
                        
                        slidersPosition = list(
        dimCount = 4, # Number of columns to draw
        xStartingDimIndex = 1, # Index of first drawn column horizontally
        yStartingDimIndex = 1 # Index of first drawn column vertically
        ),
        plotProperties = list(
          noCatColor = "DarkCyan", # Color used when categories coloring is not applied
          point = list(
            alpha = 0.3, # Opacity value used for points
            radius = 4 # Radius used to draw points as circles
          )
        ),
        zAxisDim = "Abies", 
        continuousCS = "YlOrRd",
        controlWidgets = TRUE
        #rotateTitle = TRUE
        
    )
      
    })
    
  }

# ShinyApp ----
  shinyApp(ui, server)











# Fuente https://cran.r-project.org/web/packages/scatterPlotMatrix/vignettes/introduction-to-scatterplotmatrix.html

