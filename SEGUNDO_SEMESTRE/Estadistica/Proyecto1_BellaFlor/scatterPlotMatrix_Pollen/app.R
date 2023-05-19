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



if(interactive()) {
  library(shiny)
  library(scatterPlotMatrix)
  
# UI ----
  ui <- fluidPage(
    titlePanel("Matriz de gráficos de disperción por especie de polen"),
    p("Las barras grises horizontal y vertical son usadas para determinar la dimensión de la matriz"),
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
}



# Fuente https://cran.r-project.org/web/packages/scatterPlotMatrix/vignettes/introduction-to-scatterplotmatrix.html


# Ejemplo ----

# data2 <- read_excel("TB4_3.xlsx")
# 
# # arreglo de nombres por clave
# # especies <- data2$cv
# # names(especies) = data2$Planta


# entidades_a_escoger <- catalogo_estatal$cve_ent[-c(1, 34:36)]
# names(entidades_a_escoger) = catalogo_estatal$entidad[-c(1, 34:36)]

# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#   titlePanel("Matriz de gráficos de correlación para especies de polen"),
# 
#   # selectInput("X", label = "Clave de especies",  choices = c(1:117)),
#   #
#   # selectInput("Y", label = "Especie",  choices = sort(names(data[,2:118]))),
# 
#   sliderInput(inputId = "range",
#               label = "Especies",
#               min = 1,
#               max = 117,
#               value = range(especies)),
# 
#   scatterPlotMatrixOutput(outputId = "spMatrix"),
# 
#   # selectInput("Y", label = "Egresos: Descripción de categoría",choices = names(EgreSon1321[,4:49])),
#   #
# 
#   #
#   # plotlyOutput(outputId = "distPlot")
# 
# 
# )
# 
# server <- function(input, output) {
# 
#   output$spMatrix <- renderScatterPlotMatrix({
# 
#     renderScatterPlotMatrix(data)
# 
#   })
# 
#   observeEvent(input$range, {
#     scatterPlotMatrix::setCategoricalColorScale("spMatrix", input$range)
#   })
# 
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)
