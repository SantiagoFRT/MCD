library(rcartocolor)
library(leaflet)
library(sf)
library(terra)
library(readxl)
library(tidyverse)
library(htmltools)
library(htmlwidgets)
library(rgdal)
library(rgeos)
library(rcartocolor)
library(leaflet)
library(magrittr)
library(readr)
library(highcharter)
library(dplyr)
library(colorspace)
library(RColorBrewer)
library(monochromeR)
library(janitor)
library(bsicons)
library(forecast)


Mex_pop_9020 <- read_excel("Mex_pop_9020.xlsx") %>% 
  mutate(Grupo= NULL)
View(Mex_pop_9020)


forecast(Mex_pop_9020)

# https://www.geeksforgeeks.org/time-series-and-forecasting-using-r/