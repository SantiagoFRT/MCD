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

# Datos ----

pob_son_2022 <- read_csv("pob_son_2022.csv")%>% 
  mutate(cl_inegi = as.character(cl_inegi))

# colores ----
guinda <-"#832347"
verde <-"#097275"
dorado <-"#CCB656"
gris <-"#A8A8A7"
naranja <-"#C96C1C"
café <-"#7C5C15"

# capas ----

mun <- st_read("cartografia/MUNSON.shp")

son <- st_read("cartografia/ENTSON.shp")

capa_munison_df <- fortify(mun, region="concat")

capa_mapa <- left_join(mun, pob_son_2022, by = c("concat" = "cl_inegi"))


mapa <- ggplot() +
  geom_sf(aes(geometry=geometry, fill = grupo),color = "white",size=0.5, data = capa_mapa) + 
  geom_sf(fill = "transparent",color = "black",size=0.5, data = son) +
  scale_fill_manual(values = c("urbano" = verde, "semi-urbanos" = dorado, "rural" = café)) +
  theme_void() +
  theme(legend.position = "none") 
  # geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
  #              fill="transparent", color="black", size=0.2)
mapa

ggsave(paste0("Gráficos/mapa_son_pob_2022.png"),mapa,  width = 8, height = 8, dpi = 400, bg="transparent")


