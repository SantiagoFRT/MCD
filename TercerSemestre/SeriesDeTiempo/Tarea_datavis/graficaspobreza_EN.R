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
library(RColorBrewer)
library(wesanderson)
library(GGally)
library(janitor)
library(fmsb)


# Data ----
poorpop2016_2022 <- read_csv("poorpop2016_2022.csv", 
                             locale = locale(encoding = "WINDOWS-1252"))
# Grafico de líneas ----
### Poblacion en situacion de pobreza ----
#### Sonora ----

pobpobre <- poorpop2016_2022 %>% 
  filter(entidad == "Sonora", indicador== "Población en situación de pobreza")




pobpobre_graph <- ggplot(pobpobre, aes(x= Año, y= porcentaje)) +
  geom_line() + 
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") +
  labs(y= "Percentage", x = "Year") +
  ylim(10, 40) +
  ggtitle("Percentage of people living in poverty in Sonora: 2016-2017")


  
       
pobpobre_graph                                   
                         
#### Mexico ----

# graph_Abies <- ggplot(data8, aes(x=Profundidad1, y=Abies)) +
#   geom_line(color= "cornflowerblue", size=1) +
#   ylab("Abies (variación del % de polen encontrado)") +
#   xlab ("Profundidad (cm)") +
#   scale_x_continuous(name= "Profundidad (cm)", breaks= c(0, 50, 100, 150, 200, 250, 300)) +
#   scale_y_continuous(name= "Abies (variación del % de polen encontrado)", breaks= waiver()) +
#   theme(panel.background = element_rect(fill = 'white', color = 'grey'),
#         panel.grid.minor = element_line(color = 'black', linetype = 'dotted'),
#         panel.grid.major = element_line(color = 'black', size = 0.001)) +
#   ggtitle("Abies: variación del % de polen encontrado") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(plot.title = element_text(face = "bold", size = 15)) +
#   theme(axis.text.y = element_text(size = 9),
#         axis.title.y = element_text(size = 9))
# 
# graph_Abies

poorpop_Mex <- poorpop2016_2022 %>% 
  filter(entidad == "Nacional", indicador== "Población en situación de pobreza")


poorpop_graph_Mex <- ggplot(poorpop_Mex, aes(x= Año, y= porcentaje)) +
  geom_line(size=1.2, color= "blue") + 
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal()+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        plot.title = element_text(size = 30),
        panel.grid.minor = element_line(color = 'black', linetype = 'dotted'),
        panel.grid.major = element_line(color = 'black', size = 0.001)) +
  labs(y= "Percentage", x = "Year") +
  ylim(10, 50) +
  ggtitle("Percentage of people living in poverty in Mexico: 2016-2022")


poorpop_graph_Mex

### Poblacion en situacion de pobreza extrema ----
#### Sonora ----
pobpobreExt <- poorpop2016_2022 %>% 
  filter(entidad == "Sonora", indicador== "Población en situación de pobreza extrema")


pobpobreExt_graph <- ggplot(pobpobreExt, aes(x= Año, y= porcentaje)) +
  geom_line() + 
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") +
  labs(y= "Percentage", x = "Year") +
  ylim(0, 5) +
  ggtitle("Percentage of people living in extreme poverty in Sonora: 2016-2017")


pobpobreExt_graph  


#### Mexico ----

pobpobreExt_Mex <- poorpop2016_2022 %>% 
  filter(entidad == "Nacional", indicador== "Población en situación de pobreza extrema")


pobpobreExt_graph_Mex <- ggplot(pobpobreExt_Mex, aes(x= anio, y= porcentaje)) +
  geom_line() + 
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") +
  labs(y= "Porcentaje", x = "Año") +
  ylim(0, 10) +
  ggtitle("Porcentaje de la población en situación de pobreza extrema en México: 2016-2022")


pobpobreExt_graph_Mex



# Treemap ----
### Poblacion en situacion de pobreza ----

treemap_pobreza_data <- poorpop2016_2022 %>% 
  filter(indicador== "Población en situación de pobreza") %>% 
  mutate(ent = NULL, tipo= NULL, porcentaje= NULL)

treemap_pobreza_data <-  rows_delete(treemap_pobreza_data, tibble(entidad = "Nacional")) 
  
treemap_pobreza_data <- treemap_pobreza_data %>% mutate(Nacional = "Nacional")

treemap_pobreza_data <- treemap_pobreza_data %>%  relocate(Nacional, .before = "entidad") %>% 
  relocate(indicador, .before= "Año")


#### Creamos una paleta de colores ---

guinda <-"#832347"
verde <-"#097275"
dorado <-"#CCB656"
gris <-"#A8A8A7"
naranja <-"#C96C1C"
café <-"#7C5C15"

colors_isaf <- c(guinda, verde, dorado, gris, naranja, café)

n_colors <- 32
palette1 <- rainbow(n_colors) 

palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
palette3_all <- unlist(mapply(brewer.pal,                     # Create vector with all colors
                              palette3_info$maxcolors,
                              rownames(palette3_info)))


set.seed(2643598)                                             # Set random seed
palette3 <- sample(palette3_all, n_colors)                    # Sample colors
palette3                                            

 colors <- brewer.pal(n = 12, name = "Set3")

#### Codigo de treemap ---
 
  treemap_pobreza <- treemap_pobreza_data %>% 
    data_to_hierarchical(c(Año, entidad),miles_de_personas) %>%
    hchart(type = "treemap",
           colorByPoint = TRUE,
           # hcaes(color = colors),
           allowTraversingTree = T,
           levelIsConstant = F,
           drillUpButton = list(
             text = "← Volver"
           ),
           levels = list(
             list(level = 1, dataLabels = list(enabled = TRUE,
                                               format = "{point.name}<br>
                                                      {point.value} people"), borderColor = "black", borderWidth = 2),
             list(level = 2, dataLabels = list(enabled = FALSE,
                                               format = "{point.name}<br>
                                                      {point.value} people"))
             # list(level = 3, dataLabels = list(enabled = FALSE))
             # list(level = 4, dataLabels = list(enabled = FALSE))
           )
           
    ) %>%
    hc_colors(colors = palette3) %>% 
    hc_title(text = "People living in poverty by State in Mexico: 2016-2022")
  
# }
  treemap_pobreza
  
hcl_palettes("qualitative", plot = TRUE)


### Población en situación de pobreza extrema ----

treemap_pobrezaExt_data <- poorpop2016_2022 %>% 
  filter(indicador== "Población en situación de pobreza extrema") %>% 
  mutate(ent = NULL, tipo= NULL, porcentaje= NULL)

treemap_pobrezaExt_data <-  rows_delete(treemap_pobrezaExt_data, tibble(entidad = "Nacional")) 

treemap_pobrezaExt_data <- treemap_pobrezaExt_data %>% mutate(Nacional = "Nacional")

treemap_pobrezaExt_data <- treemap_pobrezaExt_data %>%  relocate(Nacional, .before = "entidad") %>% 
  relocate(indicador, .before= "anio")


#### Codigo de treemap ---

treemap_pobrezaExt <- treemap_pobrezaExt_data %>% 
  data_to_hierarchical(c(anio, entidad),miles_de_personas) %>%
  hchart(type = "treemap",
         colorByPoint = TRUE,
         # hcaes(color = colors),
         allowTraversingTree = T,
         levelIsConstant = F,
         drillUpButton = list(
           text = "← Volver"
         ),
         levels = list(
           list(level = 1, dataLabels = list(enabled = TRUE,
                                             format = "{point.name}<br>
                                                      {point.value} personas"), borderColor = "black", borderWidth = 2),
           list(level = 2, dataLabels = list(enabled = FALSE,
                                             format = "{point.name}<br>
                                                      {point.value} personas"))
           # list(level = 3, dataLabels = list(enabled = FALSE))
           # list(level = 4, dataLabels = list(enabled = FALSE))
         )
         
  ) %>%
  hc_colors(colors = palette3) %>% 
  hc_title(text = "Población en situación de pobreza extrema por entidad federativa: 2016-2022")


treemap_pobrezaExt

hcl_palettes("qualitative", plot = TRUE)



# Barplot ----
### Poblacion en situación de pobreza en la frontera norte ----

poorpop_north <- poorpop2016_2022 %>% 
  # group_by(anio, entidad, indicador, miles_de_personas) %>% 
  filter(entidad == "Baja California" |
                entidad == "Chihuahua" |
                entidad == "Coahuila" |
              entidad == "Nuevo León" |
                  entidad == "Sonora" |
              entidad == "Tamaulipas" ,
           
         indicador== "Población en situación de pobreza")

paleta <- RColorBrewer::brewer.pal(4, "BrBG")

poorpop_north_graph <- ggplot(data = poorpop_north, 
                               aes(x= entidad, y= porcentaje, fill=Año, group= Año)) +
  geom_col(position = "dodge") +
  scale_fill_continuous(breaks= c(2016, 2018, 2020, 2022)) +
  theme_minimal() + # add theme 
  theme(axis.text = element_text(size = 18), # axis text size
        axis.title = element_text(size = 23), # axis tittle size
        plot.title = element_text(size = 27), # plot tittle size
        legend.key.height= unit(1, 'cm'), # legend height
        legend.key.width= unit(1, 'cm'), # legend width
        legend.text = element_text(size=18), # legend text size
        legend.title = element_text(size=18)) + # legend title size
  ylim(0, 40) + # y axis limits
  labs(y= "Percentage", x = "State", fill= "Year") + # labs names and legend fill
  labs(title="People living in poverty in the Northern Border States of Mexico: 2016-2022") # plot tittle

# show plot
poorpop_north_graph


### Población en situación de pobreza extrema ----

pobpobreExt_norte <- poorpop2016_2022 %>% 
  # group_by(anio, entidad, indicador, miles_de_personas) %>% 
  filter(entidad == "Baja California" |
           entidad == "Chihuahua" |
           entidad == "Coahuila" |
           entidad == "Nuevo León" |
           entidad == "Sonora" |
           entidad == "Tamaulipas" ,
         
         indicador== "Población en situación de pobreza extrema")



pobpobreExt_norte_graph <- ggplot(pobpobreExt_norte, 
                               aes(entidad, porcentaje, fill=anio, group= anio)) +
  geom_col(position = "dodge") +
  ylim(0, 5) +
  labs(title="Porcentaje de la población en situación de pobreza extrema en la frontera norte: 2016-2022")


pobpobreExt_norte_graph


# Correlacion de Pearson ----
##### Sonora-----
 
pobpobre_Son_16_22 <- poorpop2016_2022 %>% 
  filter(entidad == "Sonora") %>% 
  mutate(ent = NULL, entidad = NULL, tipo = NULL, porcentaje= NULL) %>% 
  pivot_wider(values_from= 'miles_de_personas', names_from='indicador') %>% 
  mutate(anio= NULL) %>% 
  clean_names()

  

corr_pobpobre_Son_16_22_ggpairs <- ggpairs(pobpobre_Son_16_22, title="Correlacion entre variables de pobreza y carencia en Sonora: 2016-2022") 

corr_pobpobre_Son_16_22_ggcorr <- ggcorr(pobpobre_Son_16_22, method = c("everything", "pearson")) 



corr_pobpobre_Son_16_22_ggpairs

corr_pobpobre_Son_16_22_ggcorr




#### México ----

Mex_pobreza_16_22 <- poorpop2016_2022 %>% 
  filter(entidad == "Nacional") %>% 
  mutate(ent = NULL, entidad = NULL, tipo = NULL, porcentaje= NULL) %>% 
  pivot_wider(values_from= 'miles_de_personas', names_from='indicador') %>% 
  mutate(anio = NULL)
  clean_names()


corr_Mex_pobreza_16_22 <- ggcorr(Mex_pobreza_16_22, method = c("everything", "pearson")) 


corr_Mex_pobreza_16_22


# Spider graph ----
#### Sonora ----

carencias_Son_1622_wider <-  poorpop2016_2022 %>% 
  filter(entidad == "Sonora", indicador== "Carencia por acceso a los servicios de salud" |
                              indicador== "Carencia por acceso a la seguridad social" |
                             indicador== "Carencia por calidad y espacios de la vivienda" |
                             indicador== "Carencia por acceso a los servicios básicos en la vivienda" |
                             indicador== "Carencia por acceso a la alimentación nutritiva y de calidad" 
           ) %>% 
  mutate(ent = NULL, entidad = NULL, tipo = NULL, miles_de_personas= NULL) %>% 
  pivot_wider(values_from= 'porcentaje', names_from='indicador')%>% 
  mutate(Año = NULL)

# cambiamos nombre de columnas para evitar reduncancia
carencias_Son_1622_wider <- rename(carencias_Son_1622_wider,`servicios de salud` = `Carencia por acceso a los servicios de salud`)
carencias_Son_1622_wider <- rename(carencias_Son_1622_wider,`seguridad social` = `Carencia por acceso a la seguridad social`)
carencias_Son_1622_wider <- rename(carencias_Son_1622_wider,`calidad y espacios de la vivienda` = `Carencia por calidad y espacios de la vivienda`)
carencias_Son_1622_wider <- rename(carencias_Son_1622_wider,`acceso a los servicios básicos en la vivienda` = `Carencia por acceso a los servicios básicos en la vivienda`)
carencias_Son_1622_wider <- rename(carencias_Son_1622_wider,`acceso a la alimentación nutritiva` = `Carencia por acceso a la alimentación nutritiva y de calidad`)



# renombramos los renglones
rownames(carencias_Son_1622_wider) <- c("2016", "2018", "2020", "2022")

# agregamos dos renglones que correspondan al máximo y mínimo del gráfico
carencias_Son_1622_wider <- rbind(rep(50,5) , rep(0,5) , carencias_Son_1622_wider)


# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )


radarchart(carencias_Son_1622_wider,
           axistype=1 , 
           #custom polygon
           pcol=colors_isaf, plwd=4 , plty=2,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c("10%","20%","30%","40%","50%"), cglwd=1,
           #custom labels
           vlcex= 0.9 ,
           title= "Porcentaje de la población en Sonora por carencias sociales: 2016-2022")

legend("topright",
       legend = paste(c(2016, 2018, 2020, 2022)),
       bty = "n", pch = 20, col = colors_isaf,
       text.col = "grey25", pt.cex = 3)


#### Mexico ----

# df for the radar chart filtering the needed data
socialdep_Mex_1622_wider <-  poorpop2016_2022 %>% 
  filter(entidad == "Nacional", indicador== "Carencia por acceso a los servicios de salud" |
           indicador== "Carencia por acceso a la seguridad social" |
           indicador== "Carencia por calidad y espacios de la vivienda" |
           indicador== "Carencia por acceso a los servicios básicos en la vivienda" |
           indicador== "Carencia por acceso a la alimentación nutritiva y de calidad" 
  ) %>% 
  mutate(ent = NULL, entidad = NULL, tipo = NULL, miles_de_personas= NULL) %>% 
  pivot_wider(values_from= 'porcentaje', names_from='indicador')%>% 
  mutate(Año = NULL)

# change names of the variables from Spanish to English
socialdep_Mex_1622_wider <- rename(socialdep_Mex_1622_wider,`Health care` = `Carencia por acceso a los servicios de salud`)
socialdep_Mex_1622_wider <- rename(socialdep_Mex_1622_wider,`Social secutiry` = `Carencia por acceso a la seguridad social`)
socialdep_Mex_1622_wider <- rename(socialdep_Mex_1622_wider,`Quality of living standards` = `Carencia por calidad y espacios de la vivienda`)
socialdep_Mex_1622_wider <- rename(socialdep_Mex_1622_wider,`Access to basic services in the home` = `Carencia por acceso a los servicios básicos en la vivienda`)
socialdep_Mex_1622_wider <- rename(socialdep_Mex_1622_wider,`Access to nutritious food` = `Carencia por acceso a la alimentación nutritiva y de calidad`)



# rename the rows according to the years of study
rownames(socialdep_Mex_1622_wider) <- c("2016", "2018", "2020", "2022")

# add two rows corresponding to the max and min values of the chart; this arrangement is necesary for
# the radar chart to be work
socialdep_Mex_1622_wider <- rbind(rep(60,5) , rep(0,5) , socialdep_Mex_1622_wider)

# Set a color palette
wine <-"#832347"
green <-"#097275"
golden <-"#CCB656"
grey <-"#A8A8A7"
orange <-"#C96C1C"
brown <-"#7C5C15"

colors <- c(wine, green, golden, grey, orange, brown)

# radar chart
radarchart(socialdep_Mex_1622_wider, # data
           axistype=1 , # axis type can be 1, 2 or 3
           #custom polygon
           pcol=colors, plwd=4 , plty=2,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c("12%","24%","36%","48%","60%"), cglwd=1,
           #custom labels
           vlcex= 0.9 ,
           title= "Percentage of people living in Mexico by social deprivations: 2016-2022") # add title


# add legend
legend("topright",
       legend = paste(c(2016, 2018, 2020, 2022)),
       bty = "n", pch = 20, col = colors,
       text.col = "grey25", pt.cex = 3)








set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)
