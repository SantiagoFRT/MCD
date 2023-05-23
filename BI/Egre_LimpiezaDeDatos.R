library(shiny)
library(flexdashboard)
library(readxl)
library(tidyverse)
library(readr)
library(plotly)
library(ggmap)
library(janitor) #limpiar variables
library(stringi)

################################ EGRESOS 2021 ####################################
#### Llamamos a la base de datos ####
Egre21 <- read.csv("data/Egre/EgreSon21.csv", encoding = 'UTF-8')

#### Agregamos una columna que correponda a la clave de los **EGRESOS** ####
Egre21 <- Egre21 %>%
  add_column(Número = "constant_value") 

#### Relocalizamos la columna "Número" ####
Egre21 = Egre21 %>% relocate(Número, .before = VALOR)

#### Cambiamos los valores de por "11000" porque, por algún motivo, el valor ####
# de "Remuneraciones al personal" no se cambia
Egre21$Número[Egre21$Número == "constant_value"] <- "11000"

#### Cambiamos algunos conceptos para que embone el diccionario de ISAF con el de INEGI
Egre21$DESCRIPCION_CATEGORIA[Egre21$DESCRIPCION_CATEGORIA == "Bienes muebles, inmuebles e intangibles diversos"] <- "Bienes inmuebles"
Egre21$DESCRIPCION_CATEGORIA[Egre21$DESCRIPCION_CATEGORIA == "Materiales y suministros diversos"] <- "Materiales y suministros para seguridad"


#### Cambiamos los valores de la columna "Número" de acuerdo a los valores de la columna ####
# "DESCRIPCION_CATEGORÍA"

Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Remuneraciones al personal"] = "11000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Remuneraciones adicionales y especiales"] = "13000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Seguridad social"] = "14000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Otras prestaciones sociales y económicas"] = "15000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Pago de estímulos a servidores públicos"] = "17000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Materiales de administración, emisión de documentos y artículos oficiales"] = "21000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Alimentos y utensilios"] = "22000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Materiales y artículos de construcción y de reparación"] = "24000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Productos químicos, farmacéuticos, laboratorio y materias primas de producción y comercialización"] = "25000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Combustibles, lubricantes y aditivos"] = "26000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Vestuario, blancos, prendas de protección y artículos deportivos"] = "27000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Materiales y suministros para seguridad"] = "28000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Herramientas, refacciones y accesorios menores"] = "29000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Servicios básicos"] = "31000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Servicios de arrendamiento"] = "32000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Servicios profesionales, científicos, técnicos y otros servicios"] = "33000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Servicios financieros, bancarios y comerciales"] = "34000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Servicios de instalación, reparación, mantenimiento y conservación"] = "35000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Servicios de comunicación social y publicidad"] = "36000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Servicios de traslado y viáticos"] = "37000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Servicios oficiales"] = "38000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Otros servicios generales"] = "39000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Transferencias internas y asignaciones al sector público"] = "41000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Subsidios y subvenciones"] = "43000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Ayudas sociales"] = "44000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Pensiones y jubilaciones"] = "45000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Mobiliario y equipo de administración"] = "51000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Mobiliario y equipo educacional y recreativo"] = "52000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Vehículos y equipo de transporte"] = "54000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Equipo de defensa y seguridad y equipo e instrumental médico y laboratorio"] = "55000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Maquinaria, otros equipos y herramientas"] = "56000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Bienes inmuebles"] = "58000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Activos intangibles"] = "59000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público"] = "61000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Obra pública en bienes propios"] = "62000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Amortización de la deuda pública"] = "91000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Intereses de la deuda pública"] = "92000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Comisiones de la deuda pública"] = "93000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Gastos de la deuda pública"] = "94000"
Egre21$Número[Egre21$DESCRIPCION_CATEGORIA == "Adeudos de ejercicios fiscales anteriores (ADEFAS)"] = "99000"

#### Convertimos la columna VALOR a tipo numérico ####
#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1))

Egre21$VALOR = as.numeric(as.character(Egre21$VALOR))

#### Eliminamos columnas inecesarias por posición (index).
# df2 <- df[,-2]
Egre21 = Egre21[,c(3, 5, 8, 9, 10)]

##### Cambiamos el nombre de la columna anio por AÑo y VALOR a Valor
# colnames(df)[2] <- "new_col2"
colnames(Egre21)[1] = "Año"
colnames(Egre21)[5] = "Valor"

#### Combinamos las columnas "Número" y "DESCRIPCION_CATEGORIA" en una sola ####

Egre21$Concepto = paste(Egre21$Número, Egre21$DESCRIPCION_CATEGORIA, sep=" ")

#### Eliminamos las columnas "Número" y "DESCRIPCION_CATEGORIA"

Egre21 = Egre21[,c(1, 2, 5, 6)]

#### Aplicamos pivotwider sobre la columna "Concepto" ####

Egre21 <- Egre21 %>%
  pivot_wider(names_from = Concepto, values_from = Valor)


#### Cargamos la base de datos con los nombres de los municipios ####
mun <- read.csv("data/REGIONES_CPCEM.csv", encoding = 'UTF-8')


#### Agregamos la columna "Nom_mun" al df Egre21 ####
Egre21$Municipio <- mun$NOM_MUN


#### Relocalizamos la columna "Municipio" del df Egreso21 ####
# df  = df %>% relocate(x, .after=y)

Egre21 = Egre21 %>% relocate(Municipio, .after=ID_MUNICIPIO)


######################################## EGRESOS 20 #########################################

Egre20 <- read.csv("data/Egre/EgreSon20.csv", encoding = 'UTF-8')

#### Agregamos una columna que correponda a la clave de los **EGRESOS** ####
Egre20 <- Egre20 %>%
  add_column(Número = "constant_value") 

#### Relocalizamos la columna "Número" ####
Egre20 = Egre20 %>% relocate(Número, .before = VALOR)

#### Cambiamos los valores de por "11000" porque, por algún motivo, el valor ####
# de "Remuneraciones al personal" no se cambia
Egre20$Número[Egre20$Número == "constant_value"] <- "11000"

#### Cambiamos algunos conceptos para que embone el diccionario de ISAF con el de INEGI
Egre20$DESCRIPCION_CATEGORIA[Egre20$DESCRIPCION_CATEGORIA == "Bienes muebles, inmuebles e intangibles diversos"] <- "Bienes inmuebles"
Egre20$DESCRIPCION_CATEGORIA[Egre20$DESCRIPCION_CATEGORIA == "Materiales y suministros diversos"] <- "Materiales y suministros para seguridad"


#### Cambiamos los valores de la columna "Número" de acuerdo a los valores de la columna ####
# "DESCRIPCION_CATEGORÍA"

Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Remuneraciones al personal"] = "11000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Remuneraciones adicionales y especiales"] = "13000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Seguridad social"] = "14000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Otras prestaciones sociales y económicas"] = "15000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Pago de estímulos a servidores públicos"] = "17000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Materiales de administración, emisión de documentos y artículos oficiales"] = "21000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Alimentos y utensilios"] = "22000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Materiales y artículos de construcción y de reparación"] = "24000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Productos químicos, farmacéuticos, laboratorio y materias primas de producción y comercialización"] = "25000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Combustibles, lubricantes y aditivos"] = "26000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Vestuario, blancos, prendas de protección y artículos deportivos"] = "27000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Materiales y suministros para seguridad"] = "28000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Herramientas, refacciones y accesorios menores"] = "29000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Servicios básicos"] = "31000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Servicios de arrendamiento"] = "32000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Servicios profesionales, científicos, técnicos y otros servicios"] = "33000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Servicios financieros, bancarios y comerciales"] = "34000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Servicios de instalación, reparación, mantenimiento y conservación"] = "35000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Servicios de comunicación social y publicidad"] = "36000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Servicios de traslado y viáticos"] = "37000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Servicios oficiales"] = "38000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Otros servicios generales"] = "39000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Transferencias internas y asignaciones al sector público"] = "41000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Subsidios y subvenciones"] = "43000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Ayudas sociales"] = "44000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Pensiones y jubilaciones"] = "45000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Mobiliario y equipo de administración"] = "51000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Mobiliario y equipo educacional y recreativo"] = "52000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Vehículos y equipo de transporte"] = "54000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Equipo de defensa y seguridad y equipo e instrumental médico y laboratorio"] = "55000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Maquinaria, otros equipos y herramientas"] = "56000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Activos biológicos"] = "57000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Bienes inmuebles"] = "58000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Activos intangibles"] = "59000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público"] = "61000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Obra pública en bienes propios"] = "62000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Proyectos productivos y acciones de fomento"] = "63000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Amortización de la deuda pública"] = "91000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Intereses de la deuda pública"] = "92000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Comisiones de la deuda pública"] = "93000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Gastos de la deuda pública"] = "94000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Adeudos de ejercicios fiscales anteriores (ADEFAS)"] = "99000"
Egre20$Número[Egre20$DESCRIPCION_CATEGORIA == "Egresos diversos"] = " "

#### Convertimos la columna VALOR a tipo numérico ####
#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1))

Egre20$VALOR = as.numeric(as.character(Egre20$VALOR))

#### Cambiamos espacios en blanco por "NA" ####
#data_new1[data_new1 == ""] <- NA
Egre20[Egre20 == ""] <- NA

#### Eliminamos columnas inecesarias por posición (index).
# df2 <- df[,-2]
Egre20 = Egre20[,c(3, 5, 8, 9, 10)]

##### Cambiamos el nombre de la columna anio por AÑo y VALOR a Valor
# colnames(df)[2] <- "new_col2"
colnames(Egre20)[1] = "Año"
colnames(Egre20)[5] = "Valor"

#### Combinamos las columnas "Número" y "DESCRIPCION_CATEGORIA" en una sola ####

Egre20$Concepto = paste(Egre20$Número, Egre20$DESCRIPCION_CATEGORIA, sep=" ")

#### Eliminamos las columnas "Número" y "DESCRIPCION_CATEGORIA"

Egre20 = Egre20[,c(1, 2, 5, 6)]

#### Aplicamos pivotwider sobre la columna "Concepto" ####

Egre20 <- Egre20 %>%
  pivot_wider(names_from = Concepto, values_from = Valor)


#### Cargamos la base de datos con los nombres de los municipios ####
mun <- read.csv("data/REGIONES_CPCEM.csv", encoding = 'UTF-8')


#### Agregamos la columna "Nom_mun" al df Egre20 ####
Egre20$Municipio <- mun$NOM_MUN


#### Relocalizamos la columna "Municipio" del df Egreso21 ####
# df  = df %>% relocate(x, .after=y)

Egre20 = Egre20 %>% relocate(Municipio, .after=ID_MUNICIPIO)

#### Eliminar la última columna dado que corresponde a una categoria llamada NA, originada ####
# por la falta de información en Villa Pesqueria durante 2020
Egre20 = Egre20[,c(1:46)]

colnames(Egre20)

###################################### EGRE 2019 #########################################

Egre19 <- read.csv("data/Egre/EgreSon19.csv", encoding = 'UTF-8')

#### Agregamos una columna que correponda a la clave de los **EGRESOS** ####
Egre19 <- Egre19 %>%
  add_column(Número = "constant_value") 

#### Relocalizamos la columna "Número" ####
Egre19 = Egre19 %>% relocate(Número, .before = VALOR)

#### Cambiamos los valores de por "11000" porque, por algún motivo, el valor ####
# de "Remuneraciones al personal" no se cambia
Egre19$Número[Egre19$Número == "constant_value"] <- "11000"

#### Cambiamos algunos conceptos para que embone el diccionario de ISAF con el de INEGI
Egre19$DESCRIPCION_CATEGORIA[Egre19$DESCRIPCION_CATEGORIA == "Bienes muebles, inmuebles e intangibles diversos"] <- "Bienes inmuebles"
Egre19$DESCRIPCION_CATEGORIA[Egre19$DESCRIPCION_CATEGORIA == "Materiales y suministros diversos"] <- "Materiales y suministros para seguridad"


#### Cambiamos los valores de la columna "Número" de acuerdo a los valores de la columna ####
# "DESCRIPCION_CATEGORÍA"

Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Remuneraciones al personal"] = "11000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Remuneraciones adicionales y especiales"] = "13000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Seguridad social"] = "14000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Otras prestaciones sociales y económicas"] = "15000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Previsiones"] = "16000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Pago de estímulos a servidores públicos"] = "17000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Materiales de administración, emisión de documentos y artículos oficiales"] = "21000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Alimentos y utensilios"] = "22000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Materiales y artículos de construcción y de reparación"] = "24000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Productos químicos, farmacéuticos, laboratorio y materias primas de producción y comercialización"] = "25000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Combustibles, lubricantes y aditivos"] = "26000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Vestuario, blancos, prendas de protección y artículos deportivos"] = "27000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Materiales y suministros para seguridad"] = "28000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Herramientas, refacciones y accesorios menores"] = "29000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Servicios básicos"] = "31000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Servicios de arrendamiento"] = "32000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Servicios profesionales, científicos, técnicos y otros servicios"] = "33000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Servicios financieros, bancarios y comerciales"] = "34000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Servicios de instalación, reparación, mantenimiento y conservación"] = "35000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Servicios de comunicación social y publicidad"] = "36000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Servicios de traslado y viáticos"] = "37000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Servicios oficiales"] = "38000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Otros servicios generales"] = "39000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Transferencias internas y asignaciones al sector público"] = "41000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Subsidios y subvenciones"] = "43000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Ayudas sociales"] = "44000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Pensiones y jubilaciones"] = "45000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Mobiliario y equipo de administración"] = "51000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Mobiliario y equipo educacional y recreativo"] = "52000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Vehículos y equipo de transporte"] = "54000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Equipo de defensa y seguridad y equipo e instrumental médico y laboratorio"] = "55000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Maquinaria, otros equipos y herramientas"] = "56000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Activos biológicos"] = "57000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Bienes inmuebles"] = "58000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Activos intangibles"] = "59000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público"] = "61000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Obra pública en bienes propios"] = "62000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Proyectos productivos y acciones de fomento"] = "63000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Amortización de la deuda pública"] = "91000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Intereses de la deuda pública"] = "92000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Comisiones de la deuda pública"] = "93000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Gastos de la deuda pública"] = "94000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Adeudos de ejercicios fiscales anteriores (ADEFAS)"] = "99000"
Egre19$Número[Egre19$DESCRIPCION_CATEGORIA == "Egresos diversos"] = " "

#### Convertimos la columna VALOR a tipo numérico ####
#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1))

Egre19$VALOR = as.numeric(as.character(Egre19$VALOR))

#### Eliminamos columnas inecesarias por posición (index).
# df2 <- df[,-2]
Egre19 = Egre19[,c(3, 5, 8, 9, 10)]

##### Cambiamos el nombre de la columna anio por AÑo
# colnames(df)[2] <- "new_col2"
colnames(Egre19)[1] = "Año"

#### Combinamos las columnas "Número" y "DESCRIPCION_CATEGORIA" en una sola ####

Egre19$Concepto = paste(Egre19$Número, Egre19$DESCRIPCION_CATEGORIA, sep=" ")

#### Agrupamos para sumar los valores de las categorías repetidas, de tal forma####
# que sea coherente la homogenización de conceptos entre el catálogo isaf e inegi.
# Además, aprovechamos este paso para eliminar las columnas "Número" y "DESCRIPCION_CATEGORIA"
Egre19 <- Egre19 %>% group_by(Año, ID_MUNICIPIO, Concepto) %>%
  summarise(sum = sum(VALOR), n = n())

#### Eliminamos la columna de frecuencia "n"
# Egre18 = Egre18[,c(3, 5, 8, 9, 10)]

Egre19 = Egre19[,c(1:4)]

# Cambiamos el nombre de la columna "sum" por "Valor"

colnames(Egre19)[4] = "Valor"

#### Aplicamos pivotwider sobre la columna "Concepto" ####

Egre19 <- Egre19 %>%
  pivot_wider(names_from = Concepto, values_from = Valor)


#### Cargamos la base de datos con los nombres de los municipios ####
mun <- read.csv("data/REGIONES_CPCEM.csv", encoding = 'UTF-8')

#### Agregamos la columna "Nom_mun" al df Egre19 ####
Egre19$Municipio <- mun$NOM_MUN


#### Relocalizamos la columna "Municipio" del df Egreso21 ####
# df  = df %>% relocate(x, .after=y)

Egre19 = Egre19 %>% relocate(Municipio, .after=ID_MUNICIPIO)


#### Cambiamos el tipo de dato de "Int" a "Num" en Valor
# df[["Height_Measurement"]] <- as.numeric(df[["Height_Measurement"]])
# colnames(Egre19)
# Egre19[,4] <- as.numeric(Egre19[,4])
# #class(Egre19$Valor) = "Numeric"


################################## EGRESOS 2018 ###################################

Egre18 <- read.csv("data/Egre/EgreSon18.csv", encoding = 'UTF-8')

#### Agregamos una columna que correponda a la clave de los **EGRESOS** ####
Egre18 <- Egre18 %>%
  add_column(Número = "constant_value") 

#### Relocalizamos la columna "Número" ####
Egre18 = Egre18 %>% relocate(Número, .before = VALOR)

#### Cambiamos los valores de por "11000" porque, por algún motivo, el valor ####
# de "Remuneraciones al personal" no se cambia
Egre18$Número[Egre18$Número == "constant_value"] <- "11000"

#### Cambiamos algunos conceptos para que embone el diccionario de ISAF con el de INEGI
Egre18$DESCRIPCION_CATEGORIA[Egre18$DESCRIPCION_CATEGORIA == "Bienes muebles, inmuebles e intangibles diversos"] <- "Bienes inmuebles"
Egre18$DESCRIPCION_CATEGORIA[Egre18$DESCRIPCION_CATEGORIA == "Materiales y suministros diversos"] <- "Materiales y suministros para seguridad"


#### Cambiamos los valores de la columna "Número" de acuerdo a los valores de la columna ####
# "DESCRIPCION_CATEGORÍA"

Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Remuneraciones al personal"] = "11000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Remuneraciones adicionales y especiales"] = "13000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Seguridad social"] = "14000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Otras prestaciones sociales y económicas"] = "15000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Previsiones"] = "16000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Pago de estímulos a servidores públicos"] = "17000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Materiales de administración, emisión de documentos y artículos oficiales"] = "21000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Alimentos y utensilios"] = "22000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Materiales y artículos de construcción y de reparación"] = "24000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Productos químicos, farmacéuticos, laboratorio y materias primas de producción y comercialización"] = "25000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Combustibles, lubricantes y aditivos"] = "26000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Vestuario, blancos, prendas de protección y artículos deportivos"] = "27000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Materiales y suministros para seguridad"] = "28000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Herramientas, refacciones y accesorios menores"] = "29000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Servicios básicos"] = "31000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Servicios de arrendamiento"] = "32000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Servicios profesionales, científicos, técnicos y otros servicios"] = "33000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Servicios financieros, bancarios y comerciales"] = "34000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Servicios de instalación, reparación, mantenimiento y conservación"] = "35000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Servicios de comunicación social y publicidad"] = "36000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Servicios de traslado y viáticos"] = "37000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Servicios oficiales"] = "38000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Otros servicios generales"] = "39000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Transferencias internas y asignaciones al sector público"] = "41000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Subsidios y subvenciones"] = "43000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Ayudas sociales"] = "44000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Pensiones y jubilaciones"] = "45000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Mobiliario y equipo de administración"] = "51000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Mobiliario y equipo educacional y recreativo"] = "52000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Vehículos y equipo de transporte"] = "54000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Equipo de defensa y seguridad y equipo e instrumental médico y laboratorio"] = "55000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Maquinaria, otros equipos y herramientas"] = "56000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Activos biológicos"] = "57000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Bienes inmuebles"] = "58000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Activos intangibles"] = "59000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público"] = "61000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Obra pública en bienes propios"] = "62000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Proyectos productivos y acciones de fomento"] = "63000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Amortización de la deuda pública"] = "91000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Intereses de la deuda pública"] = "92000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Comisiones de la deuda pública"] = "93000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Gastos de la deuda pública"] = "94000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Adeudos de ejercicios fiscales anteriores (ADEFAS)"] = "99000"
Egre18$Número[Egre18$DESCRIPCION_CATEGORIA == "Egresos diversos"] = " "

#### Convertimos la columna VALOR a tipo numérico ####
#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1))

Egre18$VALOR = as.numeric(as.character(Egre18$VALOR))

#### Eliminamos columnas inecesarias por posición (index).
# df2 <- df[,-2]
Egre18 = Egre18[,c(3, 5, 8, 9, 10)]

##### Cambiamos el nombre de la columna anio por AÑo
# colnames(df)[2] <- "new_col2"
colnames(Egre18)[1] = "Año"

#### Combinamos las columnas "Número" y "DESCRIPCION_CATEGORIA" en una sola ####

Egre18$Concepto = paste(Egre18$Número, Egre18$DESCRIPCION_CATEGORIA, sep=" ")

#### Agrupamos para sumar los valores de las categorías repetidas, de tal forma####
# que sea coherente la homogenización de conceptos entre el catálogo isaf e inegi.
# Además, aprovechamos este paso para eliminar las columnas "Número" y "DESCRIPCION_CATEGORIA"
Egre18 <- Egre18 %>% group_by(Año, ID_MUNICIPIO, Concepto) %>%
  summarise(sum = sum(VALOR), n = n())

#### Eliminamos la columna de frecuencia "n"
# Egre18 = Egre18[,c(3, 5, 8, 9, 10)]

Egre18 = Egre18[,c(1:4)]

# Cambiamos el nombre de la columna "sum" por "Valor"

colnames(Egre18)[4] = "Valor"

#### Aplicamos pivotwider sobre la columna "Concepto" ####

Egre18 <- Egre18 %>%
  pivot_wider(names_from = Concepto, values_from = Valor)


#### Cargamos la base de datos con los nombres de los municipios ####
mun <- read.csv("data/REGIONES_CPCEM.csv", encoding = 'UTF-8')


#### Agregamos la columna "Nom_mun" al df Egre18 ####
Egre18$Municipio <- mun$NOM_MUN


#### Relocalizamos la columna "Municipio" del df Egreso21 ####
# df  = df %>% relocate(x, .after=y)

Egre18 = Egre18 %>% relocate(Municipio, .after=ID_MUNICIPIO)


####################################### EGRESOS 2017 ####################################

Egre17 <- read.csv("data/Egre/EgreSon17.csv", encoding = 'UTF-8')

#### Agregamos una columna que correponda a la clave de los **EGRESOS** ####
Egre17 <- Egre17 %>%
  add_column(Número = "constant_value") 

#### Relocalizamos la columna "Número" ####
Egre17 = Egre17 %>% relocate(Número, .before = VALOR)

#### Cambiamos los valores de por "11000" porque, por algún motivo, el valor ####
# de "Remuneraciones al personal" no se cambia
Egre17$Número[Egre17$Número == "constant_value"] <- "11000"

#### Cambiamos algunos conceptos para que embone el diccionario de ISAF con el de INEGI
Egre17$DESCRIPCION_CATEGORIA[Egre17$DESCRIPCION_CATEGORIA == "Bienes muebles, inmuebles e intangibles diversos"] <- "Bienes inmuebles"
Egre17$DESCRIPCION_CATEGORIA[Egre17$DESCRIPCION_CATEGORIA == "Materiales y suministros diversos"] <- "Materiales y suministros para seguridad"


#### Cambiamos los valores de la columna "Número" de acuerdo a los valores de la columna ####
# "DESCRIPCION_CATEGORÍA"

Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Remuneraciones al personal"] = "11000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Remuneraciones adicionales y especiales"] = "13000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Seguridad social"] = "14000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Otras prestaciones sociales y económicas"] = "15000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Previsiones"] = "16000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Pago de estímulos a servidores públicos"] = "17000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Materiales de administración, emisión de documentos y artículos oficiales"] = "21000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Alimentos y utensilios"] = "22000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Materiales y artículos de construcción y de reparación"] = "24000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Productos químicos, farmacéuticos, laboratorio y materias primas de producción y comercialización"] = "25000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Combustibles, lubricantes y aditivos"] = "26000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Vestuario, blancos, prendas de protección y artículos deportivos"] = "27000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Materiales y suministros para seguridad"] = "28000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Herramientas, refacciones y accesorios menores"] = "29000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Servicios básicos"] = "31000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Servicios de arrendamiento"] = "32000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Servicios profesionales, científicos, técnicos y otros servicios"] = "33000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Servicios financieros, bancarios y comerciales"] = "34000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Servicios de instalación, reparación, mantenimiento y conservación"] = "35000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Servicios de comunicación social y publicidad"] = "36000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Servicios de traslado y viáticos"] = "37000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Servicios oficiales"] = "38000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Otros servicios generales"] = "39000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Transferencias internas y asignaciones al sector público"] = "41000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Subsidios y subvenciones"] = "43000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Ayudas sociales"] = "44000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Pensiones y jubilaciones"] = "45000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Mobiliario y equipo de administración"] = "51000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Mobiliario y equipo educacional y recreativo"] = "52000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Vehículos y equipo de transporte"] = "54000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Equipo de defensa y seguridad y equipo e instrumental médico y laboratorio"] = "55000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Maquinaria, otros equipos y herramientas"] = "56000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Activos biológicos"] = "57000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Bienes inmuebles"] = "58000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Activos intangibles"] = "59000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público"] = "61000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Obra pública en bienes propios"] = "62000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Proyectos productivos y acciones de fomento"] = "63000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Amortización de la deuda pública"] = "91000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Intereses de la deuda pública"] = "92000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Comisiones de la deuda pública"] = "93000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Gastos de la deuda pública"] = "94000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Adeudos de ejercicios fiscales anteriores (ADEFAS)"] = "99000"
Egre17$Número[Egre17$DESCRIPCION_CATEGORIA == "Egresos diversos"] = " "

#### Convertimos la columna VALOR a tipo numérico ####
#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1))

Egre17$VALOR = as.numeric(as.character(Egre17$VALOR))

#### Eliminamos columnas inecesarias por posición (index).
# df2 <- df[,-2]
Egre17 = Egre17[,c(3, 5, 8, 9, 10)]

##### Cambiamos el nombre de la columna anio por AÑo
# colnames(df)[2] <- "new_col2"
colnames(Egre17)[1] = "Año"

#### Combinamos las columnas "Número" y "DESCRIPCION_CATEGORIA" en una sola ####

Egre17$Concepto = paste(Egre17$Número, Egre17$DESCRIPCION_CATEGORIA, sep=" ")

#### Agrupamos para sumar los valores de las categorías repetidas, de tal forma####
# que sea coherente la homogenización de conceptos entre el catálogo isaf e inegi.
# Además, aprovechamos este paso para eliminar las columnas "Número" y "DESCRIPCION_CATEGORIA"
Egre17 <- Egre17 %>% group_by(Año, ID_MUNICIPIO, Concepto) %>%
  summarise(sum = sum(VALOR), n = n())

#### Eliminamos la columna de frecuencia "n"
# Egre17 = Egre17[,c(3, 5, 8, 9, 10)]

Egre17 = Egre17[,c(1:4)]

# Cambiamos el nombre de la columna "sum" por "Valor"

colnames(Egre17)[4] = "Valor"

#### Aplicamos pivotwider sobre la columna "Concepto" ####

Egre17 <- Egre17 %>%
  pivot_wider(names_from = Concepto, values_from = Valor)


#### Cargamos la base de datos con los nombres de los municipios ####
mun <- read.csv("data/REGIONES_CPCEM.csv", encoding = 'UTF-8')


#### Agregamos la columna "Nom_mun" al df Egre17 ####
Egre17$Municipio <- mun$NOM_MUN


#### Relocalizamos la columna "Municipio" del df Egreso21 ####
# df  = df %>% relocate(x, .after=y)

Egre17 = Egre17 %>% relocate(Municipio, .after=ID_MUNICIPIO)


############################## EGRESOS 2016 ###################################

Egre16 <- read.csv("data/Egre/EgreSon16.csv", encoding = 'UTF-8')

#### Agregamos una columna que correponda a la clave de los **EGRESOS** ####
Egre16 <- Egre16 %>%
  add_column(Número = "constant_value") 

#### Relocalizamos la columna "Número" ####
Egre16 = Egre16 %>% relocate(Número, .before = VALOR)

#### Cambiamos los valores de por "11000" porque, por algún motivo, el valor ####
# de "Remuneraciones al personal" no se cambia
Egre16$Número[Egre16$Número == "constant_value"] <- "11000"

#### Cambiamos algunos conceptos para que embone el diccionario de ISAF con el de INEGI
Egre16$DESCRIPCION_CATEGORIA[Egre16$DESCRIPCION_CATEGORIA == "Bienes muebles, inmuebles e intangibles diversos"] <- "Bienes inmuebles"
Egre16$DESCRIPCION_CATEGORIA[Egre16$DESCRIPCION_CATEGORIA == "Materiales y suministros diversos"] <- "Materiales y suministros para seguridad"


#### Cambiamos los valores de la columna "Número" de acuerdo a los valores de la columna ####
# "DESCRIPCION_CATEGORÍA"

Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Remuneraciones al personal"] = "11000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Remuneraciones adicionales y especiales"] = "13000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Seguridad social"] = "14000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Otras prestaciones sociales y económicas"] = "15000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Previsiones"] = "16000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Pago de estímulos a servidores públicos"] = "17000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Materiales de administración, emisión de documentos y artículos oficiales"] = "21000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Alimentos y utensilios"] = "22000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Materiales y artículos de construcción y de reparación"] = "24000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Productos químicos, farmacéuticos, laboratorio y materias primas de producción y comercialización"] = "25000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Combustibles, lubricantes y aditivos"] = "26000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Vestuario, blancos, prendas de protección y artículos deportivos"] = "27000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Materiales y suministros para seguridad"] = "28000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Herramientas, refacciones y accesorios menores"] = "29000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Servicios básicos"] = "31000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Servicios de arrendamiento"] = "32000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Servicios profesionales, científicos, técnicos y otros servicios"] = "33000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Servicios financieros, bancarios y comerciales"] = "34000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Servicios de instalación, reparación, mantenimiento y conservación"] = "35000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Servicios de comunicación social y publicidad"] = "36000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Servicios de traslado y viáticos"] = "37000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Servicios oficiales"] = "38000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Otros servicios generales"] = "39000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Transferencias internas y asignaciones al sector público"] = "41000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Subsidios y subvenciones"] = "43000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Ayudas sociales"] = "44000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Pensiones y jubilaciones"] = "45000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Mobiliario y equipo de administración"] = "51000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Mobiliario y equipo educacional y recreativo"] = "52000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Vehículos y equipo de transporte"] = "54000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Equipo de defensa y seguridad y equipo e instrumental médico y laboratorio"] = "55000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Maquinaria, otros equipos y herramientas"] = "56000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Activos biológicos"] = "57000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Bienes inmuebles"] = "58000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Activos intangibles"] = "59000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público"] = "61000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Obra pública en bienes propios"] = "62000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Proyectos productivos y acciones de fomento"] = "63000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Provisiones para contingencias y otras erogaciones especiales"] = "79000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Amortización de la deuda pública"] = "91000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Intereses de la deuda pública"] = "92000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Comisiones de la deuda pública"] = "93000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Gastos de la deuda pública"] = "94000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Adeudos de ejercicios fiscales anteriores (ADEFAS)"] = "99000"
Egre16$Número[Egre16$DESCRIPCION_CATEGORIA == "Egresos diversos"] = " "

#### Convertimos la columna VALOR a tipo numérico ####
#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1))

Egre16$VALOR = as.numeric(as.character(Egre16$VALOR))

#### Eliminamos columnas inecesarias por posición (index).
# df2 <- df[,-2]
Egre16 = Egre16[,c(3, 5, 8, 9, 10)]

##### Cambiamos el nombre de la columna anio por AÑo
# colnames(df)[2] <- "new_col2"
colnames(Egre16)[1] = "Año"

#### Combinamos las columnas "Número" y "DESCRIPCION_CATEGORIA" en una sola ####

Egre16$Concepto = paste(Egre16$Número, Egre16$DESCRIPCION_CATEGORIA, sep=" ")

#### Agrupamos para sumar los valores de las categorías repetidas, de tal forma####
# que sea coherente la homogenización de conceptos entre el catálogo isaf e inegi.
# Además, aprovechamos este paso para eliminar las columnas "Número" y "DESCRIPCION_CATEGORIA"
Egre16 <- Egre16 %>% group_by(Año, ID_MUNICIPIO, Concepto) %>%
  summarise(sum = sum(VALOR), n = n())

#### Eliminamos la columna de frecuencia "n"
# Egre16 = Egre16[,c(3, 5, 8, 9, 10)]

Egre16 = Egre16[,c(1:4)]

# Cambiamos el nombre de la columna "sum" por "Valor"

colnames(Egre16)[4] = "Valor"

#### Aplicamos pivotwider sobre la columna "Concepto" ####

Egre16 <- Egre16 %>%
  pivot_wider(names_from = Concepto, values_from = Valor)


#### Cargamos la base de datos con los nombres de los municipios ####
mun <- read.csv("data/REGIONES_CPCEM.csv", encoding = 'UTF-8')


#### Agregamos la columna "Nom_mun" al df Egre16 ####
Egre16$Municipio <- mun$NOM_MUN


#### Relocalizamos la columna "Municipio" del df Egreso21 ####
# df  = df %>% relocate(x, .after=y)

Egre16 = Egre16 %>% relocate(Municipio, .after=ID_MUNICIPIO)


###################################### EGRESSO 2015 ############################################


Egre15 <- read.csv("data/Egre/EgreSon15.csv", encoding = 'UTF-8')

#### Agregamos una columna que correponda a la clave de los **EGRESOS** ####
Egre15 <- Egre15 %>%
  add_column(Número = "constant_value") 

#### Relocalizamos la columna "Número" ####
Egre15 = Egre15 %>% relocate(Número, .before = VALOR)

#### Cambiamos los valores de por "11000" porque, por algún motivo, el valor ####
# de "Remuneraciones al personal" no se cambia
Egre15$Número[Egre15$Número == "constant_value"] <- "11000"

#### Cambiamos algunos conceptos para que embone el diccionario de ISAF con el de INEGI
Egre15$DESCRIPCION_CATEGORIA[Egre15$DESCRIPCION_CATEGORIA == "Bienes muebles, inmuebles e intangibles diversos"] <- "Bienes inmuebles"
Egre15$DESCRIPCION_CATEGORIA[Egre15$DESCRIPCION_CATEGORIA == "Materiales y suministros diversos"] <- "Materiales y suministros para seguridad"


#### Cambiamos los valores de la columna "Número" de acuerdo a los valores de la columna ####
# "DESCRIPCION_CATEGORÍA"

Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Remuneraciones al personal"] = "11000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Remuneraciones adicionales y especiales"] = "13000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Seguridad social"] = "14000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Otras prestaciones sociales y económicas"] = "15000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Previsiones"] = "16000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Pago de estímulos a servidores públicos"] = "17000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Materiales de administración, emisión de documentos y artículos oficiales"] = "21000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Alimentos y utensilios"] = "22000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Materiales y artículos de construcción y de reparación"] = "24000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Productos químicos, farmacéuticos, laboratorio y materias primas de producción y comercialización"] = "25000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Combustibles, lubricantes y aditivos"] = "26000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Vestuario, blancos, prendas de protección y artículos deportivos"] = "27000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Materiales y suministros para seguridad"] = "28000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Herramientas, refacciones y accesorios menores"] = "29000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Servicios básicos"] = "31000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Servicios de arrendamiento"] = "32000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Servicios profesionales, científicos, técnicos y otros servicios"] = "33000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Servicios financieros, bancarios y comerciales"] = "34000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Servicios de instalación, reparación, mantenimiento y conservación"] = "35000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Servicios de comunicación social y publicidad"] = "36000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Servicios de traslado y viáticos"] = "37000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Servicios oficiales"] = "38000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Otros servicios generales"] = "39000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Transferencias internas y asignaciones al sector público"] = "41000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Subsidios y subvenciones"] = "43000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Ayudas sociales"] = "44000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Pensiones y jubilaciones"] = "45000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Mobiliario y equipo de administración"] = "51000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Mobiliario y equipo educacional y recreativo"] = "52000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Vehículos y equipo de transporte"] = "54000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Equipo de defensa y seguridad y equipo e instrumental médico y laboratorio"] = "55000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Maquinaria, otros equipos y herramientas"] = "56000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Activos biológicos"] = "57000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Bienes inmuebles"] = "58000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Activos intangibles"] = "59000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público"] = "61000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Obra pública en bienes propios"] = "62000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Proyectos productivos y acciones de fomento"] = "63000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Provisiones para contingencias y otras erogaciones especiales"] = "79000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Amortización de la deuda pública"] = "91000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Intereses de la deuda pública"] = "92000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Comisiones de la deuda pública"] = "93000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Gastos de la deuda pública"] = "94000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Adeudos de ejercicios fiscales anteriores (ADEFAS)"] = "99000"
Egre15$Número[Egre15$DESCRIPCION_CATEGORIA == "Egresos diversos"] = " "

#### Convertimos la columna VALOR a tipo numérico ####
#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1))

Egre15$VALOR = as.numeric(as.character(Egre15$VALOR))

#### Eliminamos columnas inecesarias por posición (index).
# df2 <- df[,-2]
Egre15 = Egre15[,c(3, 5, 8, 9, 10)]

##### Cambiamos el nombre de la columna anio por AÑo
# colnames(df)[2] <- "new_col2"
colnames(Egre15)[1] = "Año"

#### Combinamos las columnas "Número" y "DESCRIPCION_CATEGORIA" en una sola ####

Egre15$Concepto = paste(Egre15$Número, Egre15$DESCRIPCION_CATEGORIA, sep=" ")

#### Agrupamos para sumar los valores de las categorías repetidas, de tal forma####
# que sea coherente la homogenización de conceptos entre el catálogo isaf e inegi.
# Además, aprovechamos este paso para eliminar las columnas "Número" y "DESCRIPCION_CATEGORIA"
Egre15 <- Egre15 %>% group_by(Año, ID_MUNICIPIO, Concepto) %>%
  summarise(sum = sum(VALOR), n = n())

#### Eliminamos la columna de frecuencia "n"
# Egre15 = Egre15[,c(3, 5, 8, 9, 10)]

Egre15 = Egre15[,c(1:4)]

# Cambiamos el nombre de la columna "sum" por "Valor"

colnames(Egre15)[4] = "Valor"

#### Aplicamos pivotwider sobre la columna "Concepto" ####

Egre15 <- Egre15 %>%
  pivot_wider(names_from = Concepto, values_from = Valor)


#### Cargamos la base de datos con los nombres de los municipios ####
mun <- read.csv("data/REGIONES_CPCEM.csv", encoding = 'UTF-8')


#### Agregamos la columna "Nom_mun" al df Egre15 ####
Egre15$Municipio <- mun$NOM_MUN


#### Relocalizamos la columna "Municipio" del df Egreso21 ####
# df  = df %>% relocate(x, .after=y)

Egre15 = Egre15 %>% relocate(Municipio, .after=ID_MUNICIPIO)


################################ EGRESOS 2014 ###################################################


Egre14 <- read.csv("data/Egre/EgreSon14.csv", encoding = 'UTF-8')

#### Agregamos una columna que correponda a la clave de los **EGRESOS** ####
Egre14 <- Egre14 %>%
  add_column(Número = "constant_value") 

#### Relocalizamos la columna "Número" ####
Egre14 = Egre14 %>% relocate(Número, .before = VALOR)

#### Cambiamos los valores de por "11000" porque, por algún motivo, el valor ####
# de "Remuneraciones al personal" no se cambia
Egre14$Número[Egre14$Número == "constant_value"] <- "11000"

#### Cambiamos algunos conceptos para que embone el diccionario de ISAF con el de INEGI
Egre14$DESCRIPCION_CATEGORIA[Egre14$DESCRIPCION_CATEGORIA == "Bienes muebles, inmuebles e intangibles diversos"] <- "Bienes inmuebles"
Egre14$DESCRIPCION_CATEGORIA[Egre14$DESCRIPCION_CATEGORIA == "Materiales y suministros diversos"] <- "Materiales y suministros para seguridad"


#### Cambiamos los valores de la columna "Número" de acuerdo a los valores de la columna ####
# "DESCRIPCION_CATEGORÍA"

Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Remuneraciones al personal"] = "11000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Remuneraciones adicionales y especiales"] = "13000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Seguridad social"] = "14000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Otras prestaciones sociales y económicas"] = "15000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Previsiones"] = "16000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Pago de estímulos a servidores públicos"] = "17000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Materiales de administración, emisión de documentos y artículos oficiales"] = "21000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Alimentos y utensilios"] = "22000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Materiales y artículos de construcción y de reparación"] = "24000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Productos químicos, farmacéuticos, laboratorio y materias primas de producción y comercialización"] = "25000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Combustibles, lubricantes y aditivos"] = "26000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Vestuario, blancos, prendas de protección y artículos deportivos"] = "27000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Materiales y suministros para seguridad"] = "28000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Herramientas, refacciones y accesorios menores"] = "29000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Servicios básicos"] = "31000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Servicios de arrendamiento"] = "32000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Servicios profesionales, científicos, técnicos y otros servicios"] = "33000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Servicios financieros, bancarios y comerciales"] = "34000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Servicios de instalación, reparación, mantenimiento y conservación"] = "35000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Servicios de comunicación social y publicidad"] = "36000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Servicios de traslado y viáticos"] = "37000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Servicios oficiales"] = "38000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Otros servicios generales"] = "39000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Transferencias internas y asignaciones al sector público"] = "41000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Subsidios y subvenciones"] = "43000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Ayudas sociales"] = "44000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Pensiones y jubilaciones"] = "45000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Mobiliario y equipo de administración"] = "51000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Mobiliario y equipo educacional y recreativo"] = "52000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Vehículos y equipo de transporte"] = "54000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Equipo de defensa y seguridad y equipo e instrumental médico y laboratorio"] = "55000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Maquinaria, otros equipos y herramientas"] = "56000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Activos biológicos"] = "57000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Bienes inmuebles"] = "58000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Activos intangibles"] = "59000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público"] = "61000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Obra pública en bienes propios"] = "62000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Proyectos productivos y acciones de fomento"] = "63000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Inversiones para el fomento de actividades productivas"] = "71000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Provisiones para contingencias y otras erogaciones especiales"] = "79000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Amortización de la deuda pública"] = "91000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Intereses de la deuda pública"] = "92000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Comisiones de la deuda pública"] = "93000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Gastos de la deuda pública"] = "94000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Adeudos de ejercicios fiscales anteriores (ADEFAS)"] = "99000"
Egre14$Número[Egre14$DESCRIPCION_CATEGORIA == "Egresos diversos"] = " "

#### Convertimos la columna VALOR a tipo numérico ####
#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1))

Egre14$VALOR = as.numeric(as.character(Egre14$VALOR))

#### Eliminamos columnas inecesarias por posición (index).
# df2 <- df[,-2]
Egre14 = Egre14[,c(3, 5, 8, 9, 10)]

##### Cambiamos el nombre de la columna anio por AÑo
# colnames(df)[2] <- "new_col2"
colnames(Egre14)[1] = "Año"

#### Combinamos las columnas "Número" y "DESCRIPCION_CATEGORIA" en una sola ####

Egre14$Concepto = paste(Egre14$Número, Egre14$DESCRIPCION_CATEGORIA, sep=" ")

#### Agrupamos para sumar los valores de las categorías repetidas, de tal forma####
# que sea coherente la homogenización de conceptos entre el catálogo isaf e inegi.
# Además, aprovechamos este paso para eliminar las columnas "Número" y "DESCRIPCION_CATEGORIA"
Egre14 <- Egre14 %>% group_by(Año, ID_MUNICIPIO, Concepto) %>%
  summarise(sum = sum(VALOR), n = n())

#### Eliminamos la columna de frecuencia "n"
# Egre14 = Egre14[,c(3, 5, 8, 9, 10)]

Egre14 = Egre14[,c(1:4)]

# Cambiamos el nombre de la columna "sum" por "Valor"

colnames(Egre14)[4] = "Valor"

#### Aplicamos pivotwider sobre la columna "Concepto" ####

Egre14 <- Egre14 %>%
  pivot_wider(names_from = Concepto, values_from = Valor)


#### Cargamos la base de datos con los nombres de los municipios ####
mun <- read.csv("data/REGIONES_CPCEM.csv", encoding = 'UTF-8')


#### Agregamos la columna "Nom_mun" al df Egre14 ####
Egre14$Municipio <- mun$NOM_MUN


#### Relocalizamos la columna "Municipio" del df Egreso21 ####
# df  = df %>% relocate(x, .after=y)

Egre14 = Egre14 %>% relocate(Municipio, .after=ID_MUNICIPIO)



#################################### EGRESOS 2013 ################################################

Egre13 <- read.csv("data/Egre/EgreSon13.csv", encoding = 'UTF-8')

#### Agregamos una columna que correponda a la clave de los **EGRESOS** ####
Egre13 <- Egre13 %>%
  add_column(Número = "constant_value") 

#### Relocalizamos la columna "Número" ####
Egre13 = Egre13 %>% relocate(Número, .before = VALOR)

#### Cambiamos los valores de por "11000" porque, por algún motivo, el valor ####
# de "Remuneraciones al personal" no se cambia
Egre13$Número[Egre13$Número == "constant_value"] <- "11000"

#### Cambiamos algunos conceptos para que embone el diccionario de ISAF con el de INEGI
Egre13$DESCRIPCION_CATEGORIA[Egre13$DESCRIPCION_CATEGORIA == "Bienes muebles, inmuebles e intangibles diversos"] <- "Bienes inmuebles"
Egre13$DESCRIPCION_CATEGORIA[Egre13$DESCRIPCION_CATEGORIA == "Materiales y suministros diversos"] <- "Materiales y suministros para seguridad"


#### Cambiamos los valores de la columna "Número" de acuerdo a los valores de la columna ####
# "DESCRIPCION_CATEGORÍA"

Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Remuneraciones al personal"] = "11000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Remuneraciones adicionales y especiales"] = "13000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Seguridad social"] = "14000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Otras prestaciones sociales y económicas"] = "15000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Previsiones"] = "16000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Pago de estímulos a servidores públicos"] = "17000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Materiales de administración, emisión de documentos y artículos oficiales"] = "21000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Alimentos y utensilios"] = "22000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Materiales y artículos de construcción y de reparación"] = "24000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Productos químicos, farmacéuticos, laboratorio y materias primas de producción y comercialización"] = "25000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Combustibles, lubricantes y aditivos"] = "26000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Vestuario, blancos, prendas de protección y artículos deportivos"] = "27000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Materiales y suministros para seguridad"] = "28000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Herramientas, refacciones y accesorios menores"] = "29000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Servicios básicos"] = "31000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Servicios de arrendamiento"] = "32000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Servicios profesionales, científicos, técnicos y otros servicios"] = "33000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Servicios financieros, bancarios y comerciales"] = "34000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Servicios de instalación, reparación, mantenimiento y conservación"] = "35000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Servicios de comunicación social y publicidad"] = "36000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Servicios de traslado y viáticos"] = "37000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Servicios oficiales"] = "38000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Otros servicios generales"] = "39000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Transferencias internas y asignaciones al sector público"] = "41000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Subsidios y subvenciones"] = "43000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Ayudas sociales"] = "44000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Pensiones y jubilaciones"] = "45000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Mobiliario y equipo de administración"] = "51000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Mobiliario y equipo educacional y recreativo"] = "52000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Vehículos y equipo de transporte"] = "54000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Equipo de defensa y seguridad y equipo e instrumental médico y laboratorio"] = "55000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Maquinaria, otros equipos y herramientas"] = "56000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Activos biológicos"] = "57000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Bienes inmuebles"] = "58000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Activos intangibles"] = "59000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Obra pública en bienes de dominio público"] = "61000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Obra pública en bienes propios"] = "62000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Proyectos productivos y acciones de fomento"] = "63000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Inversiones para el fomento de actividades productivas"] = "71000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Provisiones para contingencias y otras erogaciones especiales"] = "79000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Amortización de la deuda pública"] = "91000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Intereses de la deuda pública"] = "92000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Comisiones de la deuda pública"] = "93000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Gastos de la deuda pública"] = "94000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Adeudos de ejercicios fiscales anteriores (ADEFAS)"] = "99000"
Egre13$Número[Egre13$DESCRIPCION_CATEGORIA == "Egresos diversos"] = " "

#### Convertimos la columna VALOR a tipo numérico ####
#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1))

Egre13$VALOR = as.numeric(as.character(Egre13$VALOR))

#### Eliminamos columnas inecesarias por posición (index).
# df2 <- df[,-2]
Egre13 = Egre13[,c(3, 5, 8, 9, 10)]

##### Cambiamos el nombre de la columna anio por Año
# colnames(df)[2] <- "new_col2"
colnames(Egre13)[1] = "Año"


#### Combinamos las columnas "Número" y "DESCRIPCION_CATEGORIA" en una sola ####

Egre13$Concepto = paste(Egre13$Número, Egre13$DESCRIPCION_CATEGORIA, sep=" ")

#### Agrupamos para sumar los valores de las categorías repetidas, de tal forma####
# que sea coherente la homogenización de conceptos entre el catálogo isaf e inegi.
# Además, aprovechamos este paso para eliminar las columnas "Número" y "DESCRIPCION_CATEGORIA"
Egre13 <- Egre13 %>% group_by(Año, ID_MUNICIPIO, Concepto) %>%
  summarise(sum = sum(VALOR), n = n())

#### Eliminamos la columna de frecuencia "n"
# Egre13 = Egre13[,c(3, 5, 8, 9, 10)]

Egre13 = Egre13[,c(1:4)]

# Cambiamos el nombre de la columna "sum" por "Valor"

colnames(Egre13)[4] = "Valor"

#### Aplicamos pivotwider sobre la columna "Concepto" ####

Egre13 <- Egre13 %>%
  pivot_wider(names_from = Concepto, values_from = Valor)


#### Cargamos la base de datos con los nombres de los municipios ####
mun <- read.csv("data/REGIONES_CPCEM.csv", encoding = 'UTF-8')


#### Agregamos la columna "Nom_mun" al df Egre13 ####
Egre13$Municipio <- mun$NOM_MUN


#### Relocalizamos la columna "Municipio" del df Egreso21 ####
# df  = df %>% relocate(x, .after=y)

Egre13 = Egre13 %>% relocate(Municipio, .after=ID_MUNICIPIO)


###################################### Tablas totales 13-21 ##########################################

EgreSon1321 = bind_rows(Egre21, Egre20, Egre19, Egre18,
                        Egre17, Egre16, Egre15, Egre14, Egre13)
colnames(EgreSon1321)

#### Guardamos las tres primeras columnas en otro objeto y ####
# aplicamos la función "sort" para ordenar alfabéticamente ####

trescol = EgreSon1321[, 1:3]

new_order = sort(colnames(EgreSon1321[, 4:49]))
EgreSon1321 <- EgreSon1321[, new_order]

#### Añadimos las columnas faltantes: Año, ID_MUNICIPIO, Municipio ####
# df2 <- cbind(df, chapters)
EgreSon1321 = cbind(EgreSon1321, trescol)

#### Relocalizamos las columnas Año, ID_MUNICIPIO, Municipio  ####

EgreSon1321 = EgreSon1321 %>% relocate(Año, .before="  Egresos diversos") %>% 
 relocate("ID_MUNICIPIO", .after=Año) %>% 
 relocate(Municipio, .after="ID_MUNICIPIO")

#### Relocalizamos la columna " Egresos diversos" ####

EgreSon1321 = EgreSon1321 %>% relocate("  Egresos diversos", .after="99000 Adeudos de ejercicios fiscales anteriores (ADEFAS)")

#### Crear un archivo csv del df EgreSon1321 ####
# write.csv(DataFrame Name, "Path to export the DataFrame\\File Name.csv", row.names=FALSE)
#write.csv(EgreSon1321, "C:/Users/santi/Documents/MCD_UNISON/SEGUNDO_SEMESTRE/BI/DashShiny/data/Egre/EgreSon1321.csv", row.names=FALSE)

#### Creamos una tabla diferente con "pivot longer", para el manejo de información ####

EgreSon1321_L <- EgreSon1321 %>%
  pivot_longer(cols = c(4:49), names_to = 'Concepto', 
               values_to = 'Valor')


##### Guardamos la base EgreSon1321_L en un csv #####

#write.csv(EgreSon1321_L, "C:/Users/santi/Documents/MCD_UNISON/SEGUNDO_SEMESTRE/BI/DashShiny/data/Egre/EgreSon1321_L.csv", row.names=FALSE)


################################################## Gráfica ##########################################

# Ejemplo = ggplot(data  = subset(EgreSon1321_L, Municipio=='Hermosillo' & Concepto=='99000 Adeudos de ejercicios fiscales anteriores (ADEFAS)'), 
#        aes(x= Año, y= Valor)) +
#   geom_point() +
#   geom_line()
# 
# ggplotly(Ejemplo)
