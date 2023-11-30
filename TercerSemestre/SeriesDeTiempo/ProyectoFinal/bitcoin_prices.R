### Install packages----
# install.packages(plotly) # for candlestick chart
# install.packages(quantmod) # for financial data from the US stock market

### Set up libraries ---
library(plotly)
library(quantmod)

### Get Bitcoin-USD data from yahoo finance ----
getSymbols("BTC-USD",src='yahoo')

### Set dataframe ----
df <- data.frame(Date=index(`BTC-USD`),coredata(`BTC-USD`))
# df <- tail(df, 30)

### Set candlestick chart ----
candlestick <- df %>% plot_ly(x = df$Date, type="candlestick", # set x axis data and kind of chart
                      open = ~BTC.USD.Open, close = ~BTC.USD.Close, # set dates when markets open and close
                      high = ~BTC.USD.High, low = ~BTC.USD.Low) # set higher and lower stock prices 

candlestick <- candlestick %>% layout(title = "Daily Bitcoin stock prices: September 17, 2014 - today", # chart title
                      xaxis = list(rangeslider = list(visible = F))) 

# candlestick <- add_trace(candlestick, x = 0.3, y = 4, type = "scatter", mode = "markers", color = I("red"), inherit = FALSE, name = "myPoint")


candlestick

### Patterns ----
# https://www.investopedia.com/articles/technical/112601.asp
# https://www.quora.com/How-far-are-the-chart-patterns-reliable-for-stock-trading


### Flag pattern----
# Flags are continuation patterns constructed using
# two parallel trendlines that can slope up, down, or
# sideways (horizontal). Generally, a flag with an
# upward slope (bullish) appears as a pause in a
# down trending market; a flag with a downward
# bias (bearish) shows a break during an up
# trending market. Typically, the flag's formation
# is accompanied by declining volume, which recovers
# as price breaks out of the flag formation.
##

# 

# Definimos "nrow" como el número de renglones de nuestro df. Esto facilita el definirlo para
# próximas funciones. Se coloca el "-1" porque el último renglon de df sólo tiene NA's.

nrow <- nrow(df)-1


# Definición de parámetros o arreglos auxiliares:
# 
#   - Definir amplitud de un pivote (decir que se consideran n puntos adyacentes para poder definir un punto como pivote en la gráfica)
# 
#   - Crear dos arreglos para determinar los pivotes obtenidos con los parámetros de low y de high


n = 5

Pivotlow <- rep(1, nrow)
Pivothigh <- rep(1, nrow)

 
# Pseudocódigo:
  
# Marcamos a los primeros y últimos n elementos de dataframe con 0 para señalar que no pueden ser pivotes


  for (i in 1:n){
    
    Pivotlow[i] <- 0
    
    Pivotlow[nrow+1-i] <- 0
    
    Pivothigh[i] <- 0
    
    Pivothigh[nrow+1-i] <- 0
    
  }


# Terminar de llenar los arreglos de Pivotlow y Pivothigh con 0 o 1 para encontrar todos los pivotes de la gráfica considerando los parámetros de low y high


Nelements <- (n+1) : (nrow-n) # Entradas del Dataframe a considerar para posibles pivotes

for (i in Nelements){
  
  # En este ciclo comparamos el elemento i del dataframe con 
  # sus n vecinos de cada lado para establecer si es algún 
  # tipo de pivote a basTerminar de llenar los arreglos de Pivotlow y Pivothigh con 0 o 1 para encontrar todos los pivotes de la gráfica considerando los parámetros de low y highe de cambiar el valor de los booleanos anteriores
  
  for (j in (i - n):(i + n)) {
    if (df$BTC.USD.Low[i] > df$BTC.USD.Low[j]) {
      Pivotlow[i] <- 0
    }
    if (df$BTC.USD.High[i] < df$BTC.USD.High[j]) {
      Pivothigh[i] <- 0
    }
  
                                  }

                    }


# 
# Lo anterior nos dejar con dos arreglos Pivotlow y Pivothigh que consiste de puros 0’s y 1’s,
# de tal manera que si Pivotlow[i] = 0 significa que el valor low de la i-esima entrada
# del dataframe no es un pivote, mientras que Pivotlow[i] = 1 significaria que el
# valor low de la i-esima entrada de del dataframe si es un pivote. Analogo para Pivothigh. Esta
# es una aplicación del uso de valores booleanos.

# Procedemos a marcar los pivotes encontrados en la grafica,

# for (i in 1:nrow){
#   
#   if (Pivotlow[i] == 1){
#     
    # candlestick <- add_trace(candlestick, x = df$Date[i],
    #                          y = df$BTC.USD.Low[i] , type = "scatter",
    #                          mode = "markers", color = I("red"), inherit = FALSE)
    #                          # name = "myPoint")
#     
#                         }
#   
#   if (Pivothigh[i] == 1){
#     
#     candlestick <- add_trace(candlestick, x = df$Date[i], 
#                              y =  df$BTC.USD.High[i] , 
#                              type = "scatter", mode = "markers", color = I("green"), inherit = FALSE)
#                              # name = "myPoint"
#     
#                         }
#                         
# }

# Npivotlow es el número de pivotes inferiores en la gráfica "candlestick"; análogo
# para Npivothigh
Npivotlow = 0
Npivothigh = 0

# Se recorren los arreglos de booleanos y se cuenta la cantidad de "1's" en cada uno.
# Cada cantidad de "1's" es la cantidad de pivotes en cada arreglo.
for (i in 1:nrow){
  if (Pivotlow[i] == 1){ Npivotlow <- Npivotlow +1}
  if (Pivothigh[i] == 1){ Npivothigh <- Npivothigh +1}
}

# Se declararon arreglos en los que se guardan los datos de fecha (date) y 
# valores (pivotlow o pivothigh).
PivotlowDate <- df$Date[1:Npivotlow]
PivotLow <- rep(0, Npivotlow) # Se llenará este arreglo con el número de valores pivotlow
PivothighDate <- df$Date[1:Npivothigh]
PivotHigh <- rep(0, Npivothigh)

# Índice auxiliar. Se usan para llenar los cuatro 
#arreglos mencionados en las líneas anteriores. Su
# utilidad se refleja en el ciclo for a continuación
i_low=1
i_high=1

# El siguiente ciclo for es para llenar los valores de los arreglos
# PivotlowDate, PivotLow, PivothighDate y PivotHigh, usando los índices auxiliares
for (i in 1:nrow){
  if (Pivotlow[i] == 1){ 
    PivotlowDate[i_low] <- df$Date[i]
    PivotLow[i_low] <- df$BTC.USD.Low[i]
    i_low <- i_low + 1
  }
  if (Pivothigh[i] == 1){ 
    PivothighDate[i_high] <- df$Date[i]
    PivotHigh[i_high] <- df$BTC.USD.High[i]
    i_high <- i_high + 1
  }
}


# Se colocan los puntos sobre el objeto "candlestick" previamente creado.
candlestick <- add_trace(candlestick, x = PivotlowDate,
                         y = PivotLow , type = "scatter",
                         mode = "markers", color = I("#FFAC33"), inherit = FALSE,  
                         name = "PivotLow")

candlestick <- add_trace(candlestick, x = PivothighDate,
                         y = PivotHigh , type = "scatter",
                         mode = "markers", color = I("skyblue"), inherit = FALSE,  
                         name = "PivotHigh")

candlestick

### Regresion lineal para crear las banderas ----

# Se usa el comando lm() para generar las lineas, tanto en los valores
# PivotLow como PivotHigh, que darán forma a la bandera.


# PivotLow
df_PivotLow.lm <- data.frame(PivotLow, PivotlowDate)


matrix_coef_PivotLow.lm  <- data.frame(summary(lm(formula = PivotLow ~ PivotlowDate,
                              data = df_PivotLow.lm))$coefficients)$Estimate[2]

matrix_coef_PivotLow.lm

# PivotHigh
df_PivotHigh.lm <- data.frame(PivotHigh, PivothighDate)


matrix_coef_PivotHigh.lm  <- data.frame(summary(lm(formula = PivotHigh ~ PivothighDate,
                                                  data = df_PivotHigh.lm))$coefficients)

matrix_coef_PivotHigh.lm$Estimate[2]



# Creamos arreglos para guardar los valores de las pendientes de los pivotes low y high

PivotLow.lm_slope <- rep(0, Npivotlow - 5)

PivotHigh.lm_slope <- rep(0, Npivothigh - 5)



# Creamos arreglos para guardar los valores de los interceptos de los pivotes low y high

PivotLow.lm_inter <- rep(0, Npivotlow - 5)

PivotHigh.lm_inter <- rep(0, Npivothigh - 5)




# Aplicamos un ciclo for para generar una pendiente e interceptos
# en cada 6 puntos pivote


for (i in 1:(Npivotlow - 5)){
  
  PivotLow_i <- PivotLow[(i):(i+5)]
  PivotLowDate_i <- PivotlowDate[(i):(i+5)]
  
  df_PivotLow.lm_i <- data.frame(PivotLow_i, PivotLowDate_i)
  
  
  PivotLow.lm_slope[i] <- data.frame(summary(lm(formula = PivotLow_i ~ PivotLowDate_i,
                                                    data = df_PivotLow.lm_i))$coefficients)$Estimate[2]

  PivotLow.lm_inter[i] <- data.frame(summary(lm(formula = PivotLow_i ~ PivotLowDate_i,
                                                data = df_PivotLow.lm_i))$coefficients)$Estimate[1]
  

}


for (i in 1:(Npivothigh - 5)){
  
  PivotHigh_i <- PivotHigh[(i):(i+5)]
  PivotHighDate_i <- PivothighDate[(i):(i+5)]
  
  df_PivotHigh.lm_i <- data.frame(PivotHigh_i, PivotHighDate_i)
  
  
  PivotHigh.lm_slope[i] <- data.frame(summary(lm(formula = PivotHigh_i ~ PivotHighDate_i,
                                                data = df_PivotHigh.lm_i))$coefficients)$Estimate[2]
  
  PivotHigh.lm_inter[i] <- data.frame(summary(lm(formula = PivotHigh_i ~ PivotHighDate_i,
                                                 data = df_PivotHigh.lm_i))$coefficients)$Estimate[1]
  
}

# Verificamos que se hayan guardado las pendientes en los vectores "PivotLow.lm_slope"
# y "PivotHigh.lm_slope", así como los interceptos en
# "PivotLow.lm_inter" y "PivotHigh.lm_inter"

PivotLow.lm_slope
PivotHigh.lm_slope

PivotLow.lm_inter
PivotHigh.lm_inter

# 
# Creamos un objeto que guarde
# el tamaño de los vectores "PivotLow.lm_slope"  y "PivotHigh.lm_slope"

PivotLow.lm_slope_size <- length(PivotLow.lm_slope)
PivotHigh.lm_slope_size <- length(PivotHigh.lm_slope)

# Los siguientes ciclos for son para que se muestren las pendientes
# guardadas a lo largo de la serie de tiempo, tanto en el caso pivotlow
# como PivotHigh. La función "as.numeric()" aplicada al objeto "Date" llamado
# "PivotlowDate_num", devolverá el número de días transcurridos entre el 1 de
# enero de 1970, y la fecha del datos. OJO: esta función devulve días, horas, minutos
# o segundos, en función de el formato de fecha.


PivotlowDate_num <- as.numeric(PivotlowDate)
PivothighDate_num <- as.numeric(PivothighDate)

# El siguiente ciclo for genera las banderas sobre nuestro objeto "candlestick".
# Hay dos parámetros principales para determinar la cantidad de líneas paralelas
# que formarán los márgenes de las banderas:
# 1) La diferencia máxima en la pendiente de ambas líneas: si aumenta, se generarán
# más líneas (y, por tanto, baneras) porque significa una mayor tolerancia
# a qué tan paralelas serán los márgenes de la bandera. En nuestro caso
# lo determinaos como "<0.5".
# 2) Máximo número de diferencias de días entre el inicio de la
# línea de pivotes altos y línea de pivots bajos: si aumentamos este valor,
# aumentará el número de banderas, pues habrá una mayor tolerancia entre la diferencia
# de longitud entre la líneas que se forman con pivotes altos (pivothigh) y
# las líneas que se forman con los pivotes bajos (pivot low).


for(i in 1: PivotLow.lm_slope_size ){
  
  for( j in (1:PivotHigh.lm_slope_size)){
    
    if(abs(PivotHigh.lm_slope[j]-PivotLow.lm_slope[i]) < 0.5 & abs(PivothighDate_num[j]-PivotlowDate_num[i]) < 28){
      
      x_low = c(PivotlowDate[i], PivotlowDate[i+5])
      y_low = c((PivotLow.lm_slope[i]*PivotlowDate_num[i])+PivotLow.lm_inter[i] , 
                (PivotLow.lm_slope[i]*PivotlowDate_num[i+5])+PivotLow.lm_inter[i])
      candlestick <- add_lines(candlestick, x = x_low,
                               y = y_low, color = I("#FFAC33"))
      
      x_high = c(PivothighDate[j], PivothighDate[j+5])
      y_high = c((PivotHigh.lm_slope[j]*PivothighDate_num[j])+PivotHigh.lm_inter[j] , 
                 (PivotHigh.lm_slope[j]*PivothighDate_num[j+5])+PivotHigh.lm_inter[j])
      candlestick <- add_lines(candlestick, x = x_high,
                               y = y_high, color = I("skyblue"))
    
      i <- i+6 # Se utiliza "i+6" para considerar todos los pivotes 
      
    }
    
  }
  
}


# for(i in 1: PivotLow.lm_slope_size ){
#   
#   for( j in max(1,i-1) : min(i+1, PivotHigh.lm_slope_size)){
#     
#     print(abs(PivotHigh.lm_slope[j]-PivotLow.lm_slope[i]))
#     
#     if(abs(PivotHigh.lm_slope[j]-PivotLow.lm_slope[i]) < 0.0001){
#       
#       x_low = c(PivotlowDate[i], PivotlowDate[i+5])
#       y_low = c((PivotLow.lm_slope[i]*PivotlowDate_num[i])+PivotLow.lm_inter[i] , 
#                 (PivotLow.lm_slope[i]*PivotlowDate_num[i+5])+PivotLow.lm_inter[i])
#                 candlestick <- add_lines(candlestick, x = x_low,
#                 y = y_low, color = I("#FFAC33"))
#       
#       x_high = c(PivothighDate[j], PivothighDate[j+5])
#       y_high = c((PivotHigh.lm_slope[j]*PivothighDate_num[j])+PivotHigh.lm_inter[j] , 
#                 (PivotHigh.lm_slope[j]*PivothighDate_num[j+5])+PivotHigh.lm_inter[j])
#       candlestick <- add_lines(candlestick, x = x_high,
#                                y = y_high, color = I("skyblue"))
#       
#       i <- i+5 
#   
#       }
#   
#     }
# 
# }



candlestick



# Se colocan las líneas sobre el objeto "candlestick" previamente creado. Éstas corresponden
# a las pendientes generadas en el ciclo for anterior
# add_lines(p, x = NULL, y = NULL, z = NULL, ..., data = NULL, inherit = TRUE)


# candlestick <- add_lines(candlestick, x = x_low,
#                          y = y_low , z = NULL, data= NULL, inherit = TRUE)
# 
# candlestick <- add_trace(candlestick, x = PivothighDate,
#                          y = PivotHigh , type = "scatter",
#                          mode = "markers", color = I("skyblue"), inherit = FALSE,  
#                          name = "PivotHigh")
# 
# candlestick



# %Graficamos las rectas resultantes de las regresiones lineales que 
# mostraron tener pendientes similares
# X = c(PivotLow[i], PivotLow[i+5])
# Y = c( PivotLow.lm_slope[i]*PivotLow[i]+PivotLow.lm_inter[i] , PivotLow.lm_slope[i]*PivotLow[i+5]+PivotLow.lm_inter[i] )
# Plot (x,y)


# Referencias
# https://statisticsglobe.com/extract-regression-coefficients-of-linear-model-in-r