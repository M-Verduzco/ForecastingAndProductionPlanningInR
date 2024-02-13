
# ------------------------------------------------------------------------------

#   PROYECTO FINAL PCP POR: Mariana Zapata, Mauricio Verduzco y Chan Mun Won

# ------------------------------------------------------------------------------


library(readxl) #libreria excel
library(ggplot2)
library(fpp2)

# 1. Analisis de la demanda

datos <- read_excel("dataAutos.xlsx", sheet = "RAIAVL_11", col_names = TRUE)

datos <- datos[-1, ]
datos <- datos[-1, ]
datos <- datos[, -1]
datos <- datos[, -1]
datos <- datos[, -1]
datos

datos=strtoi(array(unlist(datos)))
print(datos)



serie_tiempo_autos <- ts(datos, start = c(2016,1), end = c(2021, 4), frequency = 12)
serie_tiempo_autos




ggAcf(serie_tiempo_autos, lag.max = 64)
Box.test(serie_tiempo_autos, lag = 1, type = "Ljung")
plot(serie_tiempo_autos)
#conclusion: estos datos parecen no ser aleatorios (p-value = 7.47e-9)

# 2. Ajustar los datos a un modelo

# meanf() promedio
# naive() naive
# ses() simple exponential smoothing
# holt() metodo para tendencias
# hw(aditivo) estacionalidad
# hw(multiplicativo) estacionalidad

datasetA <- window(serie_tiempo_autos, end = c(2020, 3))
trainingA <- window(serie_tiempo_autos, end=c(2019,9))
testA <- window(serie_tiempo_autos, start=c(2019,10), end = c(2020, 3))


horizon <- 6
# horizon cuantos datos queremos pronosticar (de septiembre 2014 a Julio 2015)

Ametodo_promedio <- meanf(trainingA, h = horizon)
Ametodo_naive <- naive(trainingA, h = horizon)
Ametodo_ses <- ses(trainingA,  initial = c("optimal"), h = horizon)
# initial = c("optimal") R encuentra el valor de alpha mas pequeño y el pronostico del primer periodo
Ametodo_holt <- holt(trainingA, h = horizon)
Ametodo_hw_a <- hw(trainingA, seasonal = "additive", h = horizon)
# additivo
Ametodo_hw_m <- hw(trainingA, seasonal = "multiplicative", h = horizon)
# multiplicativo

# 3. Seleccionar el mejor modelo que me entregue la menor RMSE (desviacion estandar) en los datos de prueba
# accuracy(____, test)

accuracy(Ametodo_promedio, testA)
accuracy(Ametodo_naive, testA)## <-- este es el más pequeño en ese caso
accuracy(Ametodo_ses, testA)
accuracy(Ametodo_holt, testA)
accuracy(Ametodo_hw_a, testA) 
accuracy(Ametodo_hw_m, testA)

## RESPUESTA: seleccionar hw_additive ya que es el que tiene el menor valor de la desviación estándar
# OJO: tenemos que elegir el valor del "test", es decir, los datos de PRUEBA, PRONOSTICADOS

# 4. Graficar los datos con el pronostico

autoplot(trainingA, serie = "training A") + 
  autolayer(Ametodo_naive$fitted, serie = "pronostico A") +
  autolayer(Ametodo_ses$fitted, serie = "ses A") +
  autolayer(Ametodo_hw_a)
# serie nos da una etiqueta

# 5. Pronostico para el plan agregado de produccion
# agarramos todos los datos anteriores para poder tener un pronostico

pronosticoA <- naive(datasetA, h = horizon)

# 6. Residuales probar que los valores son aleatorios

ggAcf(pronosticoA$residuals, lag.max = 100) + ggtitle("Prueba residuales")
Box.test(pronosticoA$residuals, lag = 1, type = "Ljung")
# queremos que el p-value sea mayor a 0.05 para que sea la evidencia aleatoria.
# En este caso el resultado es p-value = 0.03643, entonces los residuales sí son ruido blanco






datasetB <- serie_tiempo_autos
trainingB <- window(serie_tiempo_autos, end=c(2020,10))
testB <- window(serie_tiempo_autos, start=c(2020,11))



horizon <- 6
# horizon cuantos datos queremos pronosticar (de septiembre 2014 a Julio 2015)

Bmetodo_promedio <- meanf(trainingB, h = horizon)
Bmetodo_naive <- naive(trainingB, h = horizon)
Bmetodo_ses <- ses(trainingB,  initial = c("optimal"), h = horizon)
# initial = c("optimal") R encuentra el valor de alpha mas pequeño y el pronostico del primer periodo
Bmetodo_holt <- holt(trainingB, h = horizon)
Bmetodo_hw_a <- hw(trainingB, seasonal = "additive", h = horizon)
# additivo
Bmetodo_hw_m <- hw(trainingB, seasonal = "multiplicative", h = horizon)
# multiplicativo

# 3. Seleccionar el mejor modelo que me entregue la menor RMSE (desviacion estandar) en los datos de prueba
# accuracy(____, test)

accuracy(Bmetodo_promedio, testB)
accuracy(Bmetodo_naive, testB) ## <-- este es el más pequeño en ese caso
accuracy(Bmetodo_ses, testB)
accuracy(Bmetodo_holt, testB)
accuracy(Bmetodo_hw_a, testB) 
accuracy(Bmetodo_hw_m, testB)

## RESPUESTA: seleccionar NAIVE ya que es el que tiene el menor valor de la desviación estándar
# OJO: tenemos que elegir el valor del "test", es decir, los datos de PRUEBA, PRONOSTICADOS

# 4. Graficar los datos con el pronostico

autoplot(trainingB, serie = "training B") + 
  autolayer(Bmetodo_naive$fitted, serie = "pronostico B") +
  autolayer(Bmetodo_ses$fitted, serie = "ses B") +
  autolayer(Bmetodo_naive)
# serie nos da una etiqueta





# 5. Pronostico para el plan agregado de produccion
# agarramos todos los datos anteriores para poder tener un pronostico

pronosticoB <- naive(datasetB, h = horizon)

# 6. Residuales probar que los valores son aleatorios

ggAcf(pronosticoB$residuals, lag.max = 100) + ggtitle("Prueba residuales")
Box.test(pronosticoB$residuals, lag = 1, type = "Ljung")
# queremos que el p-value sea mayor a 0.05 para que sea la evidencia aleatoria


pronosticoA


pronosticoB



# ------------------------------------------------------------------------------

#                                CONCLUSIONES

# ------------------------------------------------------------------------------

# En este proyecto se analizó la demanda de autos híbridos en la Ciudad de México 
# de enero 2016 a abril 2021. Se hicieron dos pruebas: la A va de enero 2016 a la
# marzo 2020 y la B de enero 2016 a abril 2021.

# Para empezar se descargó la base de datos sacada del INEGI como un archivo de 
# Excel y se leyó en un script de R. Ya en R se limpió para poder ser utilizada:
# se quitaron los nombres de columnas, los renglones y columnas con nans, así
# como todas las columnas que no tuvieran los datos de demanda. Esto dio como
# resultado una lista que convertimos a un array para poder convertirlo a serie
# de tiempo.

# Ya con la serie de tiempo comenzamos las pruebas de accuracy para encontrar el
# método adecuado para el pronóstico de la demanda, dando como resultado el método
# NAIVE para ambas pruebas. Se tomo el Naive ya que es el de menor RMSE y el 
# único con un valor de Theil's U menor o igual a uno. 

# A pesar de esto, las pruebas de residuales nos indicaron que este no fue un 
# buen pronóstico. Esto se concluyó de la prueba de Ljung-Box, en donde obtuvimos
# 0.0354 y 0.364 para las pruebas A y B respectivamente. Al ser valores menores a
# 0.05 concluimos que los datos no son aleatorios y existe correlación entre ellos.

# Lo anterior es evidente al analizar los resultados de la demanda que obtuvimos,
# ya que salió el mismo valor para los 6 meses que pronosticamos. Esto no es el
# comportamiento común en ninguna demanda, por lo que podemos concluir que el 
# método Naive no fue el mejor para realizar el pronóstico.

#Los seis métodos probados fueron:
#-----INGENUO (NAIVE)
#-----PROMEDIO
#-----SUAVIZACIÓN EXPONENCIAL SIMPLE
#-----TENDENCIA LINEAL DE HOLT
#-----HOLT-WINTERS ADITIVO
#-----HOLT-WINTERS MULTIPLICATIVO















