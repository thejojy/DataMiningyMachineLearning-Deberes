#2 Hands On: Data Quality and Pre-Processing


#1. Assessing Data Quality 


#Load the following packages: dplyr, na.tools, tidyimpute (version from github decisionpatterns/tidyimpute”)
# Instalar los paquetes
install.packages("dplyr")
install.packages("na.tools")
install.packages("devtools")  # Este paquete es necesario para instalar desde GitHub
# Instalar tidyimpute desde GitHub
devtools::install_github("decisionpatterns/tidyimpute")


#Load the carInsurance data set about the insurance risk rating of cars based on several characteristics of each car1
# Cargar el archivo .rdata
load("D:\\Javier\\Universidad\\Machine Learning\\Practica 2\\Data\\carInsurance.Rdata")

# Acceder a los datos del conjunto de datos
datos <- carIns



#(a) Check if there are any missing values.
#Tip: use the function any_na()
# Verificar si hay valores faltantes en el conjunto de datos
hay_faltantes <- anyNA(datos)#esta funcion devuelve un true si hay al menos un valor faltante 

if (hay_faltantes) {
  print("Hay valores faltantes en el conjunto de datos.")
} else {
  print("No hay valores faltantes en el conjunto de datos.")
}



#(b) Count the number of cases that have, at least, one missing value
# Contar el número de filas con al menos un valor faltante
num_faltantes <- sum(rowSums(is.na(datos)) > 0)

print(paste("Número de filas con al menos un valor faltante:", num_faltantes))



#(c) Create a new data set by removing all the cases that have missing values.
#Tip: use the function drop_rows_any_na()
# Cargar el paquete tidyimpute
library(tidyimpute)

# Crear un nuevo conjunto de datos sin casos con valores faltantes
datosllenos <- drop_rows_any_na(datos)



#(d) Create a new data set by imputing all the missing values with 0.
#Tip: explore the variants of the function impute()

# Imputar los valores faltantes con cero (0), donde falten valores se los pondra con 0
 datosnuevos <- impute(datos, method = "fixed", value = 0)


 
 
 #(e) Create a new data set by imputing the mean in all the columns which have double type values.
 # Convertir las columnas double a numérico
 datos_double <- datos
 columnas_double <- sapply(datos_double, is.double)
 datos_double[columnas_double] <- lapply(datos_double[columnas_double], as.numeric)
 
 # Imputar la media en las columnas double
 datosmean <- impute(datos_double, method = "mean")
 
 
 
 
 #(f) Create a new data set by imputing the mode in all the columns which have integer type values
 datos_integer <- datos
 columnas_integer<- sapply(datos_integer, is.integer)
 datos_integer[columnas_integer] <- lapply(datos_integer[columnas_integer], as.numeric)
 
 datosmode <- impute(datos_integer, method = "mode")
 
 
 
 #(g) Create a new data set by imputing the most frequent value to the column ”nDoors”.
 #Tip: use the function impute_replace()
 # Reemplazar el valor más frecuente en la columna "nDoors"
 datos_imputados <- impute_replace(datos, method = "mode", cols = "nDoors")
 
 
 
 
 #(h) Combine the three last imputations to obtain a final dataset. Are there any duplicated cases?
 #Tip: use the functions distinct() and count()
 # Combinar las tres imputaciones
 conjunto_final <- bind_rows(datosmean, datosmode, datos_imputados)
 
 # Verificar casos duplicados, distinc para eliminar las filas duplicadas
 casos_duplicados <- conjunto_final %>% distinct() %>% count() %>% filter(n > 1)
 
 
 #2. Data Pre-Processing
 #2. Load the package dlookr. Use the same car insurance data set above and apply the following
 #transformations to the price attribute. Be critical regarding the obtained results.
 # Instalar el paquete dlookr
 install.packages("dlookr")
 
 # Cargar el paquete dlookr
 library(dlookr)
 
 
 #(a) Apply range-based normalization and z-score normalization.
 #Tip: use the function transform().
 # Aplicar normalización basada en el rango
 datos_norm_range <- datos %>% 
   mutate(price_range = (price - min(price)) / (max(price) - min(price)))
 
 # Aplicar normalización Z-score
 datos_norm_zscore <- datos %>% 
   mutate(price_zscore = scale(price))
 
 
 
 
 #(b) Discretize it into 4 equal-frequency ranges an into 4 equal-width ranges.
 #Tip: use the function binning().
 carInsurance_binned <- datos %>%
   mutate(price_bin = cut(price, breaks = 4, labels = FALSE))
 
 
 
 
 #3. With the seed 111019 obtain the following samples on the car insurance data set.
 #Tip: use the function sample_frac().
 set.seed(111019)
 
 
 #(a) A random sample of 60% of the cases, with replacement
 # Muestra aleatoria del 60% de los datos
 muestra_60 <- sample_frac(datos, 0.6, replace = TRUE)
 
 
 
 
 
 #(b) A stratified sample of 60% of the cases of cars, according to the fuelType attribute
 # Definir la fracción de muestra deseada
 fraction <- 0.6
 
 # Obtener la muestra estratificada
 muestra_estratificada <- datos %>% 
   group_by(fuelType) %>% 
   slice_sample(prop = fraction, replace = FALSE)
 
 # Verificar el tamaño de la muestra
 nrow(muestra_estratificada)
 
 
 
 #(c) Use the table() function to inspect the distribution of values in each of the two samples above.
 # Inspeccionar la distribución de valores en la muestra aleatoria simple
 table(muestra_60$fuelType)
 
 # Inspeccionar la distribución de valores en la muestra estratificada
 table(muestra_estratificada$fuelType)
 
 
 
 #4. Load the package corrplot and select the numeric attributes of the car insurance data set.
 install.packages("corrplot")
 library(corrplot)
 
 # Seleccionar atributos numéricos
 atributos_numericos <- select_if(datos, is.numeric)
 
 
 
 
 #(a) Using the function cor(), obtain the pearson correlation coefficient between each pair of variables
 # Calcular el coeficiente de correlación de Pearson
 matriz_correlacion <- cor(atributos_numericos)
 
 # Imprimir la matriz de correlación
 print(matriz_correlacion)
 
 
 
 
 
 #(b) Apply the function cor.mtest() to the previous result to calculate the p-values and confidence
 #intervals of the correlation coefficient for each pair of variables.
 # Calcular los valores p y los intervalos de confianza
 cor_test <- cor.mtest(atributos_numericos)
 
 # Imprimir los resultados
 print(cor_test)
 
 
 
 
 #(c) Plot the all correlation information using the function corrplot. Explore some of its parameters.
 # Graficar la matriz de correlación
 corrplot(matriz_correlacion, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
 
 
 
 
 

 
