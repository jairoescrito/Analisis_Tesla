# Ejercicio de muestra de trabajo en R para análisis de datos
# y algunas tareas de machine learning

## Paso 1. Obtención de datos y limpieza del dataset
# Cagar datos a una variable dataframe
Data <- read.csv("card_transdata.csv")
head(Data)
Variables <- data.frame(Variables = names(Data))
print(Variables)
# Dimensiones del conjunto de datos
cat("El número de resgistros (filas) es de: ", 
    format(nrow(Data),big.mark = ",")) # Total de observaciones o registros (filas)
cat("\nEl número de columnas (variables) es de: "
    , ncol(Data)) # Total de variables o atributos (columnas)
# Evaluación NA's o missign values
library(VIM)
Nas <- aggr(Data)
summary(Nas)
# Detalles iniciales de cada variable
numericas <- c(1,2,3) # vector con los índices de las columnas númericas del dataset
i <- 1
while (i<=ncol(Data)){
  print(names(Data[i]))
  Var <- Data[,i]
  if (!(i %in% numericas)){
    Var <- as.factor(Var)
  }
  print(class(Var))
  print(summary(Var))
  i = i + 1
}

# Visualización de comportamiento de las variables

Frecuencias_Num <- function(Variable) #PARA CONTINUAR
  
  
  
Min <- trunc(min(Data$distance_from_home)) # Minimo de los datos redondeado por abajo
Max <- ceiling(max(Data$distance_from_home))# Maximo de los datos redondeado por arriba
Rango <- Max - Min # El rango de variaci?n de los datos
Datos <- length(Data$distance_from_home) # Para la cantidad de datos que tiene la variable Edad
NumIntervalos <- ceiling(1+log2(Datos)) # Cantidad de intervalos, redondeado para obtener el numero entero
Ancho <- ceiling(Rango/NumIntervalos) # Amplitud - Ancho de cada intervalo
Inferior <- NULL
for (i in 0:(NumIntervalos-1)){
  Inferior <- rbind(Inferior,Min+Ancho*i)
}
Inferior
Superior <- NULL
for (i in 1:NumIntervalos){
  Superior <- rbind(Superior,(Min+Ancho*i))
}
Superior
Tabla <- data.frame(Inferior,Superior) # Intervalos para tabla de frecuencias
lim <- seq(Min,length.out=NumIntervalos+1,by=Ancho) 
Histograma <- hist(Data$distance_from_home, breaks = lim,
                   main = "Histograma de Distancia desde dirección del Hogar",
                   xlab = "Intervalos para distancia",
                   ylab = "Frecuencia",
                   col = "darkgreen")



Tabla$FA <- Histograma$counts
Tabla$FR <- round(Tabla$FA/length(Data$Edad),3)*100
Tabla$FAcum <- cumsum(Tabla$FR)




