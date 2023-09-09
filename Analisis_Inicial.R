# Ejercicio de muestra de trabajo en R para an?lisis de datos
# y algunas tareas de machine learning

## Paso 1. Obtenci?n de datos y limpieza del dataset
# Cagar datos a una variable dataframe
Data <- read.csv("card_transdata.csv")
head(Data)
Variables <- data.frame(Variables = names(Data))
print(Variables)
# Dimensiones del conjunto de datos
cat("El n?mero de resgistros (filas) es de: ", 
    format(nrow(Data),big.mark = ",")) # Total de observaciones o registros (filas)
cat("\nEl n?mero de columnas (variables) es de: "
    , ncol(Data)) # Total de variables o atributos (columnas)
# Evaluaci?n NA's o missign values
library(VIM)
Nas <- aggr(Data)
summary(Nas)
# Detalles iniciales de cada variable
numericas <- c(1,2,3) # vector con los índices de las columnas númericas del dataset
i <- 1
while (i<=ncol(Data)){
  print(names(Data[i]))
  if (!(i %in% numericas)){
    Data[,i] <- as.factor(Data[,i])
  }
  print(class(Data[,i]))
  print(summary(Data[,i]))
  i = i + 1
}

## Paso 2. VISUALIZACIÓN DEL COMPORTAMIENTO DE LAS VARIABLES
library(ggplot2)
library(plotly)
library(cowplot)

# ANÁLISIS DE LOS VALORES QUE TOMAN LAS VARIABLES UNA A UNA 
# Generar tablas de frecuencias 
# Función para crear una tabla de frecuencias tipo factor
Freq_Factor <- function(Datos){
  Tabla <- data.frame(table(Datos))
  Tabla$'Freq_Rel'<- round(Tabla$Freq/sum(Tabla$Freq),3)*100
  return(Tabla)
}
# Construir tablas de frecuencias variables factor
Lista_Tablas_Freq<-list()
for (k in 4:8){
  Lista_Tablas_Freq[[k]] <- Freq_Factor(Data[k])
}
# Generar las gráficas de frecuencia para variables factor  
Graficas_Freq <- function(Tabla_Freq,x){
  Paletas = c("Accent","Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3") 
  g <- ggplot(Tabla_Freq,aes(x="", y=Freq_Rel, fill=fraud))+
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) + 
        ggtitle(paste(Variables[x],"\n","Si(1) - No(2)")) +
        ylab(element_blank()) +
        xlab(element_blank()) +
        geom_text(aes(label = paste(Freq_Rel,"%")),
                  position = position_stack(vjust = 0.5)) + 
        theme_void() +
        scale_fill_brewer(palette=Paletas[sample(1:length(Paletas),1)]) + 
        theme(legend.title = element_blank())
  return(g)
}







Graficas_Freq[[1]]









# Función para crear una tabla de frecuencias tipo numérica
Freq_Num <- function(Variable){
  # Calculo de parámetros básicos para la tabla de frecuencias
  Min <- trunc(min(Variable)) # Mínimo de los datos redondeado por abajo
  Max <- ceiling(max(Variable))# Máximo de los datos redondeado por arriba
  Rango <- Max - Min # El rango de variación de los datos
  # NumIntervalos <- ceiling(1+log2(Datos)) # Cantidad de intervalos, redondeado para obtener el numero entero
  NumIntervalos <- 4
  Ancho <- ceiling(Rango/NumIntervalos) # Amplitud - Ancho de cada intervalo
  # Columna con los límites inferiores y superiores de cada clase para el histograma
  Inferior <- NULL
  Superior <- NULL
  for (i in 1:(NumIntervalos)){
    Inferior <- rbind(Inferior,Min+Ancho*(i-1))
    Superior <- rbind(Superior,(Min+Ancho*i))
  }
  # Construcción de dataframe con los límites de las clases
  Tabla <- data.frame(Inferior,Superior) # Intervalos para tabla de frecuencias
  Freq <- NULL
  for (i in 1:nrow(Tabla)){
    Freq<- rbind(Freq,
                 sum(Variable>=Tabla[i,1] & Variable<Tabla[i,2])
    )
  }
  Tabla <- cbind(Tabla,Freq)
  Tabla$Freq_Rel <- round(Tabla$Freq/length(Variable),6)*100
  Tabla$Freq_Acum <- cumsum(Tabla$Freq_Rel)
  return(Tabla)
}
# Construir tabla de frecuencias variables numéricas
for (k in 1:3){
  Tablas_Factor[[k]] <- Freq_Num(Data[k])
}









# ANÁLISIS DE LOS VALORES QUE TOMAN LAS VARIABLES FRENTE A LA VARIABLE DE RESPUESTA 

# Función para gráficos de barras de las variables tipo factor en relación a la variable de respuesta
Grafico_Barras <- function(Datos){
  # Dar formato a los números que aparecen en la escala y de las gráficas
  marks_no_sci <- function(x) format(x, 
                                     big.mark = ".", 
                                     decimal.mark = ",", 
                                     scientific = FALSE)
  # Definición de paletas de colores
  Paletas = c("Accent","Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
  # Generación del gráfico
  g<- ggplot(Data, aes(x=Datos, fill=fraud)) + 
        geom_bar(position="dodge", stat="count") +
        scale_y_continuous(labels = marks_no_sci) +
        xlab("") +
        ylab("Cantidad de registros") +
        scale_fill_brewer(palette=Paletas[sample(1:length(Paletas),1)]) + 
        labs(fill = "Fraude")
  return(g)
}
# Generar gráficas de las variables tipo factor en relación a la variable de respuesta
Plots_Barras<-list()
l <- 4
while (l <= 7){
  Grafica <- Grafico_Barras(Data[,l])  
  Grafica <- Grafica + ggtitle(paste(names(Data[l]),"\n","Si(1) - No(2)"))
  Plots_Barras[[l]] <- Grafica
  l = l + 1
}
plot_grid(Plots_Barras[[4]],
          Plots_Barras[[5]],
          Plots_Barras[[6]],
          Plots_Barras[[7]], 
          labels = "AUTO")




 










names(Data)
library(ggplot2)
Caja_dfh <- ggplot(Data, aes(x = fraud, y=distance_from_home)) +
        geom_boxplot()
Caja_dfh  

Caja_dlt <- ggplot(Data, aes(x = fraud, y=distance_from_last_transaction)) +
  geom_boxplot()
Caja_dlt

Caja_rmp <- ggplot(Data, aes(x = fraud, y=ratio_to_median_purchase_price)) +
  geom_boxplot()
Caja_rmp


names(Data)
 
d_caja<-boxplot(Data$distance_from_home[Data$fraud==0])
sum(Data$distance_from_home %in% d_caja$out) 

length(d_caja$out)
  
sum(Data$fraud==0)  
sum(Data$fraud==1)

Data_Out <- Data[!(Data$Valor_neto %in% d_Caja$out),]





  geom_boxplot(alpha=0.7) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="red", fill="red") +
        theme(legend.position="none") +
        scale_fill_brewer(palette="Set1")























Variable<-Data$distance_from_home


 


























