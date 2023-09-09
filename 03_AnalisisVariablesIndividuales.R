## Paso 2. VISUALIZACIÓN DEL COMPORTAMIENTO DE LAS VARIABLES
library(ggplot2)
library(cowplot)

# ANÁLISIS DE LOS VALORES QUE TOMAN LAS VARIABLES UNA A UNA

# Función para graficar de cajas para variables numéricas
Plot_Caja_Num<-function(Datos,k){
  g <- ggplot(Datos, aes(x = "", y=Data[,k])) +
    geom_boxplot(outlier.colour="red") + 
    ggtitle(paste(Variables[k,1])) +
    ylab(element_blank()) +
    xlab(element_blank()) +
    theme_minimal()
  return(g)
}
# Análisis de la dispersión de las variables numéricas

# Guardar las gráficas de columna de las variables numéricas
Lista_BoxPlot <- list()
for (l in 1:3){
  Grafica <- Plot_Caja_Num(Data,l)
  Lista_BoxPlot[[l]] <- Grafica
}
# Generar las 3 gráficas en una sola imagen
plot_grid(Lista_BoxPlot[[1]],
          Lista_BoxPlot[[2]],
          Lista_BoxPlot[[3]]
)

# Análisis de Frecuencias

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
  Intervalo <-NULL
  for (i in 1:(NumIntervalos)){
    Inferior <- rbind(Inferior,Min+Ancho*(i-1))
    Superior <- rbind(Superior,Min+Ancho*i)
    Intervalo <- rbind(Intervalo,paste(Min+Ancho*(i-1)," - ",Min+Ancho*i))
  }
  # Construcción de dataframe con los límites de las clases
  Intervalo = as.factor(Intervalo)
  Tabla <- data.frame(Inferior,Superior, Intervalo) # Intervalos para tabla de frecuencias
  Freq <- NULL
  # Calculo de frecuencia por clases definidas
  for (i in 1:nrow(Tabla)){
    Freq<- rbind(Freq,
                 sum(Variable>=Tabla[i,1] & Variable<Tabla[i,2])
    )
  }
  # Unir los datos en un solo dataframe
  Tabla <- cbind(Tabla,Freq)
  Tabla$Freq_Rel <- round(Tabla$Freq/sum(Freq),6)*100
  Tabla$Freq_Acum <- cumsum(Tabla$Freq_Rel)
  return(Tabla)
}
# Construir tabla de frecuencias variables numéricas
Lista_Tablas_Freq<-list()
for (k in 1:3){
  Lista_Tablas_Freq[[k]] <- Freq_Num(Data[k])
}

# Función para crear gráfica de columnas para las frecuencias de variables
# numéricas
Plot_Bar_Freq <- function(Datos,l){
  Paletas = c("Accent","Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
  g <- ggplot(Datos, 
              aes(x = Intervalo,
                  y = Freq_Rel,
                  fill = Intervalo))
  g + geom_bar(stat = "identity") + 
    ggtitle(paste(Variables[l,1])) +
    # se ocultan los nombres de los ejes (porque con la opción polar pierden sentido)
    ylab("% del Total de Observaciones") +
    xlab("Rango de datos") +
    labs(fill = "Rango de Datos") + 
    scale_fill_brewer(palette=Paletas[sample(1:length(Paletas),1)]) +
    geom_text(aes(label = paste(Freq_Rel,"%")),
              position = position_stack(vjust = 0.5))
}
Lista_PlotBar_Freq<-list()
for (l in 1:3){
  Grafica <- Plot_Bar_Freq(Lista_Tablas_Freq[[l]],l)
  Lista_PlotBar_Freq[[l]] <- Grafica
}
plot_grid(Lista_PlotBar_Freq[[1]],
          Lista_PlotBar_Freq[[2]],
          Lista_PlotBar_Freq[[3]]
)

# Función para crear una tabla de frecuencias tipo factor
Freq_Factor <- function(Datos){
  Tabla <- data.frame(table(Datos))
  Tabla$'Freq_Rel'<- round(Tabla$Freq/sum(Tabla$Freq),3)*100
  return(Tabla)
}
# Construir tablas de frecuencias variables factor

for (k in 4:8){
  Lista_Tablas_Freq[[k]] <- Freq_Factor(Data[k])
}
# Generar las gráficas de frecuencia para variables factor  
Plot_Pie_Freq <- function(Tabla_Freq,v){
  # Vector con las paletas de colores disponibles en ggplot2
  Paletas = c("Accent","Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3") 
  g <- ggplot(Lista_Tablas_Freq[[v]],aes(x="", y=Freq_Rel, fill=Lista_Tablas_Freq[[v]][,1])) +
    # La anterior línea define los datos a graficar, en fill se le entregan los niveles de factores
    geom_bar(stat="identity", width=1, color="white") +
    # En ggplot no existe grafico circular, sobre la gráfica de barras se ajusta a polar
    coord_polar("y", start=0) + 
    # Título de la gráfica
    ggtitle(paste(Variables[v,1],"\n","Si(1) - No(2)")) +
    # se ocultan los nombres de los ejes (porque con la opción polar pierden sentido)
    ylab(element_blank()) +
    xlab(element_blank()) +
    # Se incluyen las etiquetas de datos
    geom_text(aes(label = paste(Freq_Rel,"%")),
              position = position_stack(vjust = 0.5)) +
    # Se elimina el contorno de la gráfica (para descargarla visualmente)
    theme_void() +
    # Se cambia el color de las gráficas, se asigna color de manera aleatoria
    # tomandolo de la paleta de colores
    scale_fill_brewer(palette=Paletas[sample(1:length(Paletas),1)]) +
    # Se oculta el título de las etiquetas
    theme(legend.title = element_blank())
  return(g)
}
# Lista vacía para guardar las gráficas variables factor
Lista_Plot_Pie_Freq<-list()
# Generar las gráficas factor y guardarlas en la lista
l <- 4
while (l <= 8){
  Grafica <- Plot_Pie_Freq(Lista_Tablas_Freq[[l]],l)  
  #Grafica <- Grafica + ggtitle(paste(names(Data[l]),"\n","Si(1) - No(2)"))
  Lista_Plot_Pie_Freq[[l]] <- Grafica
  l = l + 1
}
# Dibujar las gráficas de factor en una sola imagen
plot_grid(Lista_Plot_Pie_Freq[[4]],
          Lista_Plot_Pie_Freq[[5]],
          Lista_Plot_Pie_Freq[[6]],
          Lista_Plot_Pie_Freq[[7]],
          Lista_Plot_Pie_Freq[[8]]
)

# Número total de registros de fraude
TotalFraudes = sum(Data$fraud==1)
TotalFraudes