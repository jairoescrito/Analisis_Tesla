# ANÁLISIS DE LOS VALORES QUE TOMAN LAS VARIABLES FRENTE A LA VARIABLE DE RESPUESTA 
library(ggplot2)
library(cowplot)
library(tidyverse)
# Relación de las variables numéricas frente a las variables de respuesta
Data_Aux <- Data[,c(1,2,3,8)] # Crear un dataframe auxiliar para crear factores de las variables numéricas
nombres = names(Data_Aux)
for (i in 1:3){
  Data_Aux <- Data_Aux %>%
    mutate(Rango = case_when(
      Data_Aux[i] < Lista_Tablas_Freq[[i]][1,2] ~ Lista_Tablas_Freq[[i]][1,3],
      Data_Aux[i] < Lista_Tablas_Freq[[i]][2,2] ~ Lista_Tablas_Freq[[i]][2,3],
      Data_Aux[i] < Lista_Tablas_Freq[[i]][3,2] ~ Lista_Tablas_Freq[[i]][3,3],
      Data_Aux[i] < Lista_Tablas_Freq[[i]][4,2] ~ Lista_Tablas_Freq[[i]][4,3],
    )
    )
  nombres = c(nombres, paste(nombres[i],"_Rango"))
  names(Data_Aux) = nombres
}
Data_Aux <- Data_Aux[,c(5,6,7,4)]
nombres <- names(Data_Aux)


# construir funcion
marks_no_sci <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
Paletas = c("Accent","Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
ggplot(Data_Aux, aes(x=Data_Aux[,1], fill=fraud)) + 
  geom_bar(position="dodge", stat="count") +
  scale_y_continuous(labels = marks_no_sci) +
  xlab("Rangos") + ylab("Cantidad de registros") +
  scale_fill_brewer(palette=Paletas[sample(1:length(Paletas),1)]) + 
  labs(fill = "Fraude") + 
  ggtitle(paste(Variables[1,1],"\n","Si(1) - No(2)")) +
  geom_text(stat='count', aes(x = Data_Aux[,1],label = ..count..), 
            position = position_dodge(width = 1),
            vjust = -0.2, hjust= 0.4, size = 3)

# incluirle las etiquetas

length(Data_Aux[,1][Data_Aux[,4]==1&Data_Aux[,1]=="0  -  2659"])


clase <- as.factor(Data_Aux[,1])
class(Data_Aux[4])



# Relación de las variables factor frente a la variable de respuesta
# Función para gráficos de barras de las variables tipo factor en relación a la variable de respuesta
Plot_Bar <- function(Datos,l){
  # Dar formato a los números que aparecen en la escala y de las gráficas
  marks_no_sci <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  # Definición de paletas de colores
  Paletas = c("Accent","Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
  # Generación del gráfico
  g<- ggplot(Data, aes(x=Datos, fill=fraud)) + 
    geom_bar(position="dodge", stat="count") +
    scale_y_continuous(labels = marks_no_sci) +
    xlab("") + ylab("Cantidad de registros") +
    scale_fill_brewer(palette=Paletas[sample(1:length(Paletas),1)]) + 
    labs(fill = "Fraude") + 
    ggtitle(paste(Variables[l,1],"\n","Si(1) - No(2)"))
  return(g)
}
# Generar gráficas de las variables tipo factor en relación a la variable de respuesta
plot_grid(Plot_Bar(Data[,4],4),
          Plot_Bar(Data[,5],5),
          Plot_Bar(Data[,6],6),
          Plot_Bar(Data[,7],7), 
          labels = "AUTO")