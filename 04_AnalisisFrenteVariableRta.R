# 4. ANÁLISIS DE LOS VALORES QUE TOMAN LAS VARIABLES FRENTE A LA VARIABLE DE RESPUESTA 
library(ggplot2)
library(cowplot)
library(tidyverse)

# Relación de las variables numéricas frente a las variables de respuesta
Data_Aux <- Data[,c(1,2,3,8)] # Crear un dataframe auxiliar para crear factores de las variables numéricas
nombres = names(Data_Aux)
for (i in 1:3){
  Data_Aux <- Data_Aux %>% # Usando tidyverse - dplyr se crean variables de asignación de 
    # cada dato al rango correspondiente. Esto se hace porque en la tabla de frecuencias no
    # se especifican los conteos segun el valor de la respuesta "fraud"
    mutate(Rango = case_when( # Se verifica cada dato en que rango de frecuencia está
      Data_Aux[i] < Lista_Tablas_Freq[[i]][1,2] ~ Lista_Tablas_Freq[[i]][1,3],
      Data_Aux[i] < Lista_Tablas_Freq[[i]][2,2] ~ Lista_Tablas_Freq[[i]][2,3],
      Data_Aux[i] < Lista_Tablas_Freq[[i]][3,2] ~ Lista_Tablas_Freq[[i]][3,3],
      Data_Aux[i] < Lista_Tablas_Freq[[i]][4,2] ~ Lista_Tablas_Freq[[i]][4,3],
    )
    )
  nombres = c(nombres, paste(nombres[i],"_Rango"))
  names(Data_Aux) = nombres
}
Data_Aux <- Data_Aux[,c(5,6,7,4)] # Reordenar la data auxiliar
nombres <- names(Data_Aux)
# construir función de gráfico de barras variables numéricas frente a fraude
Plot_Bar_Num <- function(Data,l){ # Data para ingresar el Data_Aux y l el número de la variable en Data_Aux
  marks_no_sci <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  Paletas = c("Accent","Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
  g <- ggplot(Data_Aux, aes(x=Data[,l], fill=fraud)) + 
          geom_bar(position="dodge", stat="count") +
          scale_y_continuous(labels = marks_no_sci) +
          xlab("Rangos") + ylab("Cantidad de registros") +
          scale_fill_brewer(palette=Paletas[sample(1:length(Paletas),1)]) + 
          labs(fill = "Fraude") + 
          ggtitle(paste(Variables[l,1],"\n","Si(1) - No(2)")) +
          geom_text(stat='count', aes(x = Data_Aux[,l],label = ..count..), 
                    position = position_dodge(width = 1),
                    vjust = -0.2, hjust= 0.4, size = 3)
  return(g)
}
# Crear la imagen de gráficas en una sola vista
plot_grid(Plot_Bar_Num(Data_Aux,1),
          Plot_Bar_Num(Data_Aux,2),
          Plot_Bar_Num(Data_Aux,3)
          )

# Relación de las variables factor frente a la variable de respuesta
# Función para gráficos de barras de las variables tipo factor en relación a la variable de respuesta
Plot_Bar <- function(Datos,l){ # Datos para ingresar el vector de datos y l el numero de variable en Data
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
    ggtitle(paste(Variables[l,1],"\n","Si(1) - No(2)")) +
    geom_text(stat='count', aes(x = Datos,label = ..count..), 
              position = position_dodge(width = 1),
              vjust = -0.2, hjust= 0.4, size = 3)
  return(g)
}
# Generar gráficas de las variables tipo factor en relación a la variable de respuesta
plot_grid(Plot_Bar(Data[,4],4),
          Plot_Bar(Data[,5],5),
          )

plot_grid(Plot_Bar(Data[,6],6),
          Plot_Bar(Data[,7],7) 
)