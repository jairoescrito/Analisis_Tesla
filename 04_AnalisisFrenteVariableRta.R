




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