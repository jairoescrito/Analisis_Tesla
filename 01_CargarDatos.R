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

# Creación de variables tipo factor
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