# Ejercicio de muestra de trabajo en R para an�lisis de datos
# y algunas tareas de machine learning

## Paso 1. Obtenci�n de datos y limpieza del dataset
# Cagar datos a una variable dataframe
Data <- read.csv("card_transdata.csv")
Variables <- data.frame(Variables = names(Data))
print(Variables)
# Dimensiones del conjunto de datos
cat("El n�mero de resgistros (filas) es de: ", 
    format(nrow(Data),big.mark = ",")) # Total de observaciones o registros (filas)
cat("\nEl n�mero de columnas (variables) es de: "
    , ncol(Data)) # Total de variables o atributos (columnas)






