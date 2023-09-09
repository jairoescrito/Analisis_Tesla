# Evaluación NA's o missign values
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