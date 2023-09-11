# Evaluación NA's o missign values
library(VIM)
Nas <- aggr(Data)
summary(Nas)