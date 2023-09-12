'
Configurar Git
'
library(usethis)
create_github_token()
use_git()

getwd()
setwd("/home/jairoescrito/OneDrive/Documentos/TeslaTeach/Proyectos/Analisis_Tesla")

'
Inicializar un repositorio de Git
'
install.packages("credentials")
library(credentials)
set_github_pat()
"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"
'
Modificar credenciales
'
install.packages("gitcreds")
library(gitcreds)
gitcreds_set()

'
Conectar con repositorio
'
use_git()
use_github()
usethis::use_github()

install.packages("reticulate")

