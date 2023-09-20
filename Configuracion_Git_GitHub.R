'
Configurar Git
'
library(usethis)
use_git()
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

