'
Configurar Git
'
library(usethis)
create_github_token()
use_git()

'
Inicializar un repositorio de Git
'
install.packages("credentials")
library(credentials)
set_github_pat()

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
usethis::use_github()