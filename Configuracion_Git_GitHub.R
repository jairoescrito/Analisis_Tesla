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
