
eccdConfig <- new.env(parent = emptyenv())

# Cargar al incio de la sesión

.onLoad <- function(libname, pkgname){
  eccdConfig$FilaStatus <- c("Nueva", "Eliminada","Cambiada","Sin cambio")
}
