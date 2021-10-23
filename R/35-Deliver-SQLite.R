#' @include Genericos.R 03-Extract.R
NULL

# Guardar tibble en tabla ----------------------
#' Guardar un tibble en SQLite
#'
#' Guarda un \code{tibble} o \code{data.frame} en una conexión \link{ConexionSQLite}.
#'
#' @importFrom DBI dbConnect dbCreateTable dbAppendTable dbReadTable dbDisconnect dbIsValid
#'
#' @importFrom dplyr tibble
#'
#' @section Condiciones y Errores:
#'
#' Si el objeto es \link{data.frame} y la conexión un \linkS4class{ConexionSQLite}, entonces arroja error si la conexión a la base de datos falla (usa \code{DBI::dbConnect} internamente).
#'
#'
#' @describeIn Guardar Guarda \link{data.frame} en en una conexión \linkS4class{ConexionSQLite}.
#'
#' @examples
#' \dontrun{
#'   conexion <- ConexionSQLiteTabla(RutaDB = "unaBaseDeDatos.sqlite"
#'     , NombreTabla = "unNombreDeTabla")
#'   fueGuardado <- Guardar(unDataFrame_Tibble, conexion)
#'   stopifnot(fueGuardado)
#' }
#'
#' @export
setMethod(
  "Guardar"
  ,
  signature = c(x = "data.frame", conexion = "ConexionSQLite")
  ,
  definition =
    function(x, conexion)
    {
      valor <- FALSE
      con <-
        DBI::dbConnect(drv = conexion@Driver, conexion@connString)
      # revisar si conexion está abierta
      if (!DBI::dbIsValid(con))
        stop("No se pudo conectar a la base de datos")

      DBI::dbBegin(con)
      # Guardar Registro
      nRegistros <-
        tryCatch({
          registros <- DBI::dbAppendTable(con
                                          , name = conexion@Tabla
                                          , value = x)

          DBI::dbCommit(con)
          registros
        },
        error = function(cnd)
        {
          DBI::dbRollback(con)
        },
        #desconectar
        finally = DBI::dbDisconnect(con)
        )# tryCatch

      if (nRegistros > 0)
        valor <- TRUE
      return(valor)
    }
)


