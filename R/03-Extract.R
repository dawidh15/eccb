#' @include Genericos.R
NULL

# Conexion-class --------------------
#' Clase Conexión
#'
#' Un objeto S4 para gestionar conexiones a archivos de datos o bases de datos.
#'
#' Pertenece al \bold{Sub-sistema 3 de extracción.}
#'
#' Maneja información específica de la conexión, que luego permite a interfaces comunes, conectarse con el archivo apropiado para extraer o almacenar datos.
#'
#' Actualmente, soporta conexiones a RDS, SQLite. Para construir la conexión usar la función \code{Conexion<ARCHIVO>}. Por ejemplo: \code{ConexionCSV(...)}, \code{ConexionSQLite(...)}.
#'
#' @slot connString Una cadena de texto con las instrucciones para realizar la conexión.
#'
#' @name Conexion-class
#' @rdname Conexion-class
#'
#' @export
setClass("Conexion",
         slots = list(connString = "character"))

# Conexion RDS -----------------
#' Clase Conexión RDS
#'
#' Un objeto S4 para gestionar conexiones objetos de datos generados por R.
#'
#' Pertenece al \bold{Sub-sistema 3 de extracción.}
#'
#' @docType class
#' @name ConexionRDS-class
#' @rdname ConexionRDS-class
#'
#' @export
setClass("ConexionRDS", contains = "Conexion")

# ConexionRDS ----------------------
#' ConexionRDS
#'
#' Crea una conexión a un archivo de datos en formato R.
#'
#' @param rutaArchivo La ruta completa hacia el archivo \code{RDS}.
#'
#' @importFrom stringr str_replace str_to_lower
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'    ConexionRDS("C:/Myfile.rds") # Añadir extensión, o arroja error
#' }
ConexionRDS <- function(rutaArchivo) {
  #TODO enviar todo esto a setValidity
  extension <- fs::path_ext(rutaArchivo)
  extension <- stringr::str_to_lower(extension)
  extOK <- switch (extension,
                             rds = TRUE,
                             stop("Extensión inválida")
  )
  if (extOK){
    valor <- new("ConexionRDS",
                 connString = rutaArchivo
    )
  }
}


# Conexion SQLite ---------------------
#' Clase Conexión SQLite
#'
#' Un objeto S4 para gestionar conexiones a una base de datos en SQLite, a través del paquete \link[=package]{RSQLite}.
#'
#' @slot Driver El driver para la conexión con SQLite.
#' Al usar \link{ConexionSQLite} para crear la conexión, el driver se asigna automáticamente.
#' @slot Tabla El nombre de la tabla donde se desea guardar los datos.
#'
#' @importClassesFrom RSQLite SQLiteDriver
#'
#' @name ConexionSQLite-Class
#'
#' @rdname ConexionSQLite-Class
#'
#' @export
setClass("ConexionSQLite",
         slots = list(Driver = "SQLiteDriver", Tabla = "character")
         , contains = "Conexion")

setValidity(Class = "ConexionSQLite",
            method =
              function(object)
              {
                valid <- FALSE
                msg <- ""
                if (
                  (object@connString == ":memory:") &&
                  is(object@Driver, "SQLiteDriver") )
                  return(TRUE)

                fileExt <- fs::path_ext(object@connString)
                switch (fileExt,
                        db = assign("valid", TRUE),
                        sqlite = assign("valid", TRUE),
                        sqlite3 = assign("valid", TRUE),
                        msg <- paste(msg,"Extensión de base de datos debe ser 'db', 'sqlite' o 'sqlite3'")
                )

                con <- DBI::dbConnect(object@Driver, object@connString)
                validCon <- DBI::dbIsValid(con)
                tryCatch(
                  {
                    if (validCon)
                    {
                      tablaExiste <-
                        DBI::dbExistsTable(conn = con, name = object@Tabla)
                      if (tablaExiste)
                      {
                        assign("valid", TRUE, pos = -1)
                      } else {
                        assign("valid", FALSE, pos = -1)
                        problema <- paste(msg,"Tabla no encontrada. ")
                        assign("msg", problema, pos = -1)

                      }
                    } else {
                      assign("valid", FALSE, pos = -1)
                      problema <- paste(msg,"Conexión a SQLite inválida. ")
                      assign("msg", problema, pos = -1)

                    }
                  },
                  finally = DBI::dbDisconnect(con)
                )

                if (valid) TRUE else msg
              })

# ConexionSQLite ctor-----------------------
#' ConexionSQLite
#'
#' Crea una conexión a un archivo de SQLite.
#'
#' @param RutaDB La ruta al archivo con la base de datos.
#' @param Tabla El nombre de la tabla en la base de datos.
#'
#' @importFrom stringr str_replace str_to_lower
#'
#' @importFrom RSQLite SQLite
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'    ConexionSQLite("C:/myDB.sqlite") # Añadir extensión, o arroja error
#'     ConexionSQLite("C:/myDB.db") # Cambia extension
#' }
ConexionSQLite <- function(RutaDB, Tabla) {
  valor <- new("ConexionSQLite",
               connString = RutaDB
               , Driver = RSQLite::SQLite()
               , Tabla = Tabla
  )
}
