
#' @describeIn Extraer-methods Extrae un data.frame de una conexión a SQLite.
#' @importFrom DBI dbConnect dbDisconnect dbReadTable
#' @export
setMethod("Extraer",
    signature = c(conexion = "ConexionSQLite"),
    definition =
      function(conexion)
      {
        con <- DBI::dbConnect( conexion@Driver, conexion@connString)
        x <- tryCatch(
          {
        value <- DBI::dbReadTable(con, name = conexion@Tabla)
        value
        },
        finally = DBI::dbDisconnect(con)
        )
        return(x)
      }
)
