#' @include Genericos.R 03-Extract.R
NULL

# Extract RDS -----------------------
#' Extrae un objeto de R de una conexión
#'
#' Extrae un objeto R de una conexión \link{ConexionRDS}.
#'
#' @section Condiciones y Errores:
#'
#' Devuelve error si el objeto no se encuentra en la ruta provista.
#'
#'
#' @describeIn Extraer Extrar un objeto R en en una conexión \linkS4class{ConexionRDS}.
#'
#' @examples
#' \dontrun{
#'   objecto <- Extraer(miConexioRDS)
#' }
#'
#' @importFrom fs file_exists
#'
#' @export
setMethod(
  "Extraer",
  signature = c(conexion = "ConexionRDS"),
  definition =
    function(conexion)
    {
      if (!fs::file_exists(conexion@connString))
        stop("Objeto RDS no encontrado.")
      x <- readRDS(conexion@connString)
      return(x)
    }
)
