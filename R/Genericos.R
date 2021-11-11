# -------Guardar-------
#' Guardar
#'
#' Guarda un objeto, usando una conexión.
#'
#' Esta función requiere del objeto y la conexión, sin ningún otro detalle adicional.
#'
#' Esta función puede ser usada en componentes como Fact Table Builders, o Fact Table Providers. También como \emph{standalone}.
#'
#' @docType methods
#' @name Guardar
#' @rdname Guardar
#'
#' @return Verdadero si el objeto fue guardado. Falso, de lo contrario.
#'
#' @param x El objeto a guardar.
#' @param conexion Una conexión al archivo o base de datos donde se desea guardar el objeto.
#'
#' @export
setGeneric("Guardar", valueClass = "logical",function(x, conexion,...)standardGeneric("Guardar"))

# Extraer -----------------------
#' Extraer
#'
#' Extrae un objeto, usando una conexión.
#'
#' Esta función requiere de la conexión, sin ningún otro detalle adicional.
#'
#'
#' @docType methods
#' @name Extraer-methods
#' @rdname Extraer-methods
#'
#' @return El tipo de objeto depende de la conexión.
#'
#' @param conexion Una conexión al archivo o base de datos donde se desea guardar el objeto.
#'
#' @export
setGeneric("Extraer",function(conexion,...)standardGeneric("Extraer"))
