# Columnas-class --------------------
#' Clase Columnas
#'
#' Un objeto S4 para modelar una columna.
#'
#' No pertenece a un sub-sistema directamente. Es una clase de apoyo.
#'
#' Contiene los nombres y los tipos de datos de una tabla.
#'
#' @slot Nombres un vector de texto con el nombre de las columnas.
#' @slot Tipos un vector de texto con los tipos de datos de las columnas.
#'
#' @name Columnas-class
#' @rdname Columnas-class
#'
#' @export
setClass("Columnas",
         slots = list(Nombres = "character", Tipos = "character"))

setValidity("Columnas", method =
              function(object){
                msg <- ""
                valido <- TRUE

                if (length(object@Nombres) != length(object@Tipos))
                {
                  msg <- paste0(msg,"La longitud de 'Nombres' no es igual a la longitud de 'Tipos'.")
                  valido <- FALSE
                }

                if (valido) TRUE else msg
              }
)


#' @describeIn Columnas-class Constructor de columnas.
#' @export
Columnas <-
  function(Nombres, Tipos)
  {
    valor <-
      new("Columnas", Nombres = Nombres, Tipos = Tipos)

    return(valor)
  }

# Fila-class --------------------
#' Clase Fila
#'
#' Un objeto S4 para modelar una Fila
#'
#'
#' Contiene los nombres y los tipos de datos de la fila de una una tabla. Además su principal función es dar seguimiento al estado de la fila; es decir, si contiene errores, está siendo actualizada, es nueva, etc...
#'
#' @section Uso previsto:
#'
#' \code{Fila} es un objeto auxiliar y no contiene datos en sí mismo. La función \link{TraerFilas(data, Filas)} se encarga de filtrar un \code{data.frame} extrayendo las filas correctas; mientras que, \code{TraerErrores(data, Filas)} extrae las filas erróneas en su propio \code{data.frame}.
#'
#' La función de este objeto, es llevar el estado de una fila en procesos de verificación, y procesamiento de registros.
#'
#' @slot Columnas un objecto con las \linkS4class{Columnas} de una tabla.
#' @slot Error indica si la fila contiene un error en algún valor de sus columnas.
#' @slot ErroresMsg un vector de texto, con un elemento por columna en error.
#' @slot ErroresIndice un vector de enteros con la(s) posición(es) de la(s) columna(s) en error.
#' @slot Status el status de la columna. Una lista enumerada ('Nueva', 'Eliminada','Cambiada','Sin cambio')
#'
#' @docType class
#' @name Fila-class
#' @rdname Fila-class
#'
#' @export
setClass("Fila", representation = list(
  Columnas = "Columnas"
  , Error = "logical"
  , ErroresMsg = "character"
  , ErroresIndice = "numeric"
  , Status = "character"
  , FueValidada = "logical"
))

setValidity("Fila"
  ,method =
    function(object)
      {
        msg <- ""
        valido <- TRUE

        if (length(object@Error) > 1){
          msg <- paste0(msg, "La fila está en error o no lo está. No se permiten vectores lógicos. ")
          valido <- FALSE
        }

        if (!(object@Status %in% eccdConfig$FilaStatus))
        {
          msg <-
            paste0(msg, "Fila con Status no permitido. ")
          valido <- FALSE
        }

        if (valido) TRUE else msg
    })



setMethod("initialize", signature = "Fila", definition =
  function(.Object, ...)
    {

      .Object@Status <- eccdConfig$FilaStatus[1] # Nueva
      .Object@Error <- FALSE
      .Object@ErroresMsg <- character(0)
      .Object@ErroresIndice <- numeric(0)
      .Object@FueValidada <- FALSE
      .Object <- callNextMethod()
      .Object
  })


#' @describeIn Fila-class Constructor de fila
#' @export
Fila <- function(Columnas)
{
  if (!is(Columnas, "Columnas"))
    stop("Argumento Columnas debe ser de tipo 'Columnas'")
  valor <- new("Fila", Columnas = Columnas)
  return(valor)
}


#  Setters Fila ----------------
#' Configura el valor de los elementos de \linkS4class{Fila}.
#'
#' Estas funciones pueden usarse para que usuarios del paquete puedan crear el método de \link{ValidarFila} más fácilmente. Ver ejemplo en \link{ValidarFIla}.
#'
#' @docType methods
#' @name accederFilas
#' @rdname accederFilas
NULL

setGeneric("filaError", function(x)
  standardGeneric("filaError"))

setGeneric("filaError<-", function(x, value)
  standardGeneric("filaError<-"))

#' @describeIn accederFilas Accede un mensaje de error en una fila
#' @export
setMethod("filaError", "Fila", function(x)x@Error)

#' @describeIn accederFilas Guarda un mensaje de error en una fila
#' @export
setMethod("filaError<-", "Fila"
          , function(x, value) {
            x@Error <- value
            return(x)
            })


setGeneric("filaColumnas"
           , function(x)standardGeneric("filaColumnas"))

setGeneric("filaColumnas<-"
           , function(x, value)standardGeneric("filaColumnas<-"))

#' @describeIn accederFilas Accede las columnas una fila
#' @export
setMethod("filaColumnas", "Fila", function(x)x@Columnas)

#' @describeIn accederFilas Guarda las columnas una fila
#' @export
setMethod("filaColumnas<-", "Fila", function(x, value) {
  x@Columnas <- value
  x
})

setGeneric("filaColNombre", function(x)standardGeneric("filaColNombre"))
#' @describeIn accederFilas Accede el nombre de los campos en una fila.
#' @export
setMethod("filaColNombre", "Fila", function(x)x@Columnas@Nombres)

setGeneric("filaColTipo", function(x)standardGeneric("filaColTipo"))
#' @describeIn accederFilas Accede el tipo de dato de los campos en una fila.
#' @export
setMethod("filaColTipo", "Fila", function(x)x@Columnas@Tipos)

setGeneric("filaMensajeError"
           , function(x)standardGeneric("filaMensajeError"))
setGeneric("filaMensajeError<-"
           , function(x, value)standardGeneric("filaMensajeError<-"))

#' @describeIn accederFilas Accede un mensaje de error
#' @export
setMethod("filaMensajeError", "Fila", function(x) x@ErroresMsg)

#' @describeIn accederFilas \bold{Anexa} un mensaje de error
#' @export
setMethod("filaMensajeError<-", "Fila", function(x, value)
  {
  x@ErroresMsg <- append(x@ErroresMsg,value)
  x
  })


setGeneric("filaColumnaEnError"
           , function(x)standardGeneric("filaColumnaEnError"))
setGeneric("filaColumnaEnError<-"
           , function(x, value)standardGeneric("filaColumnaEnError<-"))

#' @describeIn accederFilas Accede la posición de la columna en error
#' @export
setMethod("filaColumnaEnError", "Fila", function(x) x@ErroresIndice)

#' @describeIn accederFilas \bold{Anexa} la posición de la columna en error
#' @export
setMethod("filaColumnaEnError<-", "Fila", function(x, value) {
  x@ErroresIndice <- append(x@ErroresIndice,value)
  x
  })


setGeneric("filaStatus"
           , function(x)standardGeneric("filaStatus"))
setGeneric("filaStatus<-"
           , function(x,value)standardGeneric("filaStatus<-"))

#' @describeIn accederFilas Accede el estatus de la fila
#' @export
setMethod("filaStatus", "Fila", function(x) x@Status)

#' @describeIn accederFilas Guarda el estatus de la fila
#' @export
setMethod("filaStatus<-","Fila", function(x, value)
  {
  x@Status <- value
  x
  })


# ValidarFila -----
#' Validación de Fila (Genérico).
#'
#' Este genérico debe ser reemplazado con métodos que utilicen la clase \linkS4class{Fila}, o de objetos que hereden directamente de la clase.
#'
#' El autor del ETL generará un método con todas las validaciones necesarias para la fila de un conjunto de datos. Deberá devolver un objeto \linkS4class{Fila} o descendiente, con los campos completos. Especialmente, señalando si la fila se encuentra en error, el índice de los campos que se encuentran en error, y los mensajes de error.
#'
#' Esta función se llama dentro de \link{AgregarFila}; la cual primero verifica si existe un método apropiado para la \linkS4class{Fila} o descendiente, antes de agregar la fila a la colección de \linkS4class{Filas}. De lo contrario, carga la fila con el campo \code{FueValidada = FALSE}.
#'
#' @section Ubicación del método:
#'
#' Lo más apropiado es definir el método en .GlobalEnv.
#'
#' @return \linkS4class{Fila} o descendiente, con validación si el método existe, o la fila tal cual ingresa en el argumento si el método no existe.
#'
#'
#' @param Fila Un objeto \linkS4class{Fila} o descendiente
#' @param Registro es una fila del data frame. Las columnas deben tener los mismos nombres que el slot del objeto \code{Fila@@Columnas}.
#'
#' @examples
#' #' cols <- Columnas(c("ID", "nombre", "peso"), c("integer", "character", "numeric"))
#'
#' fila <- Fila(cols)
#'
#' data <-
#'   data.frame(altura = c(1.3, 2.1, 1, 1.81),
#'              Nombre = c("Frodo", "Sauron", "Golum", "Gandalf"))
#'
#' setMethod(
#'   "ValidarFila",
#'   signature = c(Fila = "Fila", Registro = "data.frame"),
#'   definition =
#'     function(Fila, Registro)
#'     {
#'       if ("altura" %in% filaColNombre(Fila))
#'       {
#'         if (Registro$altura < 1.5)
#'         {
#'           filaError(Fila) <- TRUE
#'           filaMensajeError(Fila) <- "Altura inapropiada"
#'           filaColumnaEnError(Fila) <-
#'             purrr::detect_index(colnames(Registro), function(x)
#'               x == "altura")
#'         }
#'       }
#'       return(Fila)
#'     }
#'   , where = .GlobalEnv
#' )
#'
#' fila <-
#'   Fila( Columnas(Nombres = colnames(data), c("numeric", "character")))
#'
#' filas <-
#'   ValidarFilas(Fila = fila, Datos = data)
#'
#'
#' filasCorrectas(filas)
#' filasEnError(filas)
#' filasErrores(filas)
#' filasErroresPos(filas)
#' filasStatus(filas)
#' @export
setGeneric("ValidarFila", valueClass = "Fila", def = function(Fila,Registro)standardGeneric("ValidarFila"))


# Filas-class --------------------
#' Clase Filas
#'
#' Un objeto S4 para modelar varias Filas
#'
#' No pertenece a un sub-sistema directamente. Es una clase de apoyo.
#'
#' Contiene un resumen del estado de las filas de una tabla, pero no contiene sus datos.
#'
#' @slot Columnas un objeto con las \linkS4class{Columnas} de una tabla.
#' @slot Error indica si alguna de todas las filas contiene un error
#' @slot Errores contiene la lista de errores. Cada elemento de la lista contiene los errores de una fila.
#' @slot FilasEnError un vector numérico que identifica la posición de las filas con error.
#' @slot ColumnasEnError una lista donde cada elemento contiene un vector númerico con el índice de la columna que contiene el error.
#' @slot NombreTabla el nombre de la tabla.
#' @slot FilasCorrectas un vector numérico que identifica la posición de las filas sin errores.
#' @slot Status un vector con el estatus de cada fila.
#'
#' @name Filas-class
#' @rdname Filas-class
#'
#' @export
setClass("Filas", representation = list(
  Columnas = "character"
  , Error = "logical"
  , Errores = "list"
  , FilasEnError = "numeric"
  , ColumnasEnError = "list"
  , FilasCorrectas = "numeric"
  , Status = "character"
))

setValidity("Filas"
            ,method =
              function(object)
              {
                msg <- ""
                valido <- TRUE

                if (length(object@Error) > 1){
                  msg <- paste0(msg, "La fila está en error o no lo está. No se permiten vectores lógicos. ")
                  valido <- FALSE
                }

                if (length(object@NombreTabla) > 1)
                {
                  msg <-
                    paste0(msg, "Una fila pertenece a una tabla, no a varias. ")
                  valido <- FALSE
                }

                if (any(!(object@Status %in% eccdConfig$FilaStatus)))
                {
                  msg <-
                    paste0(msg, "Fila con Status no permitido. ")
                  valido <- FALSE
                }

                if (valido) TRUE else msg
              })

#  Accesores Filas ----------------
#' Acceder a elementos de  \linkS4class{Filas}.
#'
#' @docType methods
#' @name extractoresFilas
#' @rdname extractoresFilas
NULL

setGeneric("filasEnError", signature = "Filas", function(Filas)standardGeneric("filasEnError"))

#' @describeIn extractoresFilas Extrae filas en error (vector lógico)
#' @export
setMethod("filasEnError", "Filas", function(Filas) return(Filas@Error))


setGeneric("filasErrores", signature = "Filas", function(Filas)standardGeneric("filasErrores"))

#' @describeIn extractoresFilas Extrae lista de errores
#' @export
setMethod("filasErrores", "Filas", function(Filas) return(Filas@Errores))


setGeneric("filasErroresPos", signature = "Filas", function(Filas)standardGeneric("filasErroresPos"))

#' @describeIn extractoresFilas Extrae la posición de las filas con errores
#' @export
setMethod("filasErroresPos", "Filas", function(Filas) return(Filas@FilasEnError))


setGeneric("filasCorrectas", signature = "Filas", function(Filas)standardGeneric("filasCorrectas"))

#' @describeIn extractoresFilas Extrae las filas correctas (vector lógico)
#' @export
setMethod("filasCorrectas", "Filas", function(Filas) return(Filas@FilasCorrectas))


setGeneric("filasStatus", signature = "Filas", function(Filas)standardGeneric("filasStatus"))

#' @describeIn extractoresFilas Extrae el estatus de las filas
#' @export
setMethod("filasStatus", "Filas", function(Filas) return(Filas@Status))




# AgregarFila ------
#' Agrega una fila a la colección.
#'
#' Agrega una \linkS4class(Fila) a la colección de \linkS4class{Filas}. Antes de agregar la fila a la colección, llama a la función genérica \link{ValidarFila}; si encuentra un método apropiado, lo despacha y luego agrega la fila a la colección; de lo contrario, solo agrega la fila a la colección sin realizar validación. En el primer caso, el \emph{slot} \code{FueValidado = TRUE} y en el segundo caso su valor es \code{FALSE}.
#'
#' @return \linkS4class{Filas} o descendiente.
#'
#'
#' @param Registro es una fila del data frame. Las columnas deben tener los mismos nombres que el slot del objeto \code{Fila@@Columnas}.
#' @param Fila Un objeto \linkS4class{Fila} o descendiente.
#' @param Filas un objeto \linkS4class{Filas} donde el objeto \code{Fila} será guardado.
AgregarFila <- function(Registro, Fila, Filas, contador)
{
  if (!is(Fila, "Fila"))
    stop("Fila debe ser de clase 'Fila', o su descendiente.")
  if (!is(Filas, "Filas"))
    stop("Filas debe ser de clase 'Filas', o su descendiente.")
  if (!is(Registro, "data.frame"))
    stop("Registro debe ser de clase 'data.frame'.")

  claseFila <- class(Fila)
  existeMetodo <-
    existsMethod(
      "ValidarFila"
      ,
      signature = c(Fila = claseFila, Registro = "data.frame")
      #,where = .GlobalEnv
    )

  if (existeMetodo)
  {
    .FUN <-
      getMethod(
        "ValidarFila",
        signature = c(Fila = claseFila, Registro = "data.frame"),
        where = .GlobalEnv
      )
    if (!is.null(.FUN))
    {
      Fila <- .FUN(Fila, Registro)
      Fila@FueValidada <- TRUE
    }
  }

  Filas <- .AgregarFila(Fila, Filas, contador)
}


# Agrega una fila a la colección
.AgregarFila <-
  function(Fila, Filas, contador)
  {
      posicion <- contador
      Filas@Error <- append(Filas@Error,Fila@Error)
      Filas@Errores <- append(Filas@Errores, list(Fila@ErroresMsg))
      Filas@ColumnasEnError <- append(Filas@ColumnasEnError, Fila@ErroresIndice)
      Filas@Status <- append(Filas@Status, Fila@Status)
      if (Fila@Error)
      {
        Filas@FilasEnError <- append(Filas@FilasEnError, posicion)
      }
      else
      {
        Filas@FilasCorrectas <- append(Filas@FilasCorrectas, posicion)
      }
      return(Filas)
    }


# Validar filas----
#'
#' Valida todas las filas de un data frame.
#'
#' Si existe un método para validar filas, (una implementación de la función genérica \link{ValidarFila}); lo utiliza para validar todas las filas de un data.frame. Devuelve una colección de \linkS4class{Filas}, que luego pueden usarse para obtener las filas limpias y las filas en error del \link{data.frame}.
#'
#'  Utilizar el objeto \code{Filas}, devuelto por esta función para usarlo en conjunto con la función \link{TraerFilas(data, Filas)} que se encarga de filtrar un \code{data.frame} extrayendo las filas correctas; y la función \code{TraerErrores(data, Filas)} que extrae las filas erróneas.
#'
#' @return \linkS4class{Filas} o descendiente.
#'
#'
#' @param Datos un \link{data.frame}.
#' @param Fila Un objeto \linkS4class{Fila} o que describe las filas de los \code{Datos}.
#'
#' @export
ValidarFilas <- function(Fila, Datos)
{

  filas <- new("Filas")

  for (contador in 1:nrow(Datos))
  {
    filas <- AgregarFila(Datos[contador,], Fila, filas, contador)
  }

  return(filas)
}



# Traer filas correctas----
#'
#' Devuelve las filas que pasan la validación
#'
#' Requiere de una instancia de \linkS4class{Filas} generada por \link{ValidarFilas}, y un \code{data.frame}. La función mira entre las filas que han pasado la validación, y filtra el conjunto de datos para devolver solo las filas correctas.
#'
#' @seealso
#'  la función \link{TraerErrores} que extrae las filas errónea
#'
#'  @name Data-Extractors
#'  @rdname Data-Extractors
#'
#' @return un \link{data.frame}.
#'
#'
#' @param Datos un \link{data.frame}.
#' @param Filas Un objeto \linkS4class{Filas}.
#'
#' @export
TraerFilas <- function(Filas, Datos)
{
  stopifnot(is(Filas, "Filas"))
  stopifnot(is(Datos, "data.frame"))

  return(Datos[filasCorrectas(Filas),])

}


# Traer filas erróneas----
#'
#' Devuelve las filas que \bold{no pasan} la validación.
#'
#' Requiere de una instancia de \linkS4class{Filas} generada por \link{ValidarFilas}, y un \code{data.frame}. La función mira entre las filas que contienen errores, y filtra el conjunto de datos para devolver solo las filas incorrectas.
#'
#' @seealso
#'  la función \link{TraerFilas} que extrae las filas errónea
#'
#' @return un \link{data.frame}.
#'
#'
#' @param Datos un \link{data.frame}.
#' @param Filas Un objeto \linkS4class{Filas}.
#'
#'  @name Data-Extractors
#'  @rdname Data-Extractors
#'
#' @export
TraerErrores <- function(Filas, Datos)
{
  stopifnot(is(Filas, "Filas"))
  stopifnot(is(Datos, "data.frame"))

  return(Datos[filasErroresPos(Filas),])

}
