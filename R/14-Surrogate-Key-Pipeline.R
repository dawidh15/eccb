#' EncontrarSurrogateFuzzyMemo
#'
#' Encuentra el \emph{surrogate key} aplicando fuzzy matching y memoization.
#'
#' Primero se realiza un Left Join de la columna \emph{join} de la fuente de datos, con la columna \emph{join} de la dimensión. Idealmente, ambas columnas debería pasar por una normalización de texto, para mejorar la probabilidad de encontrar un match correcto.
#'
#' Si hay match exactos, se reservan en una tabla que contiene el valor original y el surrogate key correspondiente (\bold{exact matches}). Luego, se realiza un \bold{anti-join} entre las mismas tablas. Si existen valores sin un match, se inicia el proceso de memoization.
#'
#' Para cada valor sin match exacto:
#'
#' \describe{
#' \item{1}{¿Dato en memo?}
#' \item{1.a}{SI: Agregar surrogate key a \bold{exact matches}}
#' \item{1.b}{NO:  Pasar a 2.}
#' \item{2}{Calcular \code{stringdist} sobre los valores de la columna \emph{join} en la dimensión.}
#' \item{3}{Escoger mejor match.}
#' \item{4}{Agregar surrogate key de mejor match \bold{exact matches}.}
#' \item{5}{Alimentar memo con el nuevo match.
#' }}
#'
#' @section Combinar con NormalizarTexto:
#'
#' Normalizar la columna de combinación de la tabla dimensional, junto con la columna de combinación de la tabla fuente, y luego aplicar el fuzzy matching con memoization para mejores resultados.
#'
#' @param tablaDim una versión recortada de la tabla dimensional, con el campo de join y el surrogate key. Si el campo de join es texto, entonces lo mejor es normalizarlo.
#' @param tablaFuente una versión recortada de la tabla fuente, con el campo de join. Si el campo de join es texto, entonces lo mejor es normalizarlo.
#' @param nombreDimJoin el nombre de la columna en la tabla dimensional usada para combinar las tablas.
#' @param nombreDimClave el nombre de la columna en la tabla dimensional que contiene el surrogate key.
#' @param nombreFuenteJoin el nombre de la columna en la fuente de datos usada para combinar las tablas
#' @param maxChars el máximo número de caracteres permitido, en que un texto de la fuente difiere del texto limpio en la tabla de dimensión.
#' Por ejemplo, "Frodo" difiere de "frodo" en 1 caracter. Si \code{maxChars = 3}, entonces el match se toma como bueno.
#' Este parámetro proviene del método de distancia de Levenshtein.
#'
#' @importFrom stringdist stringdist
#'
#' @returns un vector de surrogate keys. Si un dato en el origen no encuentra un match apropiado (strigrdist > maxChars), devuelve \code{NA}.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Suponer que se extrae de un archivo fuente
#' columnaSucia <- c("lUnes ","MARTES.","Miércoles","Hueves","viernes","sabado","Domingo", "Lunes", "martes","Miércoles", "Hueves ","Fiernes,","Sábado","Domingo")
#'
#' dimTable <-
#'   data.frame(dias = c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")) %>%
#'   dplyr::mutate(Key = dplyr::row_number())
#'
#' # Normalizar texto
#' columnaSucia <- NormalizarTexto(columnaSucia)
#' dimTable <- dimTable %>%
#'   dplyr::mutate(diasClean = NormalizarTexto(dias)$Limpio)
#'
#' # Fuzzy matching
#' SK <-
#'   EncontrarSurrogateFuzzyMemo(
#'   tablaFuente = data.frame(dias = columnaSucia$Limpio)
#'   ,tablaDim = dimTable
#'   ,nombreDimJoin = "diasClean"
#'   ,nombreDimClave = "Key"
#'   ,nombreFuenteJoin = "dias"
#'   ,maxChars = 3)
#'
#' # Agregar surrogate key al source table
#' data <-
#'   data.frame(diasSrcNorm = columnaSucia$Limpio,
#'              diasSrcOrig = columnaSucia$Sucio,
#'              ID = SK) %>%
#'   left_join(dimTable, by = c("ID" = "Key"))
#'
#' data
EncontrarSurrogateFuzzyMemo <-
  function(tablaDim,
           tablaFuente,
           nombreDimJoin,
           nombreDimClave,
           nombreFuenteJoin,
           maxChars)
  {
    # Crear tablas con nombre y dimensiones apropiadas
    tablaFuente <- tablaFuente %>%
      dplyr::mutate(secuenciaFuente = dplyr::row_number()) %>%
      dplyr::select(nombreFuenteJoin, secuenciaFuente)
    colnames(tablaFuente) <- c("sourceJoin", "secuenciaFuente")

    tablaDim <- tablaDim %>%
      dplyr::select(nombreDimJoin, nombreDimClave)
    colnames(tablaDim) <- c("dimJoin", "dimKey")

    # Determinar si hay matches exactos
    coincidenciaExacta <-
      dplyr::inner_join(tablaFuente,
                       tablaDim,
                       by = c("sourceJoin" = "dimJoin"),
                       keep = TRUE)

    surrogateKeys <-
      data.frame(
        secuencia = dplyr::pull(coincidenciaExacta,secuenciaFuente )
        , Key = dplyr::pull(coincidenciaExacta, dimKey) )

    # si todas las files tienen un match, regresar surrogate key
    if (length(surrogateKeys$Key) == nrow(tablaFuente))
    {
      # Devolver clave en secuencia con tabla fuente
      return(DevolverEnSecuencia(surrogateTable = surrogateKeys))
    }


    # noCoincidencia
    noCoincidencia <-
      dplyr::anti_join(tablaFuente
                       ,
                       tablaDim
                       ,
                       by = c("sourceJoin" = "dimJoin")
                       ,
                       keep = TRUE)
    assertthat::not_empty(noCoincidencia)

    memo <-
      data.frame(original = character(), surrogateKey = numeric())

    for (contador in 1:nrow(noCoincidencia))
    {
      valorActual <- noCoincidencia$sourceJoin[contador]
      enMemo <-
        .EncontrarEnMemo(original = valorActual
                         , memo = memo)

      if (length(enMemo) == 0) {
        bestMatchedKey <-
          .EncontrarSurrogateFuzzy(buscaMe = valorActual
                                   ,
                                   buscaEnTabla = tablaDim
                                   ,
                                   maxChars = maxChars)

        surrogateKeys <-  surrogateKeys %>%
          dplyr::add_row(
            Key = bestMatchedKey
            , secuencia = noCoincidencia$secuenciaFuente[contador])

        memo <- memo %>%
          dplyr::add_row( original = valorActual,
                          surrogateKey = bestMatchedKey)

      } else {
        surrogateKeys <- surrogateKeys %>%
          dplyr::add_row(
            Key = enMemo
            , secuencia = noCoincidencia$secuenciaFuente[contador])
      }

    }# for
    return(DevolverEnSecuencia(surrogateTable = surrogateKeys))
  }


.EncontrarEnMemo <- function(original, memo)
{
  matchKey <- memo$surrogateKey[which(memo$original == original)]

  if (length(matchKey) == 1) {
    return(matchKey)
  } else if (length(matchKey) == 0) {
    return(numeric(length = 0L))
  } else {
    stop("memo no puede tener múltiples matches.")
  }
}

# EncontrarSurrogateFuzzyMemo Helpers------------------

.EncontrarSurrogateFuzzy <-
  function(buscaMe
           , buscaEnTabla
           , maxChars)
  {
    distancias <-
      purrr::map_dbl(buscaEnTabla$dimJoin
                     ,
                     stringdist::stringdist
                     ,
                     b = buscaMe
                     ,
                     method = "lv") # número de caracteres necesarios de cambiar para obtener match
    minDist <- min(distancias, na.rm = TRUE)
    bestMatchedKey <-
      buscaEnTabla$dimKey[which.min(distancias)]

    if (as.integer(minDist) <= as.integer(maxChars))
    {
      return(bestMatchedKey)
    } else {
      return(NA)
    }
  }

# Devolver clave en secuencia con tabla fuente
DevolverEnSecuencia <- function(surrogateTable)
{
  surrogateTable <-
    surrogateTable %>%
    dplyr::arrange(secuencia)
  return(surrogateTable$Key)
}

