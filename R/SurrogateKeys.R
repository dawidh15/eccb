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
#' @param dimTabla una versión recortada de la tabla dimensional, con el campo de join y el surrogate key. Si el campo de join es texto, entonces lo mejor es normalizarlo.
#' @param sourceTabla una versión recortada de la tabla fuente, con el campo de join. Si el campo de join es texto, entonces lo mejor es normalizarlo.
#'
#' @importFrom stringdist stringdist
#'
#' @returns un vector de surrogate keys. Si un dato en el origen no encuentra un match apropiado (strigrdist > maxChars), devuelve \code{NA}.
#'
#' @export
#'
#' @examples
#' ## don't run this in calls to 'example(add_numbers)'
#' \dontrun{
#'    EncontrarSurrogateFuzzyMemo(2, 3)
#' }
EncontrarSurrogateFuzzyMemo <-
  function(dimTabla,
           sourceTabla,
           dimJoinName,
           dimKeyName,
           sourceJoinName,
           maxChars)
  {
    # Crear tablas con nombre y dimensiones apropiadas
    sourceTabla <- sourceTabla %>%
      dplyr::select(sourceJoinName)
    colnames(sourceTabla) <- c("sourceJoin")

    dimTabla <- dimTabla %>%
      dplyr::select(dimJoinName, dimKeyName)
    colnames(dimTabla) <- c("dimJoin", "dimKey")

    # Determinar si hay matches exactos
    exactMatches <-
      dplyr::inner_join(sourceTabla,
                       dimTabla,
                       by = c("sourceJoin" = "dimJoin"),
                       keep = TRUE)

    surrogateKeys <- dplyr::pull(exactMatches, dimKey)

    # si todas las files tienen un match, regresar surrogate key
    if (length(surrogateKeys) == nrow(sourceTabla))
      return(surrogateKeys)

    # noMatches
    noMatches <-
      dplyr::anti_join(sourceTabla
                       ,
                       dimTabla
                       ,
                       by = c("sourceJoin" = "dimJoin")
                       ,
                       keep = TRUE)
    assertthat::not_empty(noMatches)

    memo <-
      data.frame(original = character(), surrogateKey = numeric())

    for (contador in 1:nrow(noMatches))
    {
      valorActual <- noMatches$sourceJoin[contador]
      enMemo <-
        .EncontrarEnMemo(original = valorActual
                         , memo = memo)

      if (length(enMemo) == 0) {
        bestMatchedKey <-
          .EncontrarSurrogateFuzzy(buscaMe = valorActual
                                   ,
                                   buscaEnTabla = dimTabla
                                   ,
                                   maxChars = maxChars)
        surrogateKeys <- append(surrogateKeys, bestMatchedKey)
        memo <- memo %>%
          dplyr::add_row( original = valorActual,
                          surrogateKey = bestMatchedKey)
      } else {
        surrogateKeys <- append(surrogateKeys, enMemo)
      }

    }# for
    return(surrogateKeys)
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
