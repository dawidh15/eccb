#' EncontrarSurrogateFMemo
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
#' \item{1} ¿Dato en memo?
#' \item{1.a} SI: Agregar surrogate key a \bold{exact matches}
#' \item{1.b} NO:  Pasar a 2.
#' \item{2} Calcular \code{stringdist} sobre los valores de la columna \emph{join} en la dimensión.
#' \item{3} Escoger mejor match.
#' \item{4} Agregar surrogate key de mejor match \bold{exact matches}.
#' \item{5} Alimentar memo con el nuevo match.
#' }
#'
#' @param dimTabla una versión recortada de la tabla dimensional, con el campo de join y el surrogate key. Si el campo de join es texto, entonces lo mejor es normalizarlo.
#' @param sourceTabla una versión recortada de la tabla fuente, con el campo de join. Si el campo de join es texto, entonces lo mejor es normalizarlo.
#'
#' @importFrom stringdist stringdist
#'
#' @export
#'
#' @examples
#' ## don't run this in calls to 'example(add_numbers)'
#' \dontrun{
#'    EncontrarSurrogateFMemo(2, 3)
#' }
EncontrarSurrogateFMemo <-
  function(dimTabla, sourceTabla, dimJoin, dimKey, sourceJoin)
  {

  }
