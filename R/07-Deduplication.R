
# Limpiar texto

#' Normalizar  texto
#'
#' Tomar un texto, elimina los espacios, los signos de puntuación, y lo sube a mayúsculas. Usar para aumentar las probabilidades de eliminar duplicados por mala escritura, o como paso previo a un fuzzy matching con \link{EncontrarSurrogateFuzzyMemo}.
#'
#'
#' Pertenece al \bold{sub-sistema 07 de de-duplicación}.
#'
#' @param  text el texto que se desea normalizar.
#'
#' @return Una lista con el texto limpio (\code{text$Limpio}) y el texto original (\code{text$Sucio})
#'
#' @export
NormalizarTexto <- function(text)
{
  if (!is(text, "character")) return()

  textLimpio <- stringr::str_replace_all(text, "[:punct:]", "")
  textLimpio <- stringr::str_to_upper(
    stringr::str_squish(textLimpio)
  )
  textLimpio <- LimpiarVocal(textLimpio)

  return(list(Limpio = textLimpio, Sucio = text))
}

# Quitar tildes, no exportar
LimpiarVocal <- function(text)
{
  text <- stringr::str_replace_all(text,"[Á|À|Ä|Â]", "A")
  text <- stringr::str_replace_all(text,"[É|È|Ë|Ê]", "E")
  text <- stringr::str_replace_all(text,"[Í|Ì|Ï|Î]", "I")
  text <- stringr::str_replace_all(text,"[Ó|Ò|Ö|Ô]", "O")
  text <- stringr::str_replace_all(text,"[Ú|Ù|Ü|Û]", "U")
  return(text)
}
