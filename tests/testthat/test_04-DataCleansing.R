#' @import testthat
NULL
library(testthat)
library(eccd)

# Debugging
# Create breakpoint, then source on save.

# test generics ---------------
testthat::test_that(
  "Columnas y filas",
  {
    #arrange
    expect_visible(
      Columnas(c("ID", "nombre", "peso"), c("integer", "character", "numeric")))

    cols <- Columnas(c("ID", "nombre", "peso"), c("integer", "character", "numeric"))

    expect_visible( Fila( Columnas =  cols))

    fila <- Fila(cols)

    data <-
      data.frame(altura = c(1.3, 2.1, 1, 1.81),
                 Nombre = c("Frodo", "Sauron", "Golum", "Gandalf"))


    setMethod(
      "ValidarFila",
      signature = c(Fila = "Fila", Registro = "data.frame"),
      definition =
        function(Fila, Registro)
        {
          if ("altura" %in% filaColNombre(Fila))
          {
            if (Registro$altura < 1.5)
            {
              filaError(Fila) <- TRUE
              filaMensajeError(Fila) <- "Altura inapropiada"
              filaColumnaEnError(Fila) <-
                purrr::detect_index(colnames(Registro), function(x)
                  x == "altura")
            }
          }
          return(Fila)
        }
      , where = .GlobalEnv
    )

    fila <-
      Fila( Columnas(Nombres = colnames(data), c("numeric", "character")))
    filas <- new("Filas")
    #debug(ValidarFilas)

    filas <-
      ValidarFilas(Fila = fila, Datos = data)
    #undebug(ValidarFilas)
    #act
    expect_equal(filasCorrectas(filas), c(2,4))
    expect_equal(filasEnError(filas), c(TRUE, FALSE, TRUE, FALSE))
    expect_equal(filasErrores(filas), list("Altura inapropiada",character(0),"Altura inapropiada",character(0)))
    expect_equal(filasErroresPos(filas), c(1,3))
    expect_equal(filasStatus(filas), c("Nueva","Nueva","Nueva","Nueva"))

    #clean
    rm(list = ls())
  })
