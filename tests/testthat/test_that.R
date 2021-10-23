#' @import testthat
NULL

# Debugging
# Create breakpoint, then source on save.

# test generics ---------------
testthat::test_that(
  ".EncontrarEnMemo",
  {
    #arrange
    buscar <- "hola"
    badMemo <- "badMemo"
    match1 <- 1; match2 <- 123
    memo <- data.frame(
      original = c(buscar, badMemo,badMemo),
      surrogateKey = c(match1,match2,match2)
    )
    #act
    val1 <- .EncontrarEnMemo(buscar, memo = memo)
    val2 <- .EncontrarEnMemo("noExiste", memo = memo)
    testthat::expect_identical(val1,expected = match1)
    testthat::expect_length(val2, 0)
    testthat::expect_error(.EncontrarEnMemo(badMemo, memo = memo))

    #clean
    rm(list = ls())
  })


testthat::test_that(
  ".EncontrarSurrogateFuzzy",
  {
    #arrange
    match1 <- 1
    match2 <- 2
    testDimTable <-
      data.frame(dimJoin = c("Frodo", "Sam"),
                 dimKey = c(match1, match2))
    #act
    val1 <-
      .EncontrarSurrogateFuzzy(buscaMe = "frrodo",
                               buscaEnTabla = testDimTable,
                               maxChars = 3)
    val2 <-
      .EncontrarSurrogateFuzzy(buscaMe = "Ssammass",
                               buscaEnTabla = testDimTable,
                               maxChars = 3)
    val3 <-
      .EncontrarSurrogateFuzzy(buscaMe = "frodo", buscaEnTabla = testDimTable, maxChars = 3)

    testthat::expect_identical(val1,expected = match1)
    testthat::expect_identical(val2,NA)
    testthat::expect_identical(val3,expected = match1)
    #clean
    rm(list= ls())
  })



testthat::test_that(
  "EncontrarSurrogateFuzzyMemo",
  {
    #arrange
    match1 <- 1
    match2 <- 2
    testDimTable <-
      data.frame(personaje = c("Frodo", "Sam"),
                 personaje_key = c(match1, match2))
    testSourceTable <-
      data.frame(personajes = c("Frodo", "Sam","Fordo", "Zam","Fordo", "frodo", "Sam", "Frodo", "ssAsms"))
    expectedMatches <- c(match1,match2,match1,match2,match1,match1,match2,match1,NA)
    #act
    val1 <-
      EncontrarSurrogateFuzzyMemo(
        tablaDim = testDimTable,
        tablaFuente = testSourceTable,
        nombreDimJoin = "personaje",
        nombreDimClave = "personaje_key",
        nombreFuenteJoin = "personajes",
         maxChars = 3)

    testthat::expect_identical(val1,expected = expectedMatches)

    #clean
    rm(list= ls())

    #arrange
    match1 <- 1
    match2 <- 2
    testDimTable <-
      data.frame(personaje = c("Frodo", "Sam"),
                 personaje_key = c(match1, match2))
    testSourceTable <-
      data.frame(personajes = c("Frodo", "Frodo","Sam", "Frodo","Frodo", "Sam", "Sam", "Sam", "Sam"))
    expectedMatches <- c(match1,match1,match2,match1,match1,match2,match2,match2,match2)
    #act
    val1 <-
      EncontrarSurrogateFuzzyMemo(
        tablaDim = testDimTable,
        tablaFuente = testSourceTable,
        nombreDimJoin = "personaje",
        nombreDimClave = "personaje_key",
        nombreFuenteJoin = "personajes",
        maxChars = 3)

    testthat::expect_identical(val1,expected = expectedMatches)

    #clean
    rm(list= ls())
  })
