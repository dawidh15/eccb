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
    val1 <- eccd:::.EncontrarEnMemo(buscar, memo = memo)
    val2 <- eccd:::.EncontrarEnMemo("noExiste", memo = memo)
    testthat::expect_identical(val1,expected = match1)
    testthat::expect_length(val2, 0)
    testthat::expect_error(eccd:::.EncontrarEnMemo(badMemo, memo = memo))

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
      eccd:::.EncontrarSurrogateFuzzy(buscaMe = "frrodo",
                               buscaEnTabla = testDimTable,
                               maxChars = 3)
    val2 <-
      eccd:::.EncontrarSurrogateFuzzy(buscaMe = "Ssammass",
                               buscaEnTabla = testDimTable,
                               maxChars = 3)
    val3 <-
      eccd:::.EncontrarSurrogateFuzzy(buscaMe = "frodo", buscaEnTabla = testDimTable, maxChars = 3)

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


# Guardar ConexionSQLiteTabla Tibble
testthat::test_that("Probar Guardar(ConexionSQLite, Tibble)",{

  registro <-
    dplyr::tibble(
      #My_Key = NULL,
      Nombre = c("David M", "Karen D"),
      Altura = c(1.67,1.56),
      Fecha = c(as.numeric(lubridate::as_datetime(lubridate::ymd("2021-10-14")))
                ,as.numeric(lubridate::as_datetime(lubridate::ymd("2021-10-14"))))
    )


  sql <-
    "CREATE TABLE IF NOT EXISTS Altura(
    My_Key INTEGER NOT NULL UNIQUE
    , Nombre TEXT NOT NULL
    , Altura NUMERIC DEFAULT 0
    , Fecha REAL NOT NULL

    ,CONSTRAINT hola_pk PRIMARY KEY(My_Key AUTOINCREMENT)
  )"
  con <- DBI::dbConnect(RSQLite::SQLite(), "test2.sqlite")
  rs <- DBI::dbSendQuery(con, sql)
  DBI::dbClearResult(rs)

  conexion <- ConexionSQLite(RutaDB = "test2.sqlite", Tabla = "Altura")
  valor <- Guardar(registro, conexion)
  testthat::expect_true(valor)


  registroMalo <-
    dplyr::tibble(
      #My_Key = NULL,
      Nombre = "Ana M",
      Altura = 1.54,
      Fecha = NULL
    )

  valor <- Guardar(registroMalo, conexion)
  testthat::expect_false(valor)

  DBI::dbDisconnect(con)

  testthat::expect_error( # verifica que la tabla existe, de lo contrario arroja error.
    ConexionSQLite(RutaDB = "test2.sqlite", NombreTabla = "noExisto"))

  fs::file_delete("test2.sqlite")
    rm(list = ls())

})



testthat::test_that("Probar Extraer(ConexionRDS)", {
 x <- rnorm(10)
 saveRDS(x,file = "test.RDS")


 Rconn <- ConexionRDS("test.RDS")
 Robject <- Extraer(Rconn)
 testthat::expect_visible(Robject)
 testthat::expect_gte(object.size(Robject),0)
 fs::file_delete("test.RDS")
 rm(list = ls())
}
)
