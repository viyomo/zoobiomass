biomasa_brac <- function(long) {
  b <- 0.6 * long
  c <- 0.4 * long
  vol <- (0.52 * long * b *c)/1000000
  peso_seco <- vol/10 + vol/100
}

biomasa_fili <- function(long) {
  b <- 0.5 * long
  vol <- 0.52 * long * b^2
  vol
}

biomasa_kera <- function(long) {
  b <- 0.7 * long
  c <- 0.33 * long
  vol <- long * b * c
  vol
}

biomasa_polyar <- function(long) {
  b <- 0.7 * long
  c <- 0.4 * long
  vol <- long * b * c
  vol
}

biomasa_pompho <- function(long) {
  b <- 0.7 * long
  c <- 0.5 * long
  vol <- 0.4 * long * b * c
  vol
}

biomasa_testu <- function(long) {
  b <- long
  c <- 0.2 * long
  vol <- 0.4 * long * b * c
  vol
}

biomasa_notho <- function(long) {
  b <- 0.4 * long
  c <- 0.2 * long
  vol <- 0.13 * (3 * long * b * c) + 4 * c^3
  vol
}

biomasa_asplac <- function(long) {
  c <- 0.7 * long
  b <- c
  vol <- 0.52 * long * b^2
  vol
}

biomasa_nauplii <- function(long) {
  c <- 0.7 * long
  b <- c
  vol <- 0.52 * long * b^2
  vol
}
#' Title
#'
#' @param especie cadena
#' @param long     longitud
#'
#' @return    volumen
#' @export
#'
# @examples

biomassify <- function(especie, long) {

  if (stringr::str_detect(especie,"Brachionus")) {
    volumen <- biomasa_brac(long)
  }

  else if (stringr::str_detect(especie,"Filinia")) {
    volumen <- biomasa_fili(long)
  }

  else if (stringr::str_detect(especie,"Keratella")) {
    volumen <- biomasa_kera(long)
  }

  else if (stringr::str_detect(especie,"Polyarthra")) {
    volumen <- biomasa_polyar(long)
}

  else if (stringr::str_detect(especie,"Pompholyx")) {
    volumen <- biomasa_pompho(long)
}

  else if (stringr::str_detect(especie,"Testudinella")) {
    volumen <- biomasa_testu(long)
  }

  else if (stringr::str_detect(especie,"Notholca")) {
    volumen <- biomasa_notho(long)
  }

  else if (stringr::str_detect(especie,"Asplachna")) {
    volumen <- biomasa_asplac(long)
  }

  else {
    volumen <- 0
  }
  volumen

}

