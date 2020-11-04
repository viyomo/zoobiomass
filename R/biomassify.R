biomasa_brac <- function(long, k) {
  b <- 0.6 * long
  c <- 0.4 * long
  vol <- (0.52 * long * b *c)/1000000
  ps <- vol * k + vol/100
  ps
}

biomasa_fili <- function(long, k) {
  b <- 0.5 * long
  vol <- (0.52 * long * b^2) /1000000
  ps <- vol * k
  ps
}

biomasa_kera <- function(long, k) {
  b <- 0.7 * long
  c <- 0.33 * long
  vol <- (long * b * c) / 1000000
  ps <- vol * k
  ps
}

biomasa_polyar <- function(long, k) {
  b <- 0.7 * long
  c <- 0.4 * long
  vol <- (long * b * c) / 1000000
  ps <- vol *k
  ps
}

biomasa_pompho <- function(long, k) {
  b <- 0.7 * long
  c <- 0.5 * long
  vol <- (0.4 * long * b * c) /1000000
  ps <- vol * k
  ps
}

biomasa_testu <- function(long, k) {
  b <- long
  c <- 0.2 * long
  vol <- (0.4 * long * b * c) / 1000000
  ps <-   vol * k
  ps
}

biomasa_notho <- function(long, k) {
  b <- 0.4 * long
  c <- 0.2 * long
  vol <- (0.13 * (3 * long * b * c) + 4 * c^3) / 1000000
  ps <- vol *k
  ps
}

biomasa_asplac <- function(long, k) {
  b <- 0.7 * long
  vol <- (0.52 * long * b^2) /1000000
  ps <-  vol *k
  ps
}

biomasa_nauplii <- function(long, k) {
  c <- 0.7 * long
  b <- c
  vol <- (0.52 * long * b^2)/1000000
  ps <-   vol*k
  ps
}
#' Calcula el peso seco
#'
#' @param especie cadena
#' @param long     longitud
#' @param k     porcentage sin agua
#'
#' @return    volumen
#' @export
#'
# @examples

biomassify <- function(especie, long, k = 0.1) {

  if (stringr::str_detect(especie,"Brachionus")) {
    volumen <- biomasa_brac(long, k)
  }

  else if (stringr::str_detect(especie,"Filinia")) {
    volumen <- biomasa_fili(long, k)
  }

  else if (stringr::str_detect(especie,"Keratella")) {
    volumen <- biomasa_kera(long, k)
  }

  else if (stringr::str_detect(especie,"Polyarthra")) {
    pseco <- biomasa_polyar(long, k)
}

  else if (stringr::str_detect(especie,"Pompholyx")) {
    pseco <- biomasa_pompho(long, k)
}

  else if (stringr::str_detect(especie,"Testudinella")) {
    pseco <- biomasa_testu(long, k)
  }

  else if (stringr::str_detect(especie,"Notholca")) {
    pseco <- biomasa_notho(long, k)
  }

  else if (stringr::str_detect(especie,"Asplachna")) {
    pseco <- biomasa_asplac(long, k)
  }

  else {
    pseco <- 0
  }
  pseco

}

