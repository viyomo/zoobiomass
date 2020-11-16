biomasa_brac <- function(long, k) {
  b <- 0.6 * long
  c <- 0.4 * long
  vol <- (0.52 * long * b *c)/1000000
  ps <- vol * k
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
  vol <- (long * b * c) /1000000
  ps <- vol * k
  ps
}

biomasa_polyar <- function(long, k) {
  b <- 0.7 * long
  c <- 0.4 * long
  vol <- (long * b * c) /1000000
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
  vol <- (0.4 * long * b * c) /1000000
  ps <-   vol * k
  ps
}

biomasa_notho <- function(long, k) {
  b <- 0.4 * long
  c <- 0.2 * long
  vol <- (0.13 * (3 * long * b * c) + 4 * c^3) /1000000
  ps <- vol *k
  ps
}

biomasa_asplac <- function(long, k) {
  b <- 0.7 * long
  vol <- (0.52 * long * b^2) /1000000
  ps <-  vol *k
  ps
}

biomasa_nau_cyclo <- function(long, k) {#evaluar los nombres de los nauplis
  b <- 0.000011
  ps <- b * long^1.89
  ps
}

biomasa_nau_cala <- function(long, k) {#evaluar los nombres de los nauplis
  b <- 0.000011
  ps <- b * long^1.89
  ps
}

biomasa_nau_harpa <- function(long, k) {#evaluar los nombres de los nauplis
  b <- 0.000011
  ps <- b * long^1.89
  ps
}

biomasa_cope_cyclo <- function(long, k) {#evaluar los nombres de los nauplis
  b <- 0.000011
  ps <- b * long^1.89
  ps
}

biomasa_cope_cala <- function(long, k) {#evaluar los nombres de los nauplis
  b <- 0.000011
  ps <- b * long^1.89
  ps
}

biomasa_cope_harpa <- function(long, k) {#evaluar los nombres de los nauplis
  b <- 0.000011
  ps <- b * long^1.89
  ps
}

biomasa_cyclo <- function(long, k) {#evaluar los nombres de los adultos de copepodos
  b <- 0.00000011
  ps <- b * long^2.59
  ps
}

biomasa_cala <- function(long, k) {#evaluar los nombres de los adultos de copepodos
  b <-long/1000
  c <- 0.0077 * b^2.33
  ps <- c * 1000
  ps
}

biomasa_harpa <- function(long, k) {#evaluar los nombres de los adultos de copepodos
  b <-long/1000
  ps <- 12.51 * b^4.4
  ps
}

biomasa_bosmi <- function(long, k) {
  b <-long/1000
  c <- 2.7116 + (2.5294 * log(b))
  ps <- exp(c)
  ps
}

biomasa_moina <- function(long, k) {
  b <-long/1000
  ps <- 6.61 * b^2.57
  ps
}

biomasa_diapha <- function(long, k) {
  b <-long/1000
  c <- 1.6242 + (3.0468 * log(b))
  ps <- exp(c)
  ps
}

biomasa_daphnia <- function(long, k) {
  b <-long/1000
  c <- 1.0727 + (2.0915 * log(b))
  ps <- exp(c)
  ps
}

biomasa_cerioda <- function(long, k) {
  b <-long/1000
  c <- 2.5623 + (3.338 * log(b))
  ps <- exp(c)
  ps
}

biomasa_alona <- function(long, k) {
  b <-long/1000
  ps <- 29.65 * b^3.48
  ps
}

biomasa_coronatella <- function(long, k) {
  b <-long/1000
  ps <- 29.65 * b^3.48
  ps
}

biomasa_leydi <- function(long, k) {
  b <-long/1000
  ps <- 15.92 * b^3.84
  ps
}

biomasa_macro <- function(long, k) {
  b <-long
  ps <- (1.903 * 10^-6) * b^2.13
  ps
}

biomasa_ovalona <- function(long, k) {
  b <-long/1000
  ps <- 29.65 * b^3.48
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

  if (all(stringr::str_detect(especie,"Brac"))) {   #ya
    pseco <- biomasa_brac(long, k)
  }

  else if (all(stringr::str_detect(especie,"Fili"))) {  #ya
    pseco <- biomasa_fili(long, k)
  }

  else if (all(stringr::str_detect(especie,"Kera"))) {  #ya
    pseco <- biomasa_kera(long, k)
  }

  else if (all(stringr::str_detect(especie,"Poly"))) { #ya
    pseco <- biomasa_polyar(long, k)
}

  else if (all(stringr::str_detect(especie,"Pomp"))) {#ya
    pseco <- biomasa_pompho(long, k)
}

  else if (all(stringr::str_detect(especie,"Test"))) { #ya
    pseco <- biomasa_testu(long, k)
  }

  else if (all(stringr::str_detect(especie,"Noth"))) {  #ya
    pseco <- biomasa_notho(long, k)
  }

  else if (all(stringr::str_detect(especie,"Aspl"))) {   #ya
    pseco <- biomasa_asplac(long, k)
  }

  else if (all(stringr::str_detect(especie,"Cycl.naup"))) {  #ya
    pseco <- biomasa_nau_cyclo(long, 1)
  }

  else if (all(stringr::str_detect(especie,"Cala.naup"))) {  #ya
    pseco <- biomasa_nau_cala(long, 1)
}

  else if (all(stringr::str_detect(especie,"Harp.naup"))) {  #ya
    pseco <- biomasa_nau_harpa(long, 1)
}

  else if (all(stringr::str_detect(especie,"Cycl.cope"))) {   #ya
    pseco <- biomasa_cope_cyclo(long, 1)
}

  else if (all(stringr::str_detect(especie,"Cala.cope"))) {    #ya
    pseco <- biomasa_cope_cala(long, 1)
}

  else if (all(stringr::str_detect(especie,"Harp.cope"))) {    #ya
    pseco <- biomasa_cope_harpa(long, 1)
}

  else if (all(stringr::str_detect(especie,"Cyclopoi"))) {  #ya
    pseco <- biomasa_cyclo(long, 1)
}

  else if (all(stringr::str_detect(especie,"Calanoid"))) { #ya
    pseco <- biomasa_cala(long, 1)
}

  else if (all(stringr::str_detect(especie,"Harpacti"))) { #ya
    pseco <- biomasa_harpa(long, 1)
}

  else if (all(stringr::str_detect(especie,"Bosmina"))) { #ya
    pseco <- biomasa_bosmi(long, 1)
}

  else if (all(stringr::str_detect(especie,"Moina"))) { #ya
    pseco <- biomasa_moina(long, 1)
}

  else if (all(stringr::str_detect(especie,"Diaphano"))) {  #ya
    pseco <- biomasa_diapha(long, 1)
}

  else if (all(stringr::str_detect(especie,"Daphnia"))) {   #ya
    pseco <- biomasa_daphnia(long, 1)
}

  else if (all(stringr::str_detect(especie,"Ceriodap"))) {#ya
    pseco <- biomasa_cerioda(long, 1)
}

  else if (all(stringr::str_detect(especie,"Alon"))) {   #ya
    pseco <- biomasa_alona(long, 1)
}

  else if (all(stringr::str_detect(especie,"Coro"))) {  #ya
    pseco <- biomasa_coronatella(long, 1)
  }

  else if (all(stringr::str_detect(especie,"Leyd"))) { #ya
    pseco <- biomasa_leydi(long, 1)
}

  else if (all(stringr::str_detect(especie,"Macr"))) {  #ya
    pseco <- biomasa_macro(long, 1)
}

  else if (all(stringr::str_detect(especie,"Oval"))) {  #ya
    pseco <- biomasa_ovalona(long, 1)
}


  else {
    pseco <- 0
  }
  #pseco

}

