biomasa_brac <- function(long) {
  b <- 0.6 * long
  c <- 0.4 * long
  vol <- 0.52 * long * b *c
  vol
}

biomasa_fili <- function(long) {
  b <- 0.5 * long
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

  if (stringr::str_detect(especie,"Filinia")) {
    volumen <- biomasa_fili(long)
  }
  volumen

}
