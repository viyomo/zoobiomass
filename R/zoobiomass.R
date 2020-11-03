biomasa_brac <- function(long) {
  long <- long/1000   # paso a mm
  b <- 0.6 * long
  c <- 0.4 * long
  vol <- 0.52 * long * b *c
  vol
}


#' Title
#'
#' @param especie nombre
#' @param long   longitu in micrometers
#'
#' @return volumen en mm cubicos
#' @export
#'
#' @examples
#' long <- 320 # in micrometers
#' zoobiomass("brac", long)
zoobiomass <- function(especie, long) {

  if (especie=="brac") {
    biomasa_brac(long)

  }
}
