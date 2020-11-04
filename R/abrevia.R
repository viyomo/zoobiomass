#' Title
#'
#' @param genero cadena
#' @param especie cadena
#'
#' @return abreviatura
#' @export
#'
# @examples
abrevia <- function(genero, especie="vacia"){

  if (especie == "vacia"){
    ab <- stringr::str_sub(genero,1,8)
  } else {
    ab <- paste(stringr::str_sub(genero,1,4),".",stringr::str_sub(especie,1,4), sep = "")
  }
  ab

}
