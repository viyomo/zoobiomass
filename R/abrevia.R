#' Title
#'
#' @param genero cadena
#'
#' @return abreviatura
#' @export
#'
# @examples
abrevia <- function(generos){

  genero <- stringr::str_split_fixed(generos, " ", n=2)[1]
  especie <- stringr::str_split_fixed(generos, " ", n=2)[2]

  if (especie == ""){
    ab <- stringr::str_sub(genero,1,8)
  } else {
    ab <- paste(stringr::str_sub(genero,1,4),".",stringr::str_sub(especie,1,4), sep = "")
  }
  ab

}
