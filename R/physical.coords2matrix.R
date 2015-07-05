#--------------------------------------------
#' @title physical.coords2matrix
#' @description XXXX
#' 
#' @details XXXX
#'
#' @param XXXX
#' 
#' @return XXXX
#' 
#' @references \code{spatstat}
#'
#' @examples XXXX
#--------------------------------------------
physical.coords2matrix <- function(coord.mat, xyinc, num.rows, num.cols, filler=NaN) {
  
  dmat <- array(filler, c(num.rows, num.cols))
  ind.mat <- round(coord.mat[,1:2]/xyinc)
  
  for(i in 1:nrow(coord.mat)) {
    dmat[ ind.mat[i,1], ind.mat[i,2] ] <- coord.mat[i,3]
    #print(ind.mat[i,1])
    #print(ind.mat[i,2])
    #print(dmat[ ind.mat[i,1], ind.mat[i,2] ])
    #print("=========================")
  }
  
  return(dmat)

}