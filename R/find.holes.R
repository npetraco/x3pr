#--------------------------------------------
#' @title find.holes
#' @description XXXX
#' 
#' @details XXXX
#'
#' @param dmat The surface of points
#' 
#' @return A \code{spatstat} pixel image object (object of class im).
#' 
#' @references \code{spatstat}
#'
#' @examples XXXX
#--------------------------------------------
find.holes <- function(dmat, plotQ=FALSE){
  
  hole.obj <- connected(as.im(is.nan(dmat)), method="C", background=0)
  num.holes <- length(levels(hole.obj))
  print(paste("There are", num.holes, "holes."))
  
  
  if(plotQ==TRUE & num.holes >0) {
    hole.mask <- matrix(as.numeric(hole.obj$v), nrow=nrow(dmat), ncol=ncol(dmat))
    hole.mask[which(is.na(hole.mask)==TRUE, arr.ind = TRUE)] <- 0
    
    fields::image.plot(t(sapply(1:ncol(hole.mask),function(x){rev(hole.mask[,x])})),add=F,nlevel=(num.holes+1))
  }
  
  return(hole.obj)
  
}