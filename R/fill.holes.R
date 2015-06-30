#--------------------------------------------
#' @title fill.holes
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
fill.holes <- function(x3p.surf.file.info, hole.obj, hole.window.factor=1.5, fill.type.mat = NULL, printQ = FALSE){
  
  num.holes <- nlevels(hole.obj)
  
  #Create a labeled mask for the holes. I.E. Label the points beloning to each hole with their hole number.
  hole.mask <- matrix(as.numeric(hole.obj$v), nrow=nrow(hole.obj$v), ncol=ncol(hole.obj$v))
  hole.mask[which(is.na(hole.mask)==TRUE, arr.ind = TRUE)] <- 0 #Non hole points are labeled 0.
    
  #Fill all holes with lateral 1D interpolation if <= 5% NaNs, otherwise fill with 0s:
  filled.surface <- array(0.0, dim(x3p.surf.file.info[[2]]))
  for(i in 1:num.holes) {
    
    #Hole indices for hole i
    hole.idxs <- which(hole.mask==i, arr.ind=TRUE)
    
    #Indices of box enclosing hole
    minx <- min(hole.idxs[,2]) #Starting column index
    maxx <- max(hole.idxs[,2]) #Ending column index
    miny <- min(hole.idxs[,1]) #Starting row index
    maxy <- max(hole.idxs[,1]) #Ending row index
    
    #Expand the box around the hole (150% in each direction) to get a little more data for the hole filling function
    expan.x <- round(hole.window.factor*(maxx-minx))
    expan.y <- round(hole.window.factor*(maxy-miny))
    
    expan.minx <- minx - expan.x
    #If you fall off the left edge, bring it back to the left boarder
    if(expan.minx <= 0) {
      expan.minx <- 1
    }
    
    expan.maxx <- maxx + expan.x
    #If you go over the right edge, bring it back to the right boarder
    if(expan.maxx > ncol(x3p.surf.file.info[[2]])) {
      expan.maxx <- ncol(x3p.surf.file.info[[2]])
    }
    
    expan.miny <- miny - expan.y
    #If you back off the top edge, bring it back to the top boarder
    if(expan.miny <= 0) {
      expan.miny <- 1
    }
    
    expan.maxy <- maxy + expan.y
    #If you go over the bottom edge, bring it back to the bottom boarder
    if(expan.maxy > nrow(x3p.surf.file.info[[2]])) {
      expan.maxy <- nrow(x3p.surf.file.info[[2]])
    }

    hole.window <- crop[expan.miny:expan.maxy, expan.minx:expan.maxx]
    print(hole.window)
    
    print("===========")
    
    
  }
  
  
  return(filled.surface)
  
}