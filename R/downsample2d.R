#--------------------------------------------
#' @title downsample2d
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
downsample2d <- function(dmat, num.x.pts, num.y.pts) {
  
  num.pts.line <- ncol(dmat)
  num.lines <- nrow(dmat)
  
  #Downsample the surface points for faster plotting:
  if(is.null(num.x.pts)){
    dec.col.idxs<-seq(from=1, to=num.pts.line, by=1)    
  } else {
    dec.col.idxs<-unique(round(seq(from=1, to=num.pts.line, length.out=num.x.pts)))
    if(length(dec.col.idxs)>ncol(dmat)){
      stop("Number of requested points in x-direction (number of points per profile) exceeds number of columns in surface matrix!")
    }
  }
  if(is.null(num.y.pts)){
    dec.row.idxs<-seq(from=1, to=num.lines, by=1)
  } else {
    dec.row.idxs<-unique(round(seq(from=1, to=num.lines, length.out=num.y.pts)))
    if(length(dec.row.idxs)>nrow(dmat)){
      stop("Number of requested points in y-direction (numner of profiles/slices) exceeds number of rows in surface matrix!")
    }
  }
  
  tot.num.pts <- length(dec.col.idxs)*length(dec.row.idxs)
  
  print(paste("# y-points: ",length(dec.row.idxs)))
  print(paste("# x-points: ",length(dec.col.idxs)))
  print(paste("Total # points: ", tot.num.pts))
  
  downsampled.dmat <- dmat[dec.row.idxs,dec.col.idxs]
  
  return(downsampled.dmat)
  
}