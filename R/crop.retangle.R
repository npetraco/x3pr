#--------------------------------------------
#' @title crop.retangle
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
crop.retangle <- function(x3p.surf.file.info, percent.off.each.side=1) {
  
  cropped.x3p.surf.file.info <- x3p.surf.file.info
  
  #Remove any Na/NaN boundry
  col.rm.idxs <- which(colSums(x3p.surf.file.info[[2]], na.rm=T) == 0)
  row.rm.idxs <- which(rowSums(x3p.surf.file.info[[2]], na.rm=T) == 0)
  
  cropped.x3p.surf.file.info[[2]] <- x3p.surf.file.info[[2]][-row.rm.idxs,-col.rm.idxs]

  rred <- ceiling(ncol(cropped.x3p.surf.file.info[[2]])*(percent.off.each.side/100))
  cred <- ceiling(nrow(cropped.x3p.surf.file.info[[2]])*(percent.off.each.side/100))
  
  cropped.x3p.surf.file.info[[2]] <- cropped.x3p.surf.file.info[[2]][-c(1:rred,(nrow(cropped.x3p.surf.file.info[[2]])-rred):nrow(cropped.x3p.surf.file.info[[2]])),
                                                                     -c(1:cred,(ncol(cropped.x3p.surf.file.info[[2]])-cred):ncol(cropped.x3p.surf.file.info[[2]]))]
  
  cropped.x3p.surf.file.info[[1]]$num.lines <- nrow(cropped.x3p.surf.file.info[[2]])
  cropped.x3p.surf.file.info[[1]]$num.pts.line <- ncol(cropped.x3p.surf.file.info[[2]])
  
  return(cropped.x3p.surf.file.info)

}