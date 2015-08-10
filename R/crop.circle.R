#--------------------------------------------
#' @title crop.circle
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
crop.circle <- function(x3p.surf.file.info, radius, type="outer", center.pt=NULL) {
  
  head.info <- x3p.surf.file.info[[1]]
  
  if(is.null(center.pt)){
    centerb <- c(round(head.info$num.lines/2), round(head.info$num.pts.line/2))
  } else {
    centerb <- center.pt #PUT IN A TRAP IF NOT TWO DIMENSIONAL
  }
  
  xinc <- as.numeric(head.info["x.inc"]) #should be microns already
  rb <- round(radius/xinc)
  
  print(paste("Center pixel:              ", centerb[1], centerb[2]))
  print(paste("Center pixel coords:       ", centerb[1]*xinc, centerb[2]*xinc))
  print(paste("Requested radius:          ", radius))
  print(paste("Pixel dx:                  ", xinc))
  print(paste("Length of radius in pixels:", rb))
  
  #Determine circular region to crop out in terms of matrix indices (pixels):
  circ.idxs <- circle.idxs(radius=radius, xc=0, yc=0, top.halfQ=TRUE)
  
  
  #Crop out the OUTER circular region:
  if(type=="outer") {
    
    #Initalize cropped surface container:
    cropped.x3p.surf.file.info <- x3p.surf.file.info
    cropped.surface <- array(NaN, dim(x3p.surf.file.info[[2]]) )
    
    for(i in 1:nrow(circ.idxs)) {
      col.idx <- circ.idxs[i,1] + centerb[1]
      
      top.row.idx <- circ.idxs[i,2]
      bot.row.idx <- -1*top.row.idx
      
      top.row.idx <- top.row.idx + centerb[2]
      bot.row.idx <- bot.row.idx + centerb[2]
      
      cropped.surface[top.row.idx:bot.row.idx, col.idx] <- x3p.surf.file.info[[2]][top.row.idx:bot.row.idx, col.idx]
      #print(paste("Column:", col.idx, " Get row:", top.row.idx, "to row:", bot.row.idx))    
    }
    
  } else if(type=="inner") { #Crop out the INNER circular region:
    
    #Initalize cropped surface container:
    cropped.x3p.surf.file.info <- x3p.surf.file.info
    cropped.surface <- cropped.x3p.surf.file.info[[2]]
    
    for(i in 1:nrow(circ.idxs)) {
      col.idx <- circ.idxs[i,1] + centerb[1]
      
      top.row.idx <- circ.idxs[i,2]
      bot.row.idx <- -1*top.row.idx
      
      top.row.idx <- top.row.idx + centerb[2]
      bot.row.idx <- bot.row.idx + centerb[2]
      
      cropped.surface[top.row.idx:bot.row.idx, col.idx] <- NaN
      #print(paste("Column:", col.idx, " Get row:", top.row.idx, "to row:", bot.row.idx))    
    }
    
  } else {
    stop("Pick outer or inner region of the circle to crop.")
  }
  
  cropped.x3p.surf.file.info[[1]]$num.pts.line <- ncol(cropped.surface)
  cropped.x3p.surf.file.info[[1]]$num.lines <- nrow(cropped.surface)
  cropped.x3p.surf.file.info[[2]] <- cropped.surface
  
  #Cut NaN borarder excess for outer circle crop
  if(type=="outer") {
    cropped.x3p.surf.file.info <- crop.retangle(cropped.x3p.surf.file.info, percent.off.each.side=0)
  }
  
  return(cropped.x3p.surf.file.info)
  
}

#-------------------------------------------------------------------------------
#Internal.
#Generate matrix indices of circle using Bresenham’s Midpoint Circle Algorithm
#-------------------------------------------------------------------------------
circle.idxs <- function(radius, xc, yc, top.halfQ=FALSE) {
  
  index.mat <- NULL
  x <- 0
  y <- radius
  d <- 5.0/4.0 - radius
  #   SetPixel(x,y);
  index.mat <- rbind(index.mat, c(x,y))
  while(y > x) {
    if (d < 0) {
      d <- d + 2.0 * x + 3.0 # Select East
    } else {
      d <- d + 2.0 * (x-y) + 5.0 #Select SE
      y <- y - 1
    }
    x <- x + 1
    index.mat <- rbind(index.mat, c(x,y))
  }
  
  #Note: redundant points not droped at intersection of symetry planes.
  index.mat <- rbind(index.mat, cbind(index.mat[,2],index.mat[,1]))       #Generate lower half of first quadrant
  
  if(top.halfQ==TRUE) {
    index.mat <- rbind(index.mat, cbind(-1 * index.mat[,1], index.mat[,2])) #Generate third quadrant
  } else {
    index.mat <- rbind(index.mat, cbind(index.mat[,1], -1*index.mat[,2]))   #Generate second quadrant
    index.mat <- rbind(index.mat, cbind(-1 * index.mat[,1], index.mat[,2])) #Generate third and fourth quadrants    
  }
  
  #Now drop redundant points (amazed this worked):
  index.mat <- unique(index.mat)
  
  index.mat <- cbind(index.mat[,1]+xc, index.mat[,2]+yc)                  #Shift indices to desired have desired center
  
  return(index.mat)
}