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
fill.holes <- function(x3p.surf.file.info, hole.obj, 
                       big.hole.tol=2, 
                       big.hole.fill.typ ="surface.mean", 
                       hole.window.factor=1.5, 
                       fill.type.df = data.frame("all.the.same", "1D.interpolation"), 
                       printQ = FALSE) {
  
  #Get the number of holes we have to do something with
  num.holes <- nlevels(hole.obj)
  filled.info <- rep("Not filled", num.holes)
  
  #Create a labeled mask for the holes. I.E. Label the points beloning to each hole with their hole number.
  hole.mask <- matrix(as.numeric(hole.obj$v), nrow=nrow(hole.obj$v), ncol=ncol(hole.obj$v))
  hole.mask[which(is.na(hole.mask)==TRUE, arr.ind = TRUE)] <- 0 #Non-hole points are labeled 0.
    
  #Initalize the output:
  filled.surface <- x3p.surf.file.info[[2]]
  
  #Number of points in the surface file.
  num.pts <- nrow(x3p.surf.file.info[[2]])*ncol(x3p.surf.file.info[[2]])
  
  #This is what to fill big holes with if they are found:
  if(big.hole.fill.typ == "surface.mean") {
    
    big.hole.filler <- mean(x3p.surf.file.info[[2]], na.rm=TRUE)
    
  } else if((class(big.hole.fill.typ) == "numeric") | (class(big.hole.fill.typ) == "logical")) {
    
    big.hole.filler <- big.hole.fill.typ
    
  } else {
    
    big.hole.filler <- 0.0
    
  }
  
  #Examine each hole and do something with it. This is parallelalizable:
  for(i in 1:num.holes) {
    
    #print(paste("Hole", i ,"start."))
    
    #Hole indices for hole i
    hole.idxs <- which(hole.mask==i, arr.ind=TRUE)
    
    #Holes shouldn't be that gigantic. If a hole is > big.hole.tol% of the surface points, just flood fill it.
    nan.frac <- nrow(hole.idxs)/num.pts
    if(nan.frac > big.hole.tol/100) {
      
      #print("Big Hole!")
      filled.surface[hole.idxs] <- big.hole.filler
      filled.info[i] <- "Filled"
      
    } else {
      
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
      
      #if(expan.maxx == expan.minx) {
      # print(paste("************************Hole window", i, "width is 0."))
      #}
      
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
      
      #if(expan.maxy == expan.miny) {
      # print(paste("******************************Hole window", i, "height is 0."))
      #}
      
      hole.window <- as.matrix(x3p.surf.file.info[[2]][expan.miny:expan.maxy, expan.minx:expan.maxx])
      #print(hole.window)
      #print(dim(hole.window))
      #print(class(hole.window))
      
      #----------
      if(fill.type.df[1,1] == "all.the.same") {
        
        filled.hole.window <- fill.func(hole.window, fill.type.df[1,2])
        filled.surface[expan.miny:expan.maxy, expan.minx:expan.maxx] <- filled.hole.window
        
      }
      #----------
      
      filled.info[i] <- "Filled"
    }
    
    if(printQ==TRUE) {
      print(paste("Hole ", i, "done."))
    }
    
    #print("====================================")
    
  }
  #print(filled.info)
  
  return(filled.surface)
  
}

#-----------------------------------------------------------------
#Internal function. For filling holes with a desired method.
#-----------------------------------------------------------------
fill.func <- function(a.window, fill.typ) {
  
  if(fill.typ == "1D.interpolation") {
    
    #Fill all holes with lateral 1D interpolation if <= 5% NaNs, otherwise fill with window mean:
    filled.window <- array(0.0, dim( a.window ))
    for(i in 1:nrow(filled.window)) {
      
      #print(is.na(a.window[i,]))
      nan.frac <- sum(is.nan(a.window[i,]))/length(a.window[i,])
      #print(nan.frac)

      if((nan.frac > 0.2) & (nan.frac < 1) ) {
        #Fill row with the window mean if more than 5% of the points are NAN
        filled.window[i,] <- rep(mean(a.window, na.rm=TRUE), ncol(filled.window))
      } else if(nan.frac == 1) {
        #All data in the row are NaNs, skip it and print a warning
        filled.window[i,] <- a.window[i,]
        warning("No data in window line. Skipping")
      } else if(nan.frac == 0) {
        #If all of the points are there in a row of the window, then just keep them
        filled.window[i,] <- a.window[i,]
      } else {
        #1D interpolation for the row
        filled.window[i,] <- na.spline(a.window[i,])
      }
      
    }
    
  }
  
  #Fill window with a constant
  if(class(fill.typ) == "numeric") {
    filled.window <- array(fill.typ, dim( a.window ))
  }
  
  #Fill window with window mean
  if(fill.typ == "window.mean") {
    filled.window <- array(mean(a.window, na.rm=TRUE), dim( a.window ))
  }
  
  return(filled.window)
  
}