#--------------------------------------------
# This sets a (hopefully) reasonable resolution
# for the number of points to plot for a surface
# in the shinyRGL based viewer.
#--------------------------------------------
resolution.initalizer <- function(surf.mat){
  
  leng <- dim(surf.mat)[1]
  wid <- dim(surf.mat)[2]
  tot.pts <- leng*wid
  if(tot.pts > 65536) { #2^16 pts may top out most browsers. 
    
    if(wid > 256) { #Check the width (number of cols) first. Usually this is the bigger of the two
      wid <- round(0.1*wid)
      tot.pts <- leng*wid #recompute the total number of points
      
      if(tot.pts > 65536){
        leng <- round(0.1*leng)
        tot.pts <- leng*wid #recompute the total number of points
    
        if(tot.pts > 65536){ #If still too high, need to be a little drastic. Reset and do this instead:
          leng <- dim(surf.mat)[1]
          wid <- dim(surf.mat)[2]
          scale.factor <- sqrt(65536/(leng*wid))
          leng <- floor(scale.factor*leng)
          wid <- floor(scale.factor*wid)
        }
      }
    }
  }
  
  return(c(leng,wid))
  
}