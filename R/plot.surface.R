#--------------------------------------------
#' @title 3D plot  of a surface read in by x3pr package
#' @description 3D plot  of a surface 
#' 
#' @details 3D plot  of a surface. X3P, SUR and LMS are all plotted by this function. 
#' Some type of decimation is usually
#' required so that the surface can be rendered fast. The arguments num.x.pts
#' and num.slices allow the user to subsample the surface down to the desited size.
#' If the number of points is still potentially large, the function will prompt the 
#' user if they want to continue.
#'
#' @param surf.file.info information read in by \code{read.x3p}, \code{read.digital.surf.file} or \code{read.zeiss.lms.file}
#' @param num.x.pts number of points desired in the x-direction.
#' @param num.slices number of points desired in the y-direction.
#' @param aspect aspect ratios
#' @param plot.type "points" or "surface"
#' 
#' @return a plot.
#' 
#' @references http://open-gps.sourceforge.net/
#'
#' @examples
#' file.path <- system.file("extdata", "glock.x3p", package="x3pr")
#' glock.x3p.info <- read.x3p(file.path)
#' plot3D.x3p.file(glock.x3p.info, 1024, 80, aspect=c(1,3,0.4), plot.type="surface")
#--------------------------------------------
plot.surface<-function(surf.file.info, num.x.pts=NULL, num.slices=NULL, aspect=c(1,0.3,0.2), plot.type="points") {
    
  head.info<-surf.file.info[[1]]
  #print(head.info)
  surf.mat<-surf.file.info[[2]]
  
  #print(head.info["num.pts.line"])
  
  #Downsample the surface points for faster plotting:
  if(is.null(num.x.pts)){
    dec.col.idxs<-seq(from=1,to=as.numeric(head.info["num.pts.line"]),by=1)    
  } else {
    dec.col.idxs<-unique(round(seq(from=1,to=as.numeric(head.info["num.pts.line"]), length.out=num.x.pts)))
    if(length(dec.col.idxs)>ncol(surf.mat)){
      stop("Number of requested points in x-direction (number of points per profile) exceeds number of columns in surface matrix!")
    }
  }
  if(is.null(num.slices)){
    dec.row.idxs<-seq(from=1,to=as.numeric(head.info["num.lines"]),by=1)
  } else {
    dec.row.idxs<-unique(round(seq(from=1,to=as.numeric(head.info["num.lines"]), length.out=num.slices)))
    if(length(dec.row.idxs)>nrow(surf.mat)){
      stop("Number of requested points in y-direction (numner of profiles/slices) exceeds number of rows in surface matrix!")
    }
  }
  
  tot.num.pts <- length(dec.col.idxs)*length(dec.row.idxs)
  
  print(paste("# y-points: ",length(dec.row.idxs)))
  print(paste("# x-points: ",length(dec.col.idxs)))
  print(paste("Total # points: ", tot.num.pts))
  
  #More than 100,000 pts? Ask to plot.
  if(tot.num.pts>=65536){
    print("Lots of points. May be slow and Browser may not support it (shiny apps). Sure you want to do this? (y/n)")
    ans <- readline()
    if(ans=="n") {
      print("OK. Won't try to plot.")
      return()
    }
  }  
  
  decimated.surf.mat<-surf.mat[dec.row.idxs,dec.col.idxs]
  
  #Generate x and y axes to scale
  num.pts.per.line<-as.numeric(head.info["num.pts.line"])
  xinc<-as.numeric(head.info["x.inc"]) #should be microns already
  xaxis<-seq(from=0,to=xinc*(num.pts.per.line-1),length.out=num.x.pts)
  dec.xaxis<-xaxis[dec.col.idxs]
  #   
  num.lines<-as.numeric(head.info["num.lines"])
  yinc<-as.numeric(head.info["y.inc"]) #should be microns already FIX THIS LATER
  yaxis<-seq(from=0,to=yinc*(num.lines-1),length.out=num.slices)
  dec.yaxis<-yaxis[dec.row.idxs]
  
  #print(yinc)
  #print(xinc)
  
  
  if(plot.type=="points"){
    
    coords<-cbind(expand.grid(X=xaxis, Y=yaxis), as.numeric(t(decimated.surf.mat)))
    x<-coords[,1]
    y<-coords[,2]
    z<-coords[,3]
    
    #Swap x and y axes to put origin in top left corner (image coordinates)
    plot3d(y,x,z,radius=0.01, xlab="x",ylab="y",zlab="z",col="black",aspect=aspect,type="p")
    
  } else if(plot.type=="surface") {
    
    nbcol = 256
    color = rev(rainbow(nbcol, start = 0/6, end = 2/6)) #Color band width
    #zcol  = cut(t(decimated.surf.mat), nbcol)   
    zcol  = cut(decimated.surf.mat, nbcol)   
    #persp3d(xaxis, yaxis, t(decimated.surf.mat), aspect=aspect, col=color[zcol])
    
    #Swap x and y axes to put origin in top left corner (image coordinates)
    persp3d(yaxis, xaxis, decimated.surf.mat, aspect=aspect, col=color[zcol])
    
  } else {
    print("Pick surface or points.")
  }      
  
}