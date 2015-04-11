#--------------------------------------------
#' @title Read a surface file in Zeiss LMS format.
#' 
#' @description Read a surface file in Zeiss LMS format.
#' 
#' @details Read a surface file in Zeiss LMS format.
#' 
#' @param file.path
#' @return a list. The Zeiss LMS header info and the surface.
#' 
#' @examples
#'file.path <- system.file("extdata", "glock.lms", package="x3pr")
#' glock.lms.info <- read.zeiss.lms.file(file.path)
#' glock.lms.info[[1]]
#--------------------------------------------
read.zeiss.lms.file<-function(file.path) {
  
  #stop("NOT WORKING")
  
  ptr<-file(file.path, "rb") #Open up a connection to the .sur file
  header.info<-read.zeiss.lms.header(ptr)
  surface.matrix<-read.zeiss.lms.profiles(ptr,header.info)
  close(ptr)
  
  all.file.info<-c(list(NULL),list(NULL))
  names(all.file.info)<-c("Surface Info","Surface")
  all.file.info[[1]]<-header.info
  all.file.info[[2]]<-surface.matrix
  
  return(all.file.info)

}