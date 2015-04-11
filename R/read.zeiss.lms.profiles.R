#--------------------------------------------
#' @title Read out surface data in Zeiss LMS format.
#'
#' @description Read out surface data in Zeiss LMS format.
#' 
#' @details Read a surface file in Zeiss LMS format. This function is 
#' formulated as a helper to read.zeiss.lms.file and refered to as ~.profiles
#' for historical reasons.
#'
#' @param pt a pointer to a Zeiss LMS file
#' @return a matrix. The surface data in the Zeiss LMS file.
#' @seealso read.zeiss.lms.file
#' @examples
#' Coming soon.
#'
#' \dontrun{
#'  
#' }
#--------------------------------------------
read.zeiss.lms.profiles<-function(pt,header.info) {
  
  #stop("NOT WORKING")
    
  #Offset for profiles:
  offset <- header.info$offset.to.img2 + header.info$bfoffbits2
  #print(offset)
  
  #Move the pointer to where the profiles begin:
  seek(pt, where = offset, rw="r")
  
  #Read out the profiles. Units should be un microns:  
  point.byte.depth<-header.info$bibitcount2/8
  
  surface3<-readBin(pt, what=integer(), size = point.byte.depth, n = ((header.info$biwidth2)*(header.info$biheight2)), signed = F, endian = "little")
  
  #Assumes z units (height.inc) are microns IMPROVE!
  surface3<- header.info$height.inc * t(matrix(surface3, ncol=header.info$biheight2, nrow=header.info$biwidth2))
  
  return(surface3)
 
}