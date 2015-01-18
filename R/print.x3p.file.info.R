#--------------------------------------------
#' @title Format (most) x3p header file info into a nice
#' read-able format
#' @description Format (most) x3p header file info into a nice
#' read-able format
#' 
#' @details none. 
#'
#' @param main.xml.info
#' 
#' @return a string.
#' 
#' @references http://open-gps.sourceforge.net/
#'
#' @examples
#' file.path <- system.file("extdata", "glock.x3p", package="x3pr")
#' glock.x3p.info <- read.x3p(file.path)
#' glock.x3p.info[[1]]
#' print.x3p.file.info(glock.x3p.info[[1]])
#--------------------------------------------
print.x3p.file.info<-function(main.xml.info){
  
  print("------X3P File Meta Data------")
  
  if(main.xml.info["feature.type"]=="SUR") {
    print("File is a:                       SURFACE")
  }
  if(main.xml.info["feature.type"]=="PRO") {
    print("File is a:                       PROFILE")
  }
  if(length(main.xml.info["feature.type"])==0) {
    print("Problem! No surface type given!")
  }
  
  print(paste("File creation date and time:     ", main.xml.info["date"],sep=""))  
  print(paste("File creator:                    ", main.xml.info["creator"],sep=""))  
  print(paste("Instrument manufacturer:         ", main.xml.info["manufacturer"],sep=""))  
  print(paste("Instrument model:                ", main.xml.info["model"],sep=""))  
  print(paste("Instrument serial number:        ", main.xml.info["serial.number"],sep=""))
  print(paste("Instrument version:              ", main.xml.info["version"],sep=""))  
  print(paste("Instrument calibration date:     ", main.xml.info["calibration.date"],sep=""))
  print(paste("Instrument probe type:           ", main.xml.info["probe.type"],sep=""))  
  print(paste("Instrument probe identification: ", main.xml.info["probe.id"],sep=""))  
  if(main.xml.info["stored.data.type"]=="I") {
    print(paste("Type for stored z-axis data:     ", "Signed 16-bit integer (short/half)",sep=""))
  }
  if(main.xml.info["stored.data.type"]=="L") {
    print(paste("Type for stored z-axis data:     ", "Signed 32-bit integer (int)",sep=""))
  }
  if(main.xml.info["stored.data.type"]=="F") {
    print(paste("Type for stored z-axis data:     ", "32-bit floating point (single)",sep=""))
  }
  if(main.xml.info["stored.data.type"]=="I") {
    print(paste("Type for stored z-axis data:     ", "64-bit floating point (double)",sep=""))
  }
  if(length(main.xml.info["stored.data.type"]) == 0) {
    print(paste("Type for stored z-axis data:     ", "NONE SPECIFIED!!!!!! BAD!!!!",sep=""))
  }
  print(paste("Height (#points):                ", main.xml.info["num.lines"],sep=""))  
  print(paste("Width  (#points):                ", main.xml.info["num.pts.line"],sep=""))  
  print(paste("Comments:                        "))
  print(strsplit(main.xml.info[["comment"]], ",")[[1]] )      
  print("------------------------------")
  
}