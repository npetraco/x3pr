#--------------------------------------------
#' @title Read an x3p file
#'
#' @description Read an x3p file
#' 
#' @details Unzips file to tempdir().Returns a list. The first element is the x3p file header. The
#' second element is the surface data in the form of a matrix.
#'
#' @param fpath
#' 
#' @return a list
#' 
#' @references http://open-gps.sourceforge.net/
#'
#' @examples
#' file.path <- system.file("extdata", "glock.x3p", package="x3pr")
#' glock.x3p.info <- read.x3p(file.path)
#' glock.x3p.info[[1]]
#--------------------------------------------
read.x3p<-function(fpath){
  
  unzip(fpath, exdir=tempdir())
  
  doc <- xmlRoot(xmlTreeParse(paste(tempdir(),"/main.xml",sep="")))
  #print(doc)
  #print(table(names(doc)))
  
  rec1 <- doc$children$Record1
  rec2 <- doc$children$Record2
  rec3 <- doc$children$Record3
  rec4 <- doc$children$Record4
  
  
  #Record 1 information:
  
  #Surface or profile?
  FeatureType <- xmlValue(rec1$children$FeatureType)
  
  #x-axis inforamtion
  tmp <-xmlChildren(rec1$children$Axes$children$CX)
  if(length(tmp)==0) {
    xaxis.info <- list(NA) #Note: if this is NA, there may be a problem
  } else {
    xaxis.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(xaxis.info) <- names(tmp)
  #print(xaxis.info)
  
  #y-axis inforamtion
  tmp <-xmlChildren(rec1$children$Axes$children$CY)
  if(length(tmp)==0) {
    yaxis.info <- list(NA) #Note: if this is NA, there may be a problem
  } else {
    yaxis.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(yaxis.info) <- names(tmp)
  #print(yaxis.info)
  
  #z-axis inforamtion
  tmp <-xmlChildren(rec1$children$Axes$children$CZ)
  if(length(tmp)==0) {
    zaxis.info <- list(NA) #If this is NA, it probably means surface points are just stored as a vector. No increment strictly necessary
  } else {
    zaxis.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(zaxis.info) <- names(tmp)
  #print(zaxis.info)
  
  
  #Record 2 information
  
  #Get date of file creation:
  tmp <-xmlChildren(rec2$children$Date)
  if(length(tmp)==0) {
    date.info <- list("Date of creation specified")
  } else {
    date.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(date.info) <- "File creation date and time"
  #print(date.info)
  
  #Get name of file creator:
  tmp <-xmlChildren(rec2$children$Creator)
  if(length(tmp)==0) {
    creator.info <- list("No data set creator specified")
  } else {
    creator.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(creator.info) <- "Data set creator"    
  #print(creator.info)
  
  # Instrument_Manufacturer:
  tmp <-xmlChildren(rec2$children$Instrument$children$Manufacturer)
  if(length(tmp)==0) {
    manufacturer.info <- list("No Instrument Manufacturer specified")
  } else {
    manufacturer.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(manufacturer.info) <- "Instrument Manufacturer"    
  #print(manufacturer.info)
  
  # Instrument_Model:
  tmp <-xmlChildren(rec2$children$Instrument$children$Model)
  if(length(tmp)==0) {
    model.info <- list("No Instrument Model specified")
  } else {
    model.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(model.info) <- "Instrument Model"    
  #print(model.info)
  
  # Instrument_Serial:
  tmp <-xmlChildren(rec2$children$Instrument$children$Serial)
  if(length(tmp)==0) {
    serial.number.info <- list("No Instrument Serial# specified")
  } else {
    serial.number.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(serial.number.info) <- "Instrument Serial#"    
  #print(serial.number.info)
  
  # Instrument_Version:
  tmp <-xmlChildren(rec2$children$Instrument$children$Version)
  if(length(tmp)==0) {
    version.info <- list("No Instrument Version specified")
  } else {
    version.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(version.info) <- "Instrument Version"
  #print(version.info)
  
  # CalibrationDate:
  tmp <-xmlChildren(rec2$children$CalibrationDate)
  if(length(tmp)==0) {
    calibration.date.info <- list("No Calibration Date specified")
  } else {
    calibration.date.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(calibration.date.info) <- "Instrument Calibration Date"
  #print(calibration.date.info)
  
  # ProbingSystem_Type:
  tmp <-xmlChildren(rec2$children$ProbingSystem$children$Type)
  if(length(tmp)==0) {
    prob.type.info <- list("No instrument probing system type specified")
  } else {
    prob.type.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(prob.type.info) <- "Instrument Probing System Type"
  #print(prob.type.info)
  
  # ProbingSystem_Identification:
  tmp <-xmlChildren(rec2$children$ProbingSystem$children$Identification)
  if(length(tmp)==0) {
    prob.id.info <- list("No probing system identification specified")
  } else {
    prob.id.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(prob.id.info) <- "Instrument Probing System Identification"
  #print(prob.id.info)
  
  # Comment:
  tmp <-xmlChildren(rec2$children$Comment)
  if(length(tmp)==0) {
    comment.info <- list("No Comments Given")
  } else {
    comment.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  }
  names(comment.info) <- "Comment"
  #print(comment.info)
  
  
  #Record 3 information
  
  #Grab dimension of the surface to reassemble it from points
  tmp <-xmlChildren(rec3$children$MatrixDimension)
  surface.dim.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
  names(surface.dim.info) <- names(tmp)
  #print(surface.dim.info)
  
  #Read out the surface z-heights. Is this ever a matrix??? Says so in the documentation. Ask at meeting 4.13.14
  surf.data.path <- paste(tempdir(),"/bindata/data.bin",sep="")
  #print(surf.data.path)
  ptr<-file(surf.data.path, "rb") #Open up a connection to the .bin file
  
  #Move the pointer to where the profiles begin:
  seek(ptr, where = 0, rw="r") #Offset should always be 0 since surface points start in a separate file.
  #Read out the surface points. NOTE: Assuming data is signed.
  if(zaxis.info$DataType=="I"){ #2-byte (shorts/halfs) ints. Zeiss CSM-700 does this.
    point.byte.depth <- 2
    surface3 <- readBin(ptr, what=integer(), size = point.byte.depth, n = ((as.numeric(surface.dim.info$SizeX))*(as.numeric(surface.dim.info$SizeY))), signed = TRUE, endian = "little")
    surface3 <- (1e6) * (as.numeric(zaxis.info$Offset) + surface3*as.numeric(zaxis.info$Increment))
  }
  if(zaxis.info$DataType=="L"){ #4-byte ints.
    point.byte.depth<-4
    surface3<-readBin(ptr, what=integer(), size = point.byte.depth, n = ((as.numeric(surface.dim.info$SizeX))*(as.numeric(surface.dim.info$SizeY))), signed = TRUE, endian = "little")
    #CONVERT TO Meters?????
  }
  if(zaxis.info$DataType=="F"){ #4-byte floats (singles)
    point.byte.depth<-4
    surface3<-readBin(ptr, what="numeric", size = point.byte.depth, n = ((as.numeric(surface.dim.info$SizeX))*(as.numeric(surface.dim.info$SizeY))), signed = TRUE, endian = "little")
    #CONVERT TO Meters?????
  }
  if(zaxis.info$DataType=="D"){ #8-byte floats (doubles)
    point.byte.depth<-8
    surface3<-readBin(ptr, what="numeric", size = point.byte.depth, n = ((as.numeric(surface.dim.info$SizeX))*(as.numeric(surface.dim.info$SizeY))), signed = TRUE, endian = "little")
    #CONVERT TO Meters?????
  }
  close(ptr)
  
  #print(surface3)
  #Units should come out in microns, meters ..... ????
  #Xreal = Xoffset + Xinteger∗XIncrement.
  
  #Units in microns:
  surface3 <- (1e6 * t(matrix(surface3, ncol=(as.numeric(surface.dim.info$SizeY)), nrow=(as.numeric(surface.dim.info$SizeX)))))
  #Units in meters:
  #surface3 <- (t(matrix(surface3, ncol=(as.numeric(surface.dim.info$SizeY)), nrow=(as.numeric(surface.dim.info$SizeX)))))
  
  header.info <- list(
    as.character(FeatureType),
    as.character(date.info),
    as.character(creator.info),
    as.character(manufacturer.info),
    as.character(model.info),
    as.character(serial.number.info),
    as.character(version.info),
    as.character(calibration.date.info),
    as.character(prob.type.info),
    as.character(prob.id.info),
    as.character(comment.info),
    as.character(zaxis.info$DataType),
    as.numeric(xaxis.info$Increment)*1e6, #Converts to microns
    as.numeric(yaxis.info$Increment)*1e6, #Converts to microns
    as.numeric(surface.dim.info$SizeY), 
    as.numeric(surface.dim.info$SizeX)
  )
  names(header.info) <- c("feature.type","date","creator","manufacturer","model","serial.number","version",
                          "calibration.date","probe.type","probe.id","comment",
                          "stored.data.type","x.inc","y.inc","num.lines","num.pts.line")
  
  surface.info <- list(header.info, surface3)
  names(surface.info) <- c("header.info", "surface.matrix")
  
  return(surface.info)
}