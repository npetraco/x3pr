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
  #print(FeatureType)
  
  
  #x-axis inforamtion
  if(!is.null(rec1$children$Axes$children$CX)){
    tmp <-xmlChildren(rec1$children$Axes$children$CX)
    if(length(tmp)==0) {
      xaxis.info <- list(NA) #Note: if this is NA, there may be a problem
      print("X-axis information listed in main.xml, but it is empty!")
    } else {
      xaxis.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(xaxis.info) <- names(tmp)
    #print(xaxis.info)
    
  } else {
    warning("No x-axis information field in Record 1!")
    xaxis.info <- "NULL"
    names(xaxis.info) <- "x-axis.info"
  }
    
  
  #y-axis inforamtion
  if(!is.null(rec1$children$Axes$children$CY)) {
    tmp <-xmlChildren(rec1$children$Axes$children$CY)
    if(length(tmp)==0) {
      yaxis.info <- list(NA) #Note: if this is NA, there may be a problem
    } else {
      yaxis.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(yaxis.info) <- names(tmp)
    #print(yaxis.info)
    
  } else {
    warning("No y-axis information field in Record 1!")
    yaxis.info <- "NULL"
    names(yaxis.info) <- "y-axis.info"
    
  }
  
  
  #z-axis inforamtion
  if(!is.null(rec1$children$Axes$children$CZ)) {
    tmp <-xmlChildren(rec1$children$Axes$children$CZ)
    if(length(tmp)==0) {
      zaxis.info <- list(NA) #If this is NA, it probably means surface points are just stored as a vector. No increment strictly necessary
    } else {
      zaxis.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(zaxis.info) <- names(tmp)
    #print(zaxis.info)
    
  } else {
    stop("Critical! No z-axis information field in Record 1!")
    #zaxis.info <- "NULL"
    #names(zaxis.info) <- "z-axis.info"
    
  }
  
  
  #Record 2 information
  
  #Get date of file creation:
  if(!is.null(rec2$children$Date)) {
    tmp <-xmlChildren(rec2$children$Date)
    if(length(tmp)==0) {
      date.info <- list("Date of creation specified")
    } else {
      date.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(date.info) <- "File creation date and time"
    #print(date.info)
    
  } else {
    date.info <- "NULL"
    names(date.info) <- "File creation date and time"
    print("No file creation date in Record 2.")
  }

  
  #Get name of file creator:
  if( !is.null(rec2$children$Creator) ){
    tmp <-xmlChildren(rec2$children$Creator)
    if(length(tmp)==0) {
      creator.info <- list("No data set creator specified")
    } else {
      creator.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(creator.info) <- "Data set creator"    
    #print(creator.info)
    
  } else {
    print("No Creator field in Record 2")
    creator.info <- "NULL"
    names(creator.info) <- "Data set creator"    
    #print(creator.info)
    
  }
  
  
  # Instrument_Manufacturer:
  if(!is.null(rec2$children$Instrument$children$Manufacturer)) {
    tmp <-xmlChildren(rec2$children$Instrument$children$Manufacturer)
    if(length(tmp)==0) {
      manufacturer.info <- list("No Instrument Manufacturer specified")
    } else {
      manufacturer.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(manufacturer.info) <- "Instrument Manufacturer"    
    #print(manufacturer.info)
    
  } else {
    print("No Instrument Manufacturer field in Record 2.")
    manufacturer.info <- "NULL"
    names(manufacturer.info) <- "Instrument Manufacturer"
    
  }
  
  
  # Instrument_Model:
  if(!is.null(rec2$children$Instrument$children$Model)) {
    tmp <-xmlChildren(rec2$children$Instrument$children$Model)
    if(length(tmp)==0) {
      model.info <- list("No Instrument Model specified")
    } else {
      model.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(model.info) <- "Instrument Model"    
    #print(model.info)
    
  } else {
    print("No Model Info field in Record 2.")
    model.info <- "NULL"
    names(model.info) <- "Instrument Model"
    
  }
  
  
  # Instrument_Serial:
  if(!is.null(rec2$children$Instrument$children$Serial)){
    tmp <-xmlChildren(rec2$children$Instrument$children$Serial)
    if(length(tmp)==0) {
      serial.number.info <- list("No Instrument Serial# specified")
    } else {
      serial.number.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(serial.number.info) <- "Instrument Serial#"    
    #print(serial.number.info)
    
  } else {
    print("No Serial Number field in Record 2.")
    serial.number.info <- "NULL"
    names(serial.number.info) <- "Instrument Serial#"    
    
  }
  
  
  # Instrument_Version:
  if(!is.null(rec2$children$Instrument$children$Version)) {
    tmp <-xmlChildren(rec2$children$Instrument$children$Version)
    if(length(tmp)==0) {
      version.info <- list("No Instrument Version specified")
    } else {
      version.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(version.info) <- "Instrument Version"
    #print(version.info)
    
  } else {
    print("No Instrument Version field in Record 2.")
    version.info <- "NULL"
    names(version.info) <- "Instrument Version"
  }
  
  # CalibrationDate:
  if(!is.null(rec2$children$CalibrationDate)) {
    tmp <-xmlChildren(rec2$children$CalibrationDate)
    if(length(tmp)==0) {
      calibration.date.info <- list("No Calibration Date specified")
    } else {
      calibration.date.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(calibration.date.info) <- "Instrument Calibration Date"
    #print(calibration.date.info)
    
  } else {
    calibration.date.info <- "NULL"
    names(calibration.date.info) <- "Instrument Calibration Date"
    print("No Instrument Calibration Date field in Record 2.")
    
  }
  
  
  # ProbingSystem_Type:
  if(!is.null(rec2$children$ProbingSystem$children$Type)) {
    tmp <-xmlChildren(rec2$children$ProbingSystem$children$Type)
    if(length(tmp)==0) {
      prob.type.info <- list("No instrument probing system type specified")
    } else {
      prob.type.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(prob.type.info) <- "Instrument Probing System Type"
    #print(prob.type.info)
    
  } else {
    prob.type.info <- "NULL"
    names(prob.type.info) <- "Instrument Probing System Type"
    print("No Instrument Probe Type field in Record 2.")
  }
  
  # ProbingSystem_Identification:
  if(!is.null(rec2$children$ProbingSystem$children$Identification)){
    tmp <-xmlChildren(rec2$children$ProbingSystem$children$Identification)
    if(length(tmp)==0) {
      prob.id.info <- list("No probing system identification specified")
    } else {
      prob.id.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(prob.id.info) <- "Instrument Probing System Identification"
    #print(prob.id.info)
    
  } else {
    prob.id.info <- "NULL"
    names(prob.id.info) <- "Instrument Probing System Identification"
    print("No probe ID field in Record 2.")
  }
  
  # Comment:
  if(!is.null(rec2$children$Comment)){
    tmp <-xmlChildren(rec2$children$Comment)
    if(length(tmp)==0) {
      comment.info <- list("No Comments Given")
    } else {
      comment.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
    }
    names(comment.info) <- "Comment"
    #print(comment.info)
    
  } else {
    print("No Comment field in Record 2.")
    comment.info <- "NULL"
    names(comment.info) <- "Comment"
    #print(comment.info)
  }

  
  #Record 3 information
  
  #Grab dimension of the surface to reassemble it from points
  if(!is.null(rec3$children$MatrixDimension)) {
    if(length(tmp) == 0) {
      stop("Critical! Surface Dimention field in Record 3 provided, but it is empty!")
      
    } else {
      tmp <-xmlChildren(rec3$children$MatrixDimension)
      surface.dim.info <- lapply(1:length(tmp), function(x){ xmlValue(tmp[[x]]) })
      names(surface.dim.info) <- names(tmp)
      #print(surface.dim.info)
      
    }
    
  } else {
    stop("Critical! No Surface Dimension field in Record 3!")
  }
  
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
    #surface3 <- (1e6) * (as.numeric(zaxis.info$Offset) + surface3*as.numeric(zaxis.info$Increment))
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
  #surface3 <- (1e6) * (as.numeric(zaxis.info$Offset) + surface3*as.numeric(zaxis.info$Increment))
  
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