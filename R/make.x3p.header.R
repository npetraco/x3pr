#--------------------------------------------
#' @title Put together the XML format x3p header following the ISO 25178 and ISO 5436-2 standards. 
#'
#' @description Put together the XML format x3p header, main.xml, following the ISO 25178 and ISO 5436-2 standards.
#' 
#' @details Put together the XML format x3p header following the ISO 25178 and ISO 5436-2 standards. The 
#' arguements to this function are the obligatory fields that must appear in the x3p header
#' according to the ISO 5436-2 standard.
#'
#' @param surf.type
#' @param x.axis.type 
#' @param x.data.type 
#' @param xinc 
#' @param x.offset 
#' @param y.axis.type 
#' @param y.data.type 
#' @param yinc 
#' @param y.offset 
#' @param z.axis.type 
#' @param z.data.type 
#' @param zinc 
#' @param z.offset 
#' @param meas.dte 
#' @param who.wrote.file
#' @param manufacturer 
#' @param model 
#' @param sn 
#' @param vers 
#' @param cal.dte 
#' @param probe.type 
#' @param probe.id 
#' @param meas.comment 
#' @param pts.per.prof 
#' @param num.prof 
#' @param z.format
#' @param data.bin.md5sumcheck
#' @return An object of class \code{XMLNode}
#' 
#' @references http://open-gps.sourceforge.net/
#'
#--------------------------------------------
make.x3p.header<-function(
  surf.type, 
  x.axis.type, x.data.type, xinc, x.offset,
  y.axis.type, y.data.type, yinc, y.offset,
  z.axis.type, z.data.type, zinc, z.offset,                          
  meas.dte, who.wrote.file,
  manufacturer, model, sn, vers, cal.dte, probe.type, probe.id,
  meas.comment, 
  pts.per.prof, num.prof, z.format, data.bin.md5sumcheck)
{
  
  a <- xmlNode("p:ISO5436_2")
  xmlAttrs(a) = c("xmlns:p"="http://www.opengps.eu/2008/ISO5436_2")
  xmlAttrs(a) = c("xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance") 
  xmlAttrs(a) = c("xsi:schemaLocation"="http://www.opengps.eu/2008/ISO5436_2 http://www.opengps.eu/2008/ISO5436_2/ISO5436_2.xsd")
  
  #Record1
  a$children[[1]] <- xmlNode("Record1",
                             a$children$Record1 <- xmlNode("Revision","ISO5436 - 2000"),
                             a$children$Record1 <- xmlNode("FeatureType",surf.type),
                             a$children$Record1 <- xmlNode("Axes",
                                                           xmlNode("CX",
                                                                   xmlNode("AxisType",x.axis.type),     #"I"
                                                                   xmlNode("DataType",x.data.type),     #"F"
                                                                   xmlNode("Increment",xinc),
                                                                   xmlNode("Offset",x.offset)           #0.0 probably
                                                           ),
                                                           xmlNode("CY",
                                                                   xmlNode("AxisType",y.axis.type),     #"I"
                                                                   xmlNode("DataType",y.data.type),     #"F"
                                                                   xmlNode("Increment",yinc),
                                                                   xmlNode("Offset",y.offset)           #0.0 probably
                                                           ),
                                                           xmlNode("CZ",
                                                                   xmlNode("AxisType",z.axis.type),    #"A"
                                                                   xmlNode("DataType",z.data.type),    #"F"
                                                                   xmlNode("Increment",zinc),
                                                                   xmlNode("Offset",z.offset)       #0.0 probably
                                                           )
                             )                          
  )
  
  #Record 2
  a$children[[2]] <- xmlNode("Record2",
                             a$children$Record2 <- xmlNode("Date",meas.dte),
                             a$children$Record2 <- xmlNode("Creator",who.wrote.file),
                             a$children$Record2 <- xmlNode("Instrument",
                                                           xmlNode("Manufacturer", manufacturer),     #"Carl Zeiss AG"
                                                           xmlNode("Model", model),                   #CSM-700
                                                           xmlNode("Serial",sn),                      #FUNCTION INPUT
                                                           xmlNode("Version", vers)                   #FUNCTION INPUT
                             ),
                             a$children$Record2 <- xmlNode("CalibrationDate",cal.dte),
                             a$children$Record2 <- xmlNode("ProbingSystem",
                                                           xmlNode("Type",probe.type),               #"Non Contacting"
                                                           xmlNode("Identification",probe.id)       #"White light confocal"
                             ),
                             a$children$Record2 <- xmlNode("Comment",meas.comment)
  )
  
  #Record 3
  a$children[[3]] <- xmlNode("Record3",
                             a$children$Record3 <- xmlNode("MatrixDimension",
                                                           xmlNode("SizeX", pts.per.prof), 
                                                           xmlNode("SizeY", num.prof),     
                                                           xmlNode("SizeZ", z.format)             #1
                             ),
                             #a$children$Record3 <- xmlNode("PointDataLink","bindata/data.bin"),
                             a$children$Record3 <- xmlNode("DataLink",
                                                           xmlNode("PointDataLink","bindata/data.bin"),
                                                           xmlNode("MD5ChecksumPointData",data.bin.md5sumcheck)      
                                                           #ADD VALID POINTS FILE AT SOME POINT
                             )
  )
  
  #Record 4
  a$children[[4]] <- xmlNode("Record4",
                             a$children$Record4 <- xmlNode("ChecksumFile","md5checksum.hex")
  )
  
  return(a)
  
}