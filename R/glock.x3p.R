#' @title 3D surface of a 9-mm Glock firing pin aperture shear in X3P format.
#'
#' @format A zipped directory-file. It consists of a header file (main.xml), 
#' the surface data in binary (bindata/data.bin) and an md5 checksum (md5checksum.hex)
#' 
#' @source \url{http://NIST.DATABASE.XXXXXXXXX}
#' @source \url{http://www.opengps.eu/2008/ISO5436_2} 
#' @source \url{http://www.opengps.eu/2008/ISO5436_2/ISO5436_2.xsd}
#' 
#' @examples
#' file.path <- system.file("extdata", "glock.x3p", package="x3pr")
#' glock.x3p.info <- read.x3p(file.path)
#' glock.x3p.info[[1]]
#' 
#' @name glock.x3p
NULL