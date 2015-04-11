#' @title 3D surface of land engraved area on a 9mm bullet in Zeiss LMS format.
#'
#' @description A binary file. It is essentially a Windows Bitmap format file except that it 
#' contains two images. The first image is an RGB color image (i.e. a "texture") of the surface.
#' The second image contains the z-height data in row-major order. 
#'
#' @format A BITMAPFILEHEADER, the DIB header of the first image, image data for the first image 
#' (row major order), DIB header for the second image and image data for the second image 
#' (row major order). Info from the headers:
#' 
#' \describe{
#' BITMAPFILEHEADER
#'   \item{file.size}{}
#'   \item{file.type}{}
#'   \item{file.ver}{}
#'   \item{time.stamp}{}
#'   \item{file.format}{}
#'   \item{lens.mag}{}
#'   \item{extended.data}{}
#'   \item{width.unit}{}
#'   \item{height.unit}{}
#'   \item{width.inc}{}
#'   \item{height.inc}{}
#'   \item{width.comment}{}
#'   \item{height.comment}{}
#'   \item{extended.data2}{}
#'   \item{extended.data3}{}
#'   \item{image.pos.x}{}
#'   \item{image.pos.y}{}
#'   \item{stage.flag}{}
#'   \item{extended.data4}{}
#'   \item{model.info}{}
#'   \item{color.info}{}
#'   \item{reserved}{}
#'   \item{extended.data5}{}
#'   \item{extended.data6}{}
#'   \item{num.img}{}
#'   \item{act.img}{}
#'   \item{img1.style}{}
#'   \item{img2.style}{}
#'   \item{reserved2}{}
#'   
#' DIB header image 1
#'   \item{bftype1}{}
#'   \item{bfsize1}{}
#'   \item{bfreserved11}{}
#'   \item{bfreserved12}{}
#'   \item{bfoffbits1}{}
#'   \item{bisize1}{}
#'   \item{biwidth1}{}
#'   \item{biheight1}{}
#'   \item{biplanes1}{}
#'   \item{bibitcount1}{}
#'   \item{bicompress1}{}
#'   \item{bisizeimg1}{}
#'   \item{bixppm1}{}
#'   \item{biyppm1}{}
#'   \item{bicolorused1}{}
#'   \item{bicolorimportant1}{}
#'   \item{offset.to.img2}{}
#'
#' DIB header image 1   
#'   \item{bftype2}{}
#'   \item{bfsize2}{}
#'   \item{bfreserved21}{}
#'   \item{bfreserved22}{}
#'   \item{bfoffbits2}{}
#'   \item{bisize2}{}
#'   \item{biwidth2}{}
#'   \item{biheight2}{}
#'   \item{biplanes2}{}
#'   \item{bibitcount2}{}
#'   \item{bicompress2}{}
#'   \item{bisizeimg2}{}
#'   \item{bixppm2}{}
#'   \item{biyppm2}{}
#'   \item{bicolorused2}{}
#'   \item{bicolorimportant2}{}
#' }
#' 
#' @source \url{http://NIST.DATABASE.XXXXXXXXX}
#' @source \url{http://en.wikipedia.org/wiki/BMP_file_format}
#' 
#' @examples
#' file.path <- system.file("extdata", "lea.lms", package="x3pr")
#' glock.lms.surface.info <- read.zeiss.lms.file(file.path)
#' glock.lms.surface.info[[1]]
#' 
#' @name lea.lms
NULL