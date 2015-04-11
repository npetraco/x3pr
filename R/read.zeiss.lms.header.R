#--------------------------------------------
# @title Read a header of surface file in Zeiss LMS format.
#
# @description Read header ofsurface file in Zeiss LMS format.
# 
# @details Read header of surface file in Zeiss LMS format.
#
# @param pt R file pointer to the surface file
# @return NOTING
#
# @examples
# Coming soon.
#
# \dontrun{
#  
# }
#--------------------------------------------
read.zeiss.lms.header<-function(pt)
{
 
  #stop("NOT WORKING")
  
  #1
  seek(pt, where = 0, rw="r")
  file.size<-readBin(pt, what="integer", n = 1, signed = T, endian = "little")
  #print(file.size)
  
  #2
  seek(pt, where = 4, rw="r")
  file.type<-readBin(pt, what=character(), size = 6, n = 1, signed = FALSE, endian = "little")
  #print(file.type)
  
  #3 
  seek(pt, where = 10, rw="r")
  file.ver<-readBin(pt, what=integer(), size=2, n = 1, signed = T, endian = "little")
  #print(as.integer(file.ver))
  
  #4
  seek(pt, where = 12, rw="r")
  time.stamp<-readBin(pt, what="integer", n = 1, signed = T, endian = "little")
  #print(time.stamp)

  #5
  seek(pt, where = 16, rw="r")
  file.format<-readBin(pt, what=integer(), size=2, n = 1, signed = T, endian = "little")
  if(file.format==0){
    file.format <- "REAL Image"
  } else if(file.format==1){
    file.format <- "F Image"
  } else if(file.format==2){
    file.format <- "Z Image"
  } else if(file.format==3){
    file.format <- "F/Z Image"
  } else if(file.format==4){
    file.format <- "XZcross section image"
  } else if(file.format==5){
    file.format <- "Color image (non confocal)"
  } 
  #print(file.format)
  
  #6
  seek(pt, where = 18, rw="r")
  lens.mag<-readBin(pt, what=integer(), size=2, n = 1, signed = T, endian = "little")
  #print(lens.mag)
  
  #7
  seek(pt, where = 20, rw="r")
  extended.data<-readBin(pt, what="raw", size=1, n = 4, signed = T, endian = "little")
  #print(extended.data)
  
  #8
  seek(pt, where = 24, rw="r")
  width.unit<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  if(width.unit==0){
    width.unit <- "no setting"
  } else if(width.unit==1){
    width.unit <- "nm"
  } else if(width.unit==2){
    width.unit <- "um"
  } else if(width.unit==3){
    width.unit <- "mm"
  }
  #print(width.unit)
  
  #9
  seek(pt, where = 28, rw="r")
  height.unit<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  if(height.unit==0){
    height.unit <- "no setting"
  } else if(height.unit==1){
    height.unit <- "nm"
  } else if(height.unit==2){
    height.unit <- "um"
  } else if(height.unit==3){
    height.unit <- "mm"
  }
  #print(height.unit)
  
  #10 x/y inc in #8 units
  seek(pt, where = 32, rw="r")
  width.inc<-readBin(pt, what=double(), n = 1, signed = T, endian = "little")
  #print(width.inc)
  
  #11 z inc in #9 units
  seek(pt, where = 40, rw="r")
  height.inc<-readBin(pt, what=double(), n = 1, signed = T, endian = "little")
  #print(height.inc)
  
  #12
  seek(pt, where = 48, rw="r")
  width.comment<-readBin(pt, what="raw", size=1, n = 64, signed = T, endian = "little")
  #print(width.comment)
  
  #13
  seek(pt, where = 112, rw="r")
  height.comment<-readBin(pt, what="raw", size=1, n = 64, signed = T, endian = "little")
  #print(height.comment)
  
  #14
  seek(pt, where = 176, rw="r")
  extended.data2<-readBin(pt, what="raw", size=1, n = 12, signed = T, endian = "little")
  #print(extended.data2)
  
  #15
  seek(pt, where = 188, rw="r")
  extended.data3<-readBin(pt, what="raw", size=1, n = 4, signed = T, endian = "little")
  #print(extended.data3)
  
  #16
  seek(pt, where = 192, rw="r")
  image.pos.x<-readBin(pt, what="numeric", size=4, n = 1, signed = T, endian = "little")
  #image.pos.x<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(image.pos.x)
  
  #17
  seek(pt, where = 196, rw="r")
  image.pos.y<-readBin(pt, what="numeric", size=4, n = 1, signed = T, endian = "little")
  #image.pos.x<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(image.pos.y)
  
  #18
  seek(pt, where = 200, rw="r")
  stage.flag<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(stage.flag)
  
  #19
  seek(pt, where = 204, rw="r")
  extended.data4<-readBin(pt, what="raw", size=1, n = 8, signed = T, endian = "little")
  #print(extended.data4)
  
  #20
  seek(pt, where = 212, rw="r")
  model.info<-readBin(pt, what="raw", size=1, n = 1, signed = T, endian = "little")
  #print(model.info)
  
  #21
  seek(pt, where = 213, rw="r")
  color.info<-readBin(pt, what="raw", size=1, n = 1, signed = T, endian = "little")
  #print(color.info)
  
  #22
  seek(pt, where = 214, rw="r")
  reserved<-readBin(pt, what="raw", size=1, n = 26, signed = T, endian = "little")
  #print(reserved)
  
  #23
  seek(pt, where = 240, rw="r")
  extended.data5<-readBin(pt, what="raw", size=1, n = 80, signed = T, endian = "little")
  #print(extended.data5)
  
  #24
  seek(pt, where = 320, rw="r")
  extended.data6<-readBin(pt, what="raw", size=1, n = 4, signed = T, endian = "little")
  #print(extended.data6)
  
  #25
  seek(pt, where = 324, rw="r")
  num.img<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(num.img)
  
  #26
  seek(pt, where = 328, rw="r")
  act.img<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(act.img)
  
  #27
  seek(pt, where = 332, rw="r")
  img1.style<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(img1.style)
  
  #27
  seek(pt, where = 336, rw="r")
  img2.style<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(img2.style)
  
  #28
  seek(pt, where = 340, rw="r")
  reserved2<-readBin(pt, what="raw", size=1, n = 12, signed = T, endian = "little")
  #print(reserved2)
  
  #DIB SECTION 1
  #29
  seek(pt, where = 352, rw="r")
  bftype1<-readBin(pt, what=character(), size=2, n = 1, signed = T, endian = "little")
  #print(bftype1)
  
  #30
  seek(pt, where = 354, rw="r")
  bfsize1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bfsize1)
  
  #31
  seek(pt, where = 358, rw="r")
  bfreserved11<-readBin(pt, what=integer(), size = 2, n = 1, signed = T, endian = "little")
  #print(bfreserved11)
  
  #32
  seek(pt, where = 360, rw="r")
  bfreserved12<-readBin(pt, what=integer(), size = 2, n = 1, signed = T, endian = "little")
  #print(bfreserved12)
  
  #33*
  seek(pt, where = 362, rw="r")
  bfoffbits1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bfoffbits1)
  
  #34
  seek(pt, where = 366, rw="r")
  bisize1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bisize1)
  
  #35*
  seek(pt, where = 370, rw="r")
  biwidth1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(biwidth1)
  
  #36*
  seek(pt, where = 374, rw="r")
  biheight1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(biheight1)
  
  #37
  seek(pt, where = 378, rw="r")
  biplanes1<-readBin(pt, what=integer(), size=2, n = 1, signed = T, endian = "little")
  #print(biplanes1)
  
  #38*
  seek(pt, where = 380, rw="r")
  bibitcount1<-readBin(pt, what=integer(), size=2, n = 1, signed = T, endian = "little")
  #print(bibitcount1)
  
  #39
  seek(pt, where = 382, rw="r")
  bicompress1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bicompress1)
  
  #40*
  seek(pt, where = 386, rw="r")
  bisizeimg1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bisizeimg1)
  
  #41
  seek(pt, where = 390, rw="r")
  bixppm1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bixppm1)
  
  #42
  seek(pt, where = 394, rw="r")
  biyppm1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(biyppm1)
  
  #43
  seek(pt, where = 398, rw="r")
  bicolorused1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bicolorused1)
  
  #44
  seek(pt, where = 402, rw="r")
  bicolorimportant1<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bicolorimportant1)
  
  offset.to.img2 <- 406+bisizeimg1
  
  #DIB SECTION 2
  #45
  seek(pt, where = offset.to.img2, rw="r")
  bftype2<-readBin(pt, what=character(), size=2, n = 1, signed = T, endian = "little")
  #print(bftype2)
  
  #46
  seek(pt, where = offset.to.img2+2, rw="r")
  bfsize2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bfsize2)
  
  #47
  seek(pt, where = offset.to.img2+6, rw="r")
  bfreserved21<-readBin(pt, what=integer(), size = 2, n = 1, signed = T, endian = "little")
  #print(bfreserved21)
  
  #48
  seek(pt, where = offset.to.img2+8, rw="r")
  bfreserved22<-readBin(pt, what=integer(), size = 2, n = 1, signed = T, endian = "little")
  #print(bfreserved22)
  
  #49*
  seek(pt, where = offset.to.img2+10, rw="r")
  bfoffbits2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bfoffbits2)
  
  #50
  seek(pt, where = offset.to.img2+14, rw="r")
  bisize2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bisize2)
  
  #51*
  seek(pt, where = offset.to.img2+18, rw="r")
  biwidth2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(biwidth2)
  
  #52*
  seek(pt, where = offset.to.img2+22, rw="r")
  biheight2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(biheight2)
  
  #53
  seek(pt, where = offset.to.img2+26, rw="r")
  biplanes2<-readBin(pt, what=integer(), size=2, n = 1, signed = T, endian = "little")
  #print(biplanes2)
  
  #54*
  seek(pt, where = offset.to.img2+28, rw="r")
  bibitcount2<-readBin(pt, what=integer(), size=2, n = 1, signed = T, endian = "little")
  #print(bibitcount2)
  
  #55
  seek(pt, where = offset.to.img2+30, rw="r")
  bicompress2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bicompress2)
  
  #56*
  seek(pt, where = offset.to.img2+34, rw="r")
  bisizeimg2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bisizeimg2)
  
  #57
  seek(pt, where = offset.to.img2+38, rw="r")
  bixppm2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bixppm2)
  
  #58
  seek(pt, where = offset.to.img2+42, rw="r")
  biyppm2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(biyppm2)
  
  #59
  seek(pt, where = offset.to.img2+46, rw="r")
  bicolorused2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bicolorused2)
  
  #60
  seek(pt, where = offset.to.img2+50, rw="r")
  bicolorimportant2<-readBin(pt, what=integer(), n = 1, signed = T, endian = "little")
  #print(bicolorimportant2)
  
  #print(offset.to.img2+bfoffbits2)
  
  offset.to.img2 <- 406+bisizeimg1
  
  #*************For compatibility of many functions from profiles:
  if(img1.style == 2) {
    
    num.pts.line <- biwidth1
    num.lines <- biheight1
    x.inc <- width.inc
    y.inc <- width.inc
    z.inc <- height.inc    
    
  } else if(img2.style == 2) {
    
    num.pts.line <- biwidth2
    num.lines <- biheight2
    x.inc <- width.inc
    y.inc <- width.inc
    z.inc <- height.inc
    
  } else {
    stop("Problem determing which image is the z-height data!!!!!!")
  }
  
  header.info<-list(
    file.size,
    file.type,
    file.ver,
    time.stamp,
    file.format,
    lens.mag,
    extended.data,
    width.unit,
    height.unit,
    width.inc,
    height.inc,
    width.comment,
    height.comment,
    extended.data2,
    extended.data3,
    image.pos.x,
    image.pos.y,
    stage.flag,
    extended.data4,
    model.info,
    color.info,
    reserved,
    extended.data5,
    extended.data6,
    num.img,
    act.img,
    img1.style,
    img2.style,
    reserved2,
    bftype1,
    bfsize1,
    bfreserved11,
    bfreserved12,
    bfoffbits1,
    bisize1,
    biwidth1,
    biheight1,
    biplanes1,
    bibitcount1,
    bicompress1,
    bisizeimg1,
    bixppm1,
    biyppm1,
    bicolorused1,
    bicolorimportant1,
    offset.to.img2,
    bftype2,
    bfsize2,
    bfreserved21,
    bfreserved22,
    bfoffbits2,
    bisize2,
    biwidth2,
    biheight2,
    biplanes2,
    bibitcount2,
    bicompress2,
    bisizeimg2,
    bixppm2,
    biyppm2,
    bicolorused2,
    bicolorimportant2,
    num.pts.line, #These are for compatability with old profiles codes
    num.lines,
    x.inc,
    y.inc,
    z.inc
  )
  
  names(header.info)<-c(
    "file.size",
    "file.type",
    "file.ver",
    "time.stamp",
    "file.format",
    "lens.mag",
    "extended.data",
    "width.unit",
    "height.unit",
    "width.inc",
    "height.inc",
    "width.comment",
    "height.comment",
    "extended.data2",
    "extended.data3",
    "image.pos.x",
    "image.pos.y",
    "stage.flag",
    "extended.data4",
    "model.info",
    "color.info",
    "reserved",
    "extended.data5",
    "extended.data6",
    "num.img",
    "act.img",
    "img1.style",
    "img2.style",
    "reserved2",
    "bftype1",
    "bfsize1",
    "bfreserved11",
    "bfreserved12",
    "bfoffbits1",
    "bisize1",
    "biwidth1",
    "biheight1",
    "biplanes1",
    "bibitcount1",
    "bicompress1",
    "bisizeimg1",
    "bixppm1",
    "biyppm1",
    "bicolorused1",
    "bicolorimportant1",
    "offset.to.img2",
    "bftype2",
    "bfsize2",
    "bfreserved21",
    "bfreserved22",
    "bfoffbits2",
    "bisize2",
    "biwidth2",
    "biheight2",
    "biplanes2",
    "bibitcount2",
    "bicompress2",
    "bisizeimg2",
    "bixppm2",
    "biyppm2",
    "bicolorused2",
    "bicolorimportant2",
    "num.pts.line", #These are for compatability with old profiles codes
    "num.lines",
    "x.inc",
    "y.inc",
    "z.inc"
    )

  return(header.info)
}