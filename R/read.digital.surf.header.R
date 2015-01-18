#--------------------------------------------
#' @title Read the header off a Digital Surf file.
#'
#' @description pt = pointer returned by calling file on a digital surf .sur or 
#' .pro binary format file
#' 
#' @details **SEE FORMATS SECTION OF HELP IN MOUNTAINS SOFTWARE FOR 
#' INFORMATION ON THE .sur and .pro formats. Also see code for \code{read.digital.surf.file}.
#'
#' @param pt pointer returned by calling \code{read.digital.surf.file}
#' 
#' @return a list.
#' 
#' @references http://open-gps.sourceforge.net/
#--------------------------------------------
read.digital.surf.header<-function(pt)
{

#1
seek(pt, where = 0, rw="r")
code<-readBin(pt, what=character(), size = 12, n = 1, signed = FALSE, endian = "little")

#2
seek(pt, where = 12, rw="r")
format<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#3
seek(pt, where = 14, rw="r")
num.obj<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#4
seek(pt, where = 16, rw="r")
ver.num<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#5
seek(pt, where = 18, rw="r")
stud.typ<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#6********** 
seek(pt, where = 20, rw="r")
name.obj<-readBin(pt, what=character(), size = 30, n = 1, signed = FALSE, endian = "little")
name.obj<-strsplit(name.obj, " ")[[1]]
name.obj<-paste(name.obj[-c(which(name.obj==""),length(name.obj))],collapse="")
#print("name.obj")
#print(name.obj)
#print("")

#7**********
seek(pt, where = 50, rw="r")
name.op<-readBin(pt, what=character(), size = 30, n = 1, signed = FALSE, endian = "little")
#print("name.op")
#print(name.op)
#print("")

#8
seek(pt, where = 80, rw="r")
unused.int1<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#9
seek(pt, where = 82, rw="r")
unused.int2<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#10
seek(pt, where = 84, rw="r")
unused.int3<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#11
seek(pt, where = 86, rw="r")
non.meas.pts.flg<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#12
seek(pt, where = 88, rw="r")
abs.z.flg<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#13************
#This should be a raw byte sequence but is not reading in as such
#Will treat as an 8 byte integer
seek(pt, where = 90, rw="r")
reserv<-readBin(pt, what=integer(), size = 8, n = 1, signed = TRUE, endian = "little")
#print("reserv:")
#print(reserv)
#print("")

#14 Number of bits per point
seek(pt, where = 98, rw="r")
num.bits.pt<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")
if(num.bits.pt==32)
 {
  print("WARNING. 32-bit FILE INDICATED!")  
 }
#print(num.bits.pt.a)
#num.bits.pt<-16 #CAUTION!!!!! PATCH, BECAUSE MOUNTAINS SOMETIMES SAVING AS 32!!!!!!!!!!! Why???????

#15
seek(pt, where = 100, rw="r")
min.z<-readBin(pt, what=integer(), size = 4, n = 1, signed = TRUE, endian = "little")

#16
seek(pt, where = 104, rw="r")
max.z<-readBin(pt, what=integer(), size = 4, n = 1, signed = TRUE, endian = "little")

#17 NUMBER OF POINTS PER PROFILE
seek(pt, where = 108, rw="r")
num.pts.line<-readBin(pt, what=integer(), size = 4, n = 1, signed = TRUE, endian = "little")

#18 NUMBER OF PROFILES IN SURFACE
seek(pt, where = 112, rw="r")
num.lines<-readBin(pt, what=integer(), size = 4, n = 1, signed = TRUE, endian = "little")

#19
seek(pt, where = 116, rw="r")
num.pts.total<-readBin(pt, what=integer(), size = 4, n = 1, signed = TRUE, endian = "little")

#20
seek(pt, where = 120, rw="r")
x.inc<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#21
seek(pt, where = 124, rw="r")
y.inc<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#22
seek(pt, where = 128, rw="r")
z.inc<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#23 **************
seek(pt, where = 132, rw="r")
x.label<-readBin(pt, what=character(), size = 16, n = 1, signed = FALSE, endian = "little")
x.label<-strsplit(x.label, " ")[[1]][1]
#print("x.label:")
#print(x.label)
#print("")

#24 ****************
seek(pt, where = 148, rw="r")
y.label<-readBin(pt, what=character(), size = 16, n = 1, signed = FALSE, endian = "little")
y.label<-strsplit(y.label, " ")[[1]][1]
#print("y.label:")
#print(y.label)
#print("")

#25 *****************
seek(pt, where = 164, rw="r")
z.label<-readBin(pt, what=character(), size = 16, n = 1, signed = FALSE, endian = "little")
z.label<-strsplit(z.label, " ")[[1]][1]
#print("z.label")
#print(z.label)
#print("")

#26
seek(pt, where = 180, rw="r")
x.unit<-readBin(pt, what=character(), size = 16, n = 1, signed = FALSE, endian = "little")
x.unit<-strsplit(x.unit, " ")[[1]][1]

#27
seek(pt, where = 196, rw="r")
y.unit<-readBin(pt, what=character(), size = 16, n = 1, signed = FALSE, endian = "little")
y.unit<-strsplit(y.unit, " ")[[1]][1]

#28
seek(pt, where = 212, rw="r")
z.unit<-readBin(pt, what=character(), size = 16, n = 1, signed = FALSE, endian = "little")
z.unit<-strsplit(z.unit, " ")[[1]][1]

#29 ************
seek(pt, where = 228, rw="r")
x.len.unit<-readBin(pt, what=character(), size = 16, n = 1, signed = FALSE, endian = "little")
x.len.unit<-strsplit(x.len.unit, " ")[[1]][1]
#print("x.len.unit:")
#print(x.len.unit)
#print("")

#30 ********
seek(pt, where = 244, rw="r")
y.len.unit<-readBin(pt, what=character(), size = 16, n = 1, signed = FALSE, endian = "little")
y.len.unit<-strsplit(y.len.unit, " ")[[1]][1]
#print("y.len.unit")
#print(y.len.unit)
#print("")

#31 ***********
seek(pt, where = 260, rw="r")
z.len.unit<-readBin(pt, what=character(), size = 16, n = 1, signed = FALSE, endian = "little")
z.len.unit<-strsplit(z.len.unit, " ")[[1]][1]
#print("z.len.unit")
#print(z.len.unit)
#print("")

#32
seek(pt, where = 276, rw="r")
x.ratio<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#33
seek(pt, where = 280, rw="r")
y.ratio<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#34
seek(pt, where = 284, rw="r")
z.ratio<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#35
seek(pt, where = 288, rw="r")
replica<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#36
seek(pt, where = 290, rw="r")
inverted<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#37
seek(pt, where = 292, rw="r")
leveled<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#38
seek(pt, where = 294, rw="r")
unused.sing1<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#39
seek(pt, where = 298, rw="r")
unused.sing2<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#40
seek(pt, where = 302, rw="r")
unused.sing3<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#41
seek(pt, where = 306, rw="r")
meas.time.sec<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#42
seek(pt, where = 308, rw="r")
meas.time.min<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#43
seek(pt, where = 310, rw="r")
meas.time.hr<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#44
seek(pt, where = 312, rw="r")
meas.time.day<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#45
seek(pt, where = 314, rw="r")
meas.time.month<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#46
seek(pt, where = 316, rw="r")
meas.time.yr<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#47
seek(pt, where = 318, rw="r")
meas.time.wkday<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#48
seek(pt, where = 320, rw="r")
meas.dur<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#49
seek(pt, where = 324, rw="r")
unused.int4<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#50
seek(pt, where = 326, rw="r")
unused.int5<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#51
seek(pt, where = 328, rw="r")
unused.sing4<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#52
seek(pt, where = 332, rw="r")
unused.int6<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#53
seek(pt, where = 334, rw="r")
leng.comment.zone<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#54
seek(pt, where = 336, rw="r")
leng.private.zone<-readBin(pt, what=integer(), size = 2, n = 1, signed = TRUE, endian = "little")

#55 *****************
#This probably should be a 128 byte raw as well, but will be treated as a 128 char
#Only type that seems to be able to be this big
seek(pt, where = 338, rw="r")
free.zone<-readBin(pt, what=character(), size = 128, n = 1, signed = FALSE, endian = "little")
#print("free.zone")
#print(free.zone)
#print("")

#56
seek(pt, where = 460, rw="r")
x.off<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#57
seek(pt, where = 464, rw="r")
y.off<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#58
seek(pt, where = 468, rw="r")
z.off<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#59
seek(pt, where = 472, rw="r")
TT.spacing<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#60 NOTE: documented same as above. Weird.
seek(pt, where = 472, rw="r")
TT.off<-readBin(pt, what=single(), size = 4, n = 1, signed = TRUE, endian = "little")

#61 **********
seek(pt, where = 476, rw="r")
TT.name<-readBin(pt, what=character(), size = 13, n = 1, signed = FALSE, endian = "little")
#print("T.name")
#print(T.name)
#print("")

#62 ***********
seek(pt, where = 489, rw="r")
TT.step.unit<-readBin(pt, what=character(), size = 13, n = 1, signed = FALSE, endian = "little")
#print("T.step.unit")
#print(T.step.unit)
#print("")

#63 *********
#Comment zone. Assume a char for now.......
seek(pt, where = 489+13, rw="r")
comment.zone<-readBin(pt, what=character(), size = leng.comment.zone, n = 1, signed = FALSE, endian = "little")
#print("comment.zone:")
#print(comment.zone)
#print("")

#64 ************
#Private zone. Also assume a char for now.......
seek(pt, where = 489+13+leng.comment.zone, rw="r")
private.zone<-readBin(pt, what=character(), size = leng.private.zone, n = 1, signed = FALSE, endian = "little")
#print("private.zone")
#print(private.zone)
#print("")

#Short "essential" version:
#header.info<-list(name.obj, stud.typ,num.obj, num.bits.pt, num.pts.line, 
#                  num.lines, num.pts.total, max.z, min.z, x.inc, 
#                  y.inc, z.inc, x.off, y.off, 
#                  z.off, x.unit, y.unit, z.unit, x.len.unit, 
#                  y.len.unit, z.len.unit, leng.comment.zone, 
#                  leng.private.zone)
#names(header.info)<-c("name.obj","stud.typ","num.obj","num.bits.pt", 
#                      "num.pts.line", "num.lines", "num.pts.total", 
#                      "max.z", "min.z", "x.inc", "y.inc", "z.inc", 
#                      "x.off", "y.off", "z.off", "x.unit", "y.unit", 
#                      "z.unit", "x.len.unit", "y.len.unit", 
#                      "z.len.unit", "leng.comment.zone",
#                      "leng.private.zone")

#Long "total info" version:
header.info<-list(code,format,num.obj,ver.num,stud.typ,name.obj,
       name.op,unused.int1,unused.int2,unused.int3,
       non.meas.pts.flg,abs.z.flg,reserv,num.bits.pt,min.z,
       max.z,num.pts.line,num.lines,num.pts.total,x.inc,y.inc,
       z.inc,x.label,y.label,z.label,x.unit,y.unit,z.unit,
       x.len.unit,y.len.unit,z.len.unit,x.ratio,y.ratio,z.ratio,
       replica,inverted,leveled,unused.sing1,unused.sing2,
       unused.sing3,meas.time.sec,meas.time.min,meas.time.hr,
       meas.time.day,meas.time.month,meas.time.yr,meas.time.wkday,
       meas.dur,unused.int4,unused.int5,unused.sing4,unused.int6,
       leng.comment.zone,leng.private.zone,free.zone,x.off,y.off,
       z.off,TT.spacing,TT.off,TT.name,TT.step.unit,comment.zone,
       private.zone)

names(header.info)<-c("code","format","num.obj","ver.num",
     "stud.typ","name.obj",
     "name.op","unused.int1","unused.int2","unused.int3",
     "non.meas.pts.flg","abs.z.flg","reserv","num.bits.pt",
     "min.z","max.z","num.pts.line","num.lines",
     "num.pts.total","x.inc","y.inc","z.inc","x.label",
     "y.label","z.label","x.unit","y.unit","z.unit",
     "x.len.unit","y.len.unit","z.len.unit","x.ratio",
     "y.ratio","z.ratio","replica","inverted","leveled",
     "unused.sing1","unused.sing2","unused.sing3",
     "meas.time.sec","meas.time.min","meas.time.hr",
     "meas.time.day","meas.time.month","meas.time.yr",
     "meas.time.wkday","meas.dur","unused.int4",
     "unused.int5","unused.sing4","unused.int6",
     "leng.comment.zone","leng.private.zone","free.zone",
     "x.off","y.off","z.off","T.spacing","T.off",
     "T.name","T.step.unit","comment.zone","private.zone")
                      
return(header.info)
	
}