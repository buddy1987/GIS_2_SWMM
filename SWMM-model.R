# XAY DUNG MO HINH SWMM
# Y TUONG: chuan bi mang tinh tren gis va dung R tao cac phan tron file SWMM.inp 

library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(tidyr)
library(raster)
library(stringr)
library(readxl)
library(dplyr)

setwd("E:/Rdatabase/database/AutoMakeSWMM-model")

#PHAN I: CHUAN BI SO LIEU SHAPE FILE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## note: tao san cac shapefile: conduit, junction; pol_lv voi 1 doi tuong gia dinh de R co the doc duoc, sau bo di o buoc namming.

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Bước 1: THEM THUOC TINH CHO *.SHP &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#1: Xy ly luu vuc
##them cot thuoc tinh cho shapefile luu vuc
lv = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/plo_lv.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
d = dim(lv@data)
lv@data[,c(2:d[2])] = NULL

lv@data$name = "lv"
lv@data$rgage = "p20"   # so gia dinh 1000N la text
lv@data$outlet = "1000N"
lv@data$area = 50
lv@data$imperv = 65
lv@data$width = 200
lv@data$slope = 0.02
lv@data$curblen = 0
lv@data$snowpack = ""
lv@data$nimperv = 0.01
lv@data$nperv = 0.1
lv@data$simperv = 0.05
lv@data$sperv = 0.05
lv@data$pctzero = 25
lv@data$routeto = "OUTLET"
lv@data$pcrouted = ""
lv@data$maxrate = 3.0
lv@data$minrate = 0.5
lv@data$decay = 4
lv@data$drytime = 7
lv@data$maxinfil = 0
lv@data$Id = NULL

 ## ghi de len file lv co sang
writeOGR(lv, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='plo_lv', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


#2: Xy ly Junction
## them cot thuoc tinh cho shapefile luu vuc
jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
d = dim(jun@data)
jun@data[,c(2:d[2])] = NULL

jun@data$name = "jun"
jun@data$x = 548046.951
jun@data$y = 1148570.337
jun@data$elevation = 2
jun@data$maxdepth = 1.7
jun@data$InitDepth = 0.05
jun@data$SurDepth = 0
jun@data$Aponded = 2
jun@data$Id = NULL
jun@data = subset(jun@data,select= c("name","x","y","elevation","maxdepth","InitDepth","SurDepth","Aponded"))
## ghi de len file lv co sang
writeOGR(jun, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='junction', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


#3: Xy ly Outfall
## them cot thuoc tinh cho shapefile luu vuc
outf = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
d = dim(outf@data)
outf@data[,c(2:d[2])] = NULL
outf@data$name = "out"
outf@data$x = 548046.951
outf@data$y = 1148570.337
outf@data$elevation = 1.95
outf@data$Type = "TIDAL"
outf@data$StageData = "tide"
outf@data$Gated = "YES"
outf@data$RouteTo = ""
outf$Id = NULL
outf@data = subset(outf@data,select= c("name","x","y","elevation","Type","StageData","Gated","RouteTo"))
## ghi de len file lv co sang
writeOGR(outf, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'outfall', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


#4: Xy ly conduit
## them cot thuoc tinh cho shapefile luu vuc
con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/conduit.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
d = dim(con@data)
con@data[,c(2:d[2])] = NULL

con@data$name = "con"
#con@data$FromNode = "jun"
#con@data$ToNode = "jun"
con@data$length = 2000
con@data$Roughness = 0.01
con@data$InOffset = 2.1
con@data$OutOffset = 2.1
con@data$InitFlow = 1.6
con@data$MaxFlow  = 0
con@data$shape  = "CIRCULAR"
con@data$Geom1 = "1"
con@data$Geom2 = 0
con@data$Geom3 = 0
con@data$Geom4 = 0
con@data$Barrels  = 1
con@data$Culvert = ""
con@data$Id = NULL

## ghi de len file  co sang
writeOGR(con, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'conduit', driver = 'ESRI Shapefile',overwrite_layer= TRUE)



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Bước 3: BỔ SUNG THUỘC TÍNH CHO CÁC *.SHP &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

#1. Luu vuc: Dat ten, tinh dien tich, bo polygon dau tien du dong cho cac doi tuong
lv = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/plo_lv.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
lv@data$area = round(area(lv)/10000,digits = 2)            ## tinh dien tich polygon (ha)
## lv@data$id = c(seq(1,length(lv@data$name)))        ## truong hop van du pol_lv sample tu gis         
## lv = subset(lv, lv$id>1)
name = paste0("lv", seq(1,length(lv@data$area)))
lv@data$name = name
lv@data$id = NULL
writeOGR(lv, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='plo_lv', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


#2. conduit: Dat ten, cho conduit
con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/conduit.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
## con@data$id = c(seq(1,length(con@data$name)))                 
## con = subset(con, con$id>1)
name = paste0("con", seq(1,length(con@data$name)))
con@data$name = name
con@data$id = NULL
con@data$length = round(gLength(con,byid = TRUE), digits = 3)    ## tinh chieu dai cong
writeOGR(con, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='conduit', driver = 'ESRI Shapefile',overwrite_layer= TRUE)

#3. juntion: Dat ten, bo diem ban dau va ghi lai 
jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

## jun@data$id = c(seq(1,length(jun@data$name)))                 
## jun = subset(jun, jun$id>1)
name = paste0("jun", seq(1,length(jun@data$name)))
jun@data$name = name
jun@data$id = NULL
jun@data$x = coordinates(jun)[,1]   ## xac dinh toa do x
jun@data$y = coordinates(jun)[,2]   ## xac dinh toa do y
writeOGR(jun, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='junction', driver = 'ESRI Shapefile',overwrite_layer= TRUE)

#4. Outfall: Dat ten, bo diem ban dau va ghi lai 
out = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

## out@data$id = c(seq(1,length(out@data$name)))                 
## out = subset(out, out$id>1)
name = paste0("out", seq(1,length(out@data$name)))
out@data$name = name
out@data$id = NULL
out@data$x = coordinates(out)[,1]   ## xac dinh toa do x
out@data$y = coordinates(out)[,2]   ## xac dinh toa do y
writeOGR(out, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='outfall', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


#5 tạo Jun_outfall.shp để xác định điểm đầu và cuối cho conduit.shp

jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

outf = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

dfxy1 = jun@data[,c(1:3)]      ## tach phan name xy cua diem junction
dfxy2 = outf@data[,c(1:3)]     ## tach phan name xy cua diem outfall
dfxy = rbind(dfxy1,dfxy2)      ## gop lai tao thanh dfxy 

xy <- dfxy[,c(2,3)]
coords = xy
sp = SpatialPoints(coords)
spdf <- SpatialPointsDataFrame(sp,data = dfxy)

### gan toa do 
Utm48N = " +proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(spdf) = CRS(Utm48N)

### ghi ra shapefile
writeOGR(spdf, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'jun_outfall', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Bước 5: CẬP NHẬT THUỘC TÍNH CHHO CÁC*.SHP &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#1. sau khi join shape file fromto voi jun_outfall.shp se duoc file fromto_up.shp ta se update vao data cua conduit
## doc shapefile fromto da joined vao

frto = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/fromto_up.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
df_fto = frto@data
df_fto = subset(df_fto, select= c(OBJECTID,conduit_na,name))  ## loc lay cot id, name cua conduit va name cua jun_outfall
colnames(df_fto) = c("id","name","frto")
df_fto = df_fto[order(df_fto$id),]       ## sort cho dung thu tu de generating seq (0,1) voi 0 la diem dau va 1 diem cuoi
df_fto$dc = rep(c(0:1),times = (length(df_fto$id)/2))  ## tao cot co gia tri o va 1 de tach

df_from = subset(df_fto,df_fto$dc== 0)
df_from = df_from[,c(2,3)]           ## loc chi con hai cot name va fromto
colnames(df_from) = c("name","frompoint")

df_to = subset(df_fto,df_fto$dc== 1)
df_to = df_to[,c(2,3)]           ## loc chi con hai cot name va fromto
colnames(df_to) = c("name","topoint")

## doc shapefile conduit vao 
con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/conduit.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
d = dim(con@data)
con@data$id = seq(1,d[1])
con@data = merge(con@data,df_from, by = "name", all.x = TRUE)
con@data = merge(con@data,df_to, by = "name", all.x = TRUE)
con@data = con@data[order(con@data$id),]
con@data$id = NULL
writeOGR(con, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'conduit', driver = 'ESRI Shapefile',overwrite_layer= TRUE)



#2. gan huong vao cua thu nuoc tu catchment
## tu link.shp join voi junction.shp sau do join pol_lv.shp duoc flowdirect.shp
flowdirect = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/flowdirection.shp",
                              proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

lv = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/plo_lv.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

## tach bang thuoc tinh cua polygon y
dffldire = flowdirect@data
dffldire = subset(dffldire, select= c(name_1,name))
colnames(dffldire) = c("name","outlet")
dfc = lv@data[,c(1:2)] ## tach bang de merge() lay routes
dfc = merge(dfc,dffldire, by = "name", all.x=TRUE)
dfc$id = as.numeric(gsub(pattern = "lv","",dfc$name))
dfc = dfc[order(dfc$id),]
lv@data$outlet = dfc$outlet

writeOGR(lv, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'plo_lv', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


#3:Converting pol_lv.shp to pol_lv_point.shp
lv = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/plo_lv.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

lv_line = as(lv, "SpatialLines")    ## chuyen tu polygon to polyline without attribute
data = lv@data
lv_line = SpatialLinesDataFrame(lv_line, data, match.ID = TRUE)   ## gán bảng thuộc tính cho poline

writeOGR(lv_line, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'lv_line', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


### Công thực generating tự động các điểm khoảng cách 100 m từ line to poits: Stakeflow
sample.line <- function(x, sdist=5)
{
  if (!require(sp)) stop("sp PACKAGE MISSING")
  if (!inherits(x, "SpatialLinesDataFrame")) stop("MUST BE SP SpatialLinesDataFrame OBJECT")
  lgth <- SpatialLinesLengths(x) 
  lsub <- x[1,]
  ns <- round( (lgth[1] / sdist), digits=0)
  lsamp <- spsample(lsub, n=ns, type="regular", offset=c(0.5,0.5))
  data = x@data[1,]
  df = as.data.frame(lapply(data, rep, ns))
  results <- SpatialPointsDataFrame(lsamp, df, match.ID = TRUE)
  
  for (i in 2:dim(x)[1] ) 
  {    
    lsub <- x[i,]
    ns <- round( (lgth[i] / sdist), digits=0)
    lsamp <- spsample(lsub, n=ns, type="regular")
    data = x@data[i,]
    df = as.data.frame(lapply(data, rep, ns))
    lsamp <- SpatialPointsDataFrame(lsamp, df, match.ID = TRUE)
    results <- rbind(results, lsamp)     
  }
  ( results )
}

### tao thành điểm, chú ý khoảng cách các điểm ví dụ bên dưới là 50 m có một điểm ngẫu nhiên
lv_point <- sample.line(lv_line, sdist= 10) 
plot(lv_point)   
## xac dinh toa do cac diem cua lv_point
lv_point@data$x = coordinates(lv_point)[,1]
lv_point@data$y = coordinates(lv_point)[,2]
writeOGR(lv_point, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'plo_lv_point', driver = 'ESRI Shapefile',overwrite_layer= TRUE)



#[OPTIONAL] Thay tên đối tượng lv; jun, con, out .. vì trong 1 vùng nghiên cứu vd long xuyên sẽ phân thành
## nhiều model nhỏ nếu đặt tên là lv ko sẽ bị trùng khi phân tích ví dụ lv1 và lv2

## thay ten cho sub-catchment
lv = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/plo_lv.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
lv@data$name = gsub(pattern = "lv","LX23lv",lv@data$name)
lv@data$outlet = gsub(pattern = "jun","LX23J",lv@data$outlet)
writeOGR(lv, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'plo_lv', driver = 'ESRI Shapefile',overwrite_layer= TRUE)
### thay ten cho point of subcatchment
lvp = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/plo_lv_point.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
lvp@data$name = gsub(pattern = "lv","LX23lv",lvp@data$name)
writeOGR(lvp, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'plo_lv_point', driver = 'ESRI Shapefile',overwrite_layer= TRUE)

## thay ten cho CONDUIT
con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/conduit.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
con@data$name = gsub(pattern = "con","LX23C",con@data$name)
con@data$frompoint = gsub(pattern = "out","LX23O",con@data$frompoint)
con@data$frompoint = gsub(pattern = "jun","LX23J",con@data$frompoint)
con@data$topoint = gsub(pattern = "out","LX23O",con@data$topoint)
con@data$topoint = gsub(pattern = "jun","LX23J",con@data$topoint)
writeOGR(con, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'conduit', driver = 'ESRI Shapefile',overwrite_layer= TRUE)

## thay ten cho outfall
out = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
out@data$name = gsub(pattern = "out","LX23O",out@data$name)
writeOGR(out, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'outfall', driver = 'ESRI Shapefile',overwrite_layer= TRUE)

## thay ten cho junction
jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
jun@data$name = gsub(pattern = "jun","LX23J",jun@data$name)
writeOGR(jun, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'junction', driver = 'ESRI Shapefile',overwrite_layer= TRUE)



#+++++++++++++++++++++++++++++++++++++++++ TAO MODEL SWMM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#1. SCRIPT VIET CHO TẠO LƯU VỰC 

## import polygon luu vuc 
lv = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/plo_lv.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
lv_p = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/plo_lv_point.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

## tach bang thuoc tinh cua polygon 
df = lv@data
df_p = lv_p@data
dfcor = lv@bbox


## doc phan 3 cua catchment
part3 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part3_subcachment.txt")
## tach phan thuoc tinh cua part3 va ghi ra csv rui doc dang text vao de gop voi part3
df_3 = df[,c(1:9)]
write.table(df_3,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part3.csv", sep = ",",row.names = F)
df_3 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part3.csv")[-1]
part3 = c(part3,df_3)
part3 = gsub(pattern = ",", "     ", part3)
part3 = gsub(pattern = "NA", "     ", part3)
## doc phan 4 cua catchment
part4 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part4_subarea.txt")
df_4 = df[,c(1,10:16)]
write.table(df_4,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part4.csv", sep = ",",row.names = F)
df_4 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part4.csv")[-1]
part4 = c(part4,df_4)
part4 = gsub(pattern = ",", "     ", part4)

## doc phan 5 cua catchment
part5 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part5_subinfil.txt")
df_5 = df[,c(1,17:21)]
write.table(df_5,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part5.csv", sep = ",",row.names = F)
df_5 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part5.csv")[-1]
part5 = c(part5,df_5)
part5 = gsub(pattern = ",", "     ", part5)

## doc phan 19 cua catchment toa do cac diem 
part19 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part19_Polycor.txt")
df_p19 = df_p[,c(1,22:23)]
write.table(df_p19,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part19.csv", sep = ",",row.names = F)
df_19 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part19.csv")[-1]
part19 = c(part19,df_19)
part19 = gsub(pattern = ",", "     ", part19)

## doc phan 16 cua catchment dimention cua vung nghien cuu
part16 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part16_map.txt")
part16 = gsub(pattern = "AAAAA",dfcor[1,1],part16)
part16 = gsub(pattern = "BBBBB",dfcor[2,1],part16)
part16 = gsub(pattern = "CCCCC",dfcor[1,2],part16)
part16 = gsub(pattern = "DDDDD",dfcor[2,2],part16)



#2. SCRIPT VIET JUNCTION

## import JUNTION
jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

df_j = jun@data
jun@data[,c(1:2)] = NULL

## doc phan 6 cua catchment toa do cac diem 
part6 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part6_Jun.txt")
df_j6 = df_j[,c(1, c(4:8))]
write.table(df_j6,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part6.csv", sep = ",",row.names = F)
df_6 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part6.csv")[-1]
part6 = c(part6,df_6)
part6 = gsub(pattern = ",", "     ", part6)


#3. SCRIPT VIET OUTFALL VA TOA DO CUA JUNCTION VA OUTFALL

## import OUTFLALL
outf = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

df_o = outf@data

## doc phan 7 cua catchment toa do cac diem 
part7 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part7_Out.txt")
df_o7 = df_o[,c(1, c(4:8))]
write.table(df_o7,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part7.csv", sep = ",",row.names = F)
df_7 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part7.csv")[-1]
part7 = c(part7,df_7)
part7 = gsub(pattern = "NA", " ", part7)
part7 = gsub(pattern = ",", "     ", part7)


## doc phan 17 cua toa do cua junction va outfall
part17 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part17_nodeCordi.txt")
### doc toa do cua junction
df_j6 = df_j[,c(1:3)]
write.table(df_j6,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part6.csv", sep = ",",row.names = F)
df_6 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part6.csv")[-1]
### doc toa do cua outfall
df_o7 = df_o[,c(1:3)]
write.table(df_o7,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part7.csv", sep = ",",row.names = F)
df_7 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part7.csv")[-1]

part17 = c(part17,df_6,df_7)
part17 = gsub(pattern = ",", "     ", part17)


## doc phan 12 Chuan bị số liệu H từ phương án của VRSAP to SWMM

### y tuong: load outfall vao lay danh sach id node; buoc 2 doc csv thoe phuong an vao gop lai va subset
### subset theo thoi gian va thoe node.

### dọc shp outfall dê lấy danh sách node subset gia trị
outf = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

ds = as.character(outf@data$StageData)
ds = as.numeric(unique(ds))

setwd("E:/Rdatabase/database/AutoMakeSWMM-model/csvVrsap")
library(readxl)
library(tidyr)
library(stringr)
library(dplyr)

##### xac dinh danh sach files, chu y pattern la xls, XLS, csv, txt.
files = list.files(pattern = "csv")

##### dat hai bien gia thiet
data <- numeric()
revised <- numeric()

for (i in 1:length(files))
{
  data=read.csv(files[i], header=T)
  data = gather(data,node, value, -Step,-Day,-Month,-Year)
  assign(x = files,value = data)
  ## ghep cac bang vao va remove NA element.
  revised <- rbind(revised, na.omit(data))
}
data = revised
data$node = gsub(pattern = "HC","",data$node)

#### Chọn ngày cần lọc để mô phỏng ở đây ví dụ ngày 18
data = subset(data, data$Day == 18 & data$Month == 10)

for (i in 1:length(ds))
{
  df = subset(data,data$node == ds[i])
  df = df[,c(5,6)]
  
  ## chon thoi gian dinh trieu hay sau dinh trieu; ve len và gan id vao xem dinh trieu bat dau tu khi nao
  ## loc tu phan do 
  df$id  = seq(1,length(df$node))
  df = subset(df,df$id > 8)
  df$id  = seq(1,length(df$node))
  names(df) = c("name","y-value","x-value")
  df$type = "Tidal"
  df$`y-value` = df$`y-value`/100
  df = subset(df,select= c("name","type","x-value","y-value"))
  df$y-value = df$y-value - 0.17
  name =file.path("E:/Rdatabase/database/AutoMakeSWMM-model/csvVrsap/curve", paste(ds[i],".csv",sep=""))
  write.table(df,file = name, row.names = F,sep = ",")
}

#### doc phan 12 cua curve; truong hop nhieu curve link voi VRSAP
part12 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part12_curve.txt")
part12cach =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part12 dongcach.txt")

setwd("E:/Rdatabase/database/AutoMakeSWMM-model/csvVrsap/curve")
nodecsv = paste0(ds,".csv")

dff = ""
for (i in 1:length(nodecsv))
{
  df_12 = readLines(nodecsv[i])[-1]
  
  df_12[c(2:length(df_12))] = gsub(pattern="Tidal","",df_12[c(2:length(df_12))])
  dff = c(dff,df_12,part12cach)
}
part12 = c(part12,dff)
part12 = gsub(pattern = ",", "      ", part12)
## gop text vao mot line, cai nay xuong nhat
part12 = gsub(pattern = "\"", "      ", part12)
#writeLines(part12,con = "E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt")

#4. SCRIPT VIET  CHO CONDUIT

## import conduit
con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/conduit.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

df_con = con@data

## doc phan 8 bang thuoc tinh cua conduit
part8 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part8_Con.txt")
df_con8 = df_con[,c(1,15,16, c(2:7))]
write.table(df_con8,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part8.csv", sep = ",",row.names = F)
df_8 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part8.csv")[-1]
part8 = c(part8,df_8)
part8 = gsub(pattern = ",", "     ", part8)

## doc phan 10 bang thuoc Xsection cua conduit
part10 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part10_Xsec.txt")
df_con10 = df_con[,c(1,c(8:14))]
write.table(df_con10,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/part10.csv", sep = ",",row.names = F)
df_10 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/part10.csv")[-1]
part10 = c(part10,df_10)
part10 = gsub(pattern = ",", "     ", part10)
part10 = gsub(pattern = "NA", "     ", part10)

## doc phan 11 doc mat cat cua song (IRREGULAR vao)
### phan nay kha phuc tap vi phai xu ly mc tu excel; chon mc tren arcgis, mo ta mot so buoc nhu sau
### buoc 1: tu file mc đi đo về chuyên vể dạng csv có 2 cột (1)kc và (2)giatri
### buoc 2: tu arcgis chọn mặt căt đại diện cho đoạn kênh đó và gán tên của kênh thanh tên mc ví dụ con6, con7
### bước 3: dùng script dươi đây dọc vào và ghi ra dạng format của SWMM

setwd("E:/Rdatabase/database/AutoMakeSWMM-model/mc")
## xac dinh danh sach files, chu y pattern la xls, XLS, csv, txt.
files = list.files(pattern = "csv")
### lay ten của file csv de cho ten của conduit
mcname = gsub(pattern = ".csv","",files)
### doc phan dau cua dinh dang mat cat trong swmm
mcp1 = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/mc/part1.txt")
### doc phần 11 phan mở dầu của mat cat trong swmm
part11 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part11_Transec.txt")

df1 = ""
df_11 = ""

for (i in 1:length(files))
{
  df = read.csv(files[i],header=T)
  df$id = seq(1,length(df$giatri))
  mcl = length(df$giatri)                           ## lay chieu dai của dòng trong file csv de cho vao thuoc tinh SWMM
  df = gather(df,type,value, -id)
  df = df[order(df$id,df$type),]
  value = c(df$value)
  length(value) <- prod(dim(matrix(value, ncol = 10))) ## truong hop so dong ko chia het cho 10 add gia tri NA
  mx = matrix(value, ncol =10,byrow = TRUE)
  df = as.data.frame(mx)
  df = cbind(a = "GR",df)  ## bo sung cot dau tien cho mat ct 
  write.table(df,file = "E:/Rdatabase/database/AutoMakeSWMM-model/temp/mc.csv", sep = ",",row.names = F)
  df = readLines("E:/Rdatabase/database/AutoMakeSWMM-model/temp/mc.csv")[-1]
  
  ### dọc phần đâu và thay thế thuộc tính
  mcp11 =  mcp1
  mcp11 = gsub(pattern = "AAAAA",mcname[i],mcp11)
  mcp11 = gsub(pattern = "BBBBB",mcl[1],mcp11)
 
  df1 = c(mcp11,df)
  
  # gop mat cat vao 1 file
  df_11 = c(df1,df_11)
}

part11 = c(part11,df_11)
part11 = gsub(pattern = ",", "     ", part11)
part11 = gsub(pattern = "NA", "     ", part11)


#5. SCRIPT VIET  CHO rain gage
## tu so lieu mua copy file muamodel thu muc AutoMakeSWMM-model\csvmua\ load vao dang dung format cua timeseries

df = read_excel("e:/Rdatabase/database/AutoMakeSWMM-model/csvmua/muainput.xlsx",sheet = 1, col_names = T, skip = 0)

df$time = paste(df$h,df$p,sep= ":")
df$h = NULL
df$p = NULL
df = gather(df,name,value,-date,-time)
ds = unique(df$name)
### vong lap ghi cac tran mua ra csv de dua vao swmm
for (i in 1:length(ds))
{
  dfsub = subset(df,df$name == ds[i])
  dfsub = subset(dfsub,select= c("name","date","time","value"))
  dfsub$date <- format(as.Date(dfsub$date), "%d/%m/%Y")
  name =file.path("E:/Rdatabase/database/AutoMakeSWMM-model/csvmua", paste(ds[i],".csv",sep=""))
  write.table(dfsub,file = name, row.names = F,sep = ",")
}

#### doc phan 13 cua curve; truong hop nhieu curve link voi VRSAP
part13 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part13_timese.txt")
part12cach =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part12 dongcach.txt")

setwd("E:/Rdatabase/database/AutoMakeSWMM-model/csvmua")
ds = list.files(pattern = "csv")
dff = ""
for (i in 1:length(ds))
{
  df_13 = readLines(ds[i])[-1]
  dff = c(dff,df_13,part12cach)
}
part13 = c(part13,dff)
part13 = gsub(pattern = ",", "", part13)
## gop text vao mot line, cai nay xuong nhat
part13 = gsub(pattern = "\"", "      ", part13)



# SCRIPT: TAO THANH MODEL SWMM +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Đọc các phần tổng quan gồm part1, và part14.
setwd("E:/Rdatabase/database/AutoMakeSWMM-model")
part1 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part1.txt")
part14 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part14_report.txt")


## Đọc model cũ và và lọc các phần part2; part20; part13; part12.
swmm =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/LXold.inp")

### xac dinh vi tri cua part20_ symbol- vi tri cua tram mua
vt = c(grep(pattern = "[SYMBOLS]",swmm,fixed = TRUE))
part20 = swmm[vt[1]:(vt[1]+3)]

### xac dinh vi tri cua part2_rainage
vt = c(grep(pattern = "[RAINGAGES]",swmm,fixed = TRUE),grep(pattern = "[SUBCATCHMENTS]",swmm,fixed = TRUE))
part2 = swmm[vt[1]:(vt[2]-1)]


### xac dinh vi tri cua part13_ time series
vt = c(grep(pattern = "[TIMESERIES]",swmm,fixed = TRUE),grep(pattern = "[REPORT]",swmm,fixed = TRUE))
part13 = swmm[vt[1]:(vt[2]-1)]

### xac dinh vi tri cua part12_ tidal curve
vt = c(grep(pattern = "[CURVES]",swmm,fixed = TRUE),grep(pattern = "[TIMESERIES]",swmm,fixed = TRUE))
part12 = swmm[vt[1]:(vt[2]-1)]


#+++++++++++++++++++++++++++++++++++++++++++++

 # Tạo model có mặt cắt sông, kênh và kế thừa dữ liệu có sẵn
 swmm = c(part1,part2,part3,part4,part5, part6, part7,part8,part10,part11,
         part12,part13,part14,part16,part17,part19,part20)
 writeLines(swmm, con = "LXHT.inp")
 
 # Tạo model khong có sông kênh và kế thừa dữ liệu có sẵn
 swmm = c(part1,part2,part3,part4,part5, part6, part7,part8,part10,
          part12,part13,part14,part16,part17,part19,part20)
 writeLines(swmm, con = "LX.inp")

# Tạo mơi model
swmm = c(part1,part3,part4,part5, part6, part7,part8,part10,
         part14,part16,part17,part19)
writeLines(swmm, con = "BTPA.inp")





#PHAN III: KHAI THAC KET QUA +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Y TUONG: khac thac so lieu summary cua swmm file.rpt. cu the se khai thac 8 table so lieu va join voi các doi tuong khong 
# gian nhu subcatchment; junction; outfall va conduit. Va sau do the hien ket qua ra gis.
#BUOC 1: extract du lieu va tao thanh 8 data.frame df_sub; df_node; df_surcharge;df_flooding;df_inflow;
## df_outfall; df_linkflow va df_consurcharge.

name = c("plo_lv","junction","outfall","conduit")
name = paste0(name,"-PA0")

setwd("E:/Rdatabase/database/AutoMakeSWMM-model")
swmm =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Model_TV/PA0/TV.rpt")

# xac dinh ket qua cua Subcatchment Runoff Summary
vt1 = c(grep(pattern = "Subcatchment Runoff Summary",swmm,fixed = TRUE))
vt2 = c(grep(pattern = "Node Depth Summary",swmm,fixed = TRUE))
a = swmm[(vt1[1]+8):(vt2[1]-4)]

a = gsub(pattern ="[[:blank:]]", replacement = "/", a)

## tao vector chuyen, vi phai tach cot de khi chuyen sang csv bang duoc dinh dang
b = str_dup("/", 30:1)

## dua ve bang thang hang va cot
for (j in 1: length(b))
{
  a = str_replace_all(a, pattern = b[j],",")
}

writeLines(a,con = "E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt")
data <- read.table("E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt", sep=",", quote="", comment.char="")
data[,1] = NULL
df_sub = data
colnames(df_sub) = c("name","tolprec","tolron","toleva","tolinfil","tolroff","volthous","pekflow","runcoef")


# xac dinh ket qua cua Node Depth Summary
vt1 = c(grep(pattern = "Node Depth Summary",swmm,fixed = TRUE))
vt2 = c(grep(pattern = "Node Inflow Summary",swmm,fixed = TRUE))
a = swmm[(vt1[1]+8):(vt2[1]-4)]
a = gsub(pattern ="[[:blank:]]", replacement = "/", a)
## tao vector chuyen, vi phai tach cot de khi chuyen sang csv bang duoc dinh dang
b = str_dup("/", 30:1)
## dua ve bang thang hang va cot
for (j in 1: length(b))
{
  a = str_replace_all(a, pattern = b[j],",")
}

writeLines(a,con = "E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt")
data <- read.table("E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt", sep=",", quote="", comment.char="")
data[,1] = NULL
df_node = data
colnames(df_node) = c("name","type","avedepth","maxdepth","maxHGL","day","time","remaxdep")


# xac dinh ket qua cua Node Surcharge Summary
vt1 = c(grep(pattern = "Node Surcharge Summary",swmm,fixed = TRUE))
vt2 = c(grep(pattern = "Node Flooding Summary",swmm,fixed = TRUE))
a = swmm[(vt1[1]+9):(vt2[1]-4)]
a = gsub(pattern ="[[:blank:]]", replacement = "/",a)
## tao vector chuyen, vi phai tach cot de khi chuyen sang csv bang duoc dinh dang
b = str_dup("/", 30:1)
## dua ve bang thang hang va cot
for (j in 1: length(b))
{
  a = str_replace_all(a, pattern = b[j],",")
}

writeLines(a,con = "E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt")
data <- read.table("E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt", sep= ",", quote="")
data[,1] = NULL
df_surcharge = data
colnames(df_surcharge) = c("name","type","hours","mabcrown","mbelcrown")



# xac dinh ket qua cua Node Flooding Summary
vt1 = c(grep(pattern = "Node Flooding Summary",swmm,fixed = TRUE))
vt2 = c(grep(pattern = "Outfall Loading Summary",swmm,fixed = TRUE))
a = swmm[(vt1[1]+10):(vt2[1]-4)]
a = gsub(pattern ="[[:blank:]]", replacement = "/",a)
## tao vector chuyen, vi phai tach cot de khi chuyen sang csv bang duoc dinh dang
b = str_dup("/", 30:1)
## dua ve bang thang hang va cot
for (j in 1: length(b))
{
  a = str_replace_all(a, pattern = b[j],",")
}
writeLines(a,con = "E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt")
data <- read.table("E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt", sep= ",", quote="")
data[,1] = NULL
df_flooding = data
colnames(df_flooding) = c("name","flhour","mrate","days","time","volthous","mponded")


# xac dinh ket qua cua Node Inflow Summary
vt1 = c(grep(pattern = "Node Inflow Summary",swmm,fixed = TRUE))
vt2 = c(grep(pattern = "Node Surcharge Summary",swmm,fixed = TRUE))
a = swmm[(vt1[1]+9):(vt2[1]-4)]
a = gsub(pattern ="[[:blank:]]", replacement = "/",a)
## tao vector chuyen, vi phai tach cot de khi chuyen sang csv bang duoc dinh dang
b = str_dup("/", 30:1)
## dua ve bang thang hang va cot
for (j in 1: length(b))
{
  a = str_replace_all(a, pattern = b[j],",")
}
a = gsub(pattern =",ltr", replacement = "",a)
writeLines(a,con = "E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt")
data <- read.table("E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt", sep= ",", quote="")
data[,1] = NULL
df_inflow = data
colnames(df_inflow) = c("name","type","mlainf","mtolinf","day","time","lavlthous","vlthouinfl","flowbal")



# xac dinh ket qua cua Outfall Loading Summary
vt1 = c(grep(pattern = "Outfall Loading Summary",swmm,fixed = TRUE))
vt2 = c(grep(pattern = "Link Flow Summary",swmm,fixed = TRUE))
a = swmm[(vt1[1]+8):(vt2[1]-6)]
a = gsub(pattern ="[[:blank:]]", replacement = "/",a)
## tao vector chuyen, vi phai tach cot de khi chuyen sang csv bang duoc dinh dang
b = str_dup("/", 30:1)
## dua ve bang thang hang va cot
for (j in 1: length(b))
{
  a = str_replace_all(a, pattern = b[j],",")
}
writeLines(a,con = "E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt")
data <- read.table("E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt", sep= ",", quote="")
data[,1] = NULL
df_outfall = data
colnames(df_outfall) = c("name","flowfre","flowave","maxflow","volthous")



# xac dinh ket qua cua Link Flow Summary
vt1 = c(grep(pattern = "Link Flow Summary",swmm,fixed = TRUE))
vt2 = c(grep(pattern = "Flow Classification Summary",swmm,fixed = TRUE))
a = swmm[(vt1[1]+8):(vt2[1]-4)]
a = gsub(pattern ="[[:blank:]]", replacement = "/",a)
## tao vector chuyen, vi phai tach cot de khi chuyen sang csv bang duoc dinh dang
b = str_dup("/", 30:1)
## dua ve bang thang hang va cot
for (j in 1: length(b))
{
  a = str_replace_all(a, pattern = b[j],",")
}
writeLines(a,con = "E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt")
data <- read.table("E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt", sep= ",", quote="")
data[,1] = NULL
df_linkflow = data
colnames(df_linkflow) = c("name","type","maxflow","day","time","mavelo","mfulflow","mfuldep")



# xac dinh ket qua cua Conduit Surcharge Summary: don vị (hours)
vt1 = c(grep(pattern = "Conduit Surcharge Summary",swmm,fixed = TRUE))
vt2 = c(grep(pattern = "Analysis begun on",swmm,fixed = TRUE))
a = swmm[(vt1[1]+8):(vt2[1]-3)]
a = gsub(pattern ="[[:blank:]]", replacement = "/",a)
## tao vector chuyen, vi phai tach cot de khi chuyen sang csv bang duoc dinh dang
b = str_dup("/", 30:1)
## dua ve bang thang hang va cot
for (j in 1: length(b))
{
  a = str_replace_all(a, pattern = b[j],",")
}
writeLines(a,con = "E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt")
data <- read.table("E:/Rdatabase/database/AutoMakeSWMM-model/xb/a.txt", sep= ",", quote="")
data[,1] = NULL
df_conSurcharge = data
colnames(df_conSurcharge) = c("name","bothend","up","down","abnormal","capacity")


#BUOC 2: cap nhat ket qua vao shape file va ghi ra shapefile moi_shp_out.

## df_outfall; df_linkflow va df_consurcharge.
setwd("E:/Rdatabase/database/AutoMakeSWMM-model/gis")

#1: update ket qua cho subcathment
lv = readShapeSpatial('E:/Rdatabase/database/AutoMakeSWMM-model/gis/plo_lv.shp',
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
d = dim(lv@data)
lv@data[,c(5:d[2])] = NULL
lv@data$rgage = NULL
lv@data$outlet = NULL
lv@data$id = c(seq(1,d[1]))
lv@data = merge(lv@data,df_sub, by = "name",all.x = TRUE)
lv@data = lv@data[order(lv@data$id),]      # phai sort lai neu khong se bi nham ten giua cac ploygon
lv@data$id = NULL
## ghi de len file lv co sang
writeOGR(lv, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer =name[1], driver = 'ESRI Shapefile',overwrite_layer= TRUE)

#2: update ket qua cho junction
jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

jun@data = subset(jun@data, select = -c(elevation:Aponded,conduit))
d = dim(jun@data)
jun@data$id = c(seq(1,d[1]))
jun@data = merge(jun@data,df_surcharge, by = "name",all.x = TRUE)
jun@data = merge(jun@data,df_flooding, by = "name",all.x = TRUE)
jun@data$type = NULL
jun@data = merge(jun@data,df_node, by = "name",all.x = TRUE)
jun@data$type = NULL
jun@data = merge(jun@data,df_inflow, by = "name",all.x = TRUE)
jun@data = jun@data[order(jun@data$id),]
jun@data$id = NULL

## ghi ra junction_out
writeOGR(jun, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer =name[2], driver = 'ESRI Shapefile',overwrite_layer= TRUE)

#3: update ket qua cho outfall
outf = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
d = dim(outf@data)
outf@data = subset(outf@data, select = -c(elevation:RouteTo,conduit)) # xoa cac cot k can thiet
outf@data$id = c(seq(1,d[1]))
outf@data = merge(outf@data,df_outfall, by = "name",all.x = TRUE)
outf@data = merge(outf@data,df_inflow, by = "name",all.x = TRUE)
outf@data$type = NULL
outf@data = merge(outf@data,df_node, by = "name",all.x = TRUE)
outf@data$type = NULL
outf@data = outf@data[order(outf@data$id),]
outf@data$id = NULL
## ghi ra outfall_out
writeOGR(outf, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = name[3], driver = 'ESRI Shapefile',overwrite_layer= TRUE)


#4: update ket qua cho outfall
con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/conduit.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
con@data = subset(con@data, select = -c(length:topoint)) # xoa cac cot k can thiet
d = dim(con@data)
con@data$id = c(seq(1,d[1]))
con@data = merge(con@data,df_conSurcharge, by = "name",all.x = TRUE)
con@data = merge(con@data,df_linkflow, by = "name",all.x = TRUE)
con@data = con@data[order(con@data$id),]
con@data$id = NULL
## ghi de len file  co san
writeOGR(con, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer =name[4], driver = 'ESRI Shapefile',overwrite_layer= TRUE)








#@@@@@@@@@@@@@@@@@@@@ SCRIPT ROI RAC @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#1. LOC REMOVE DUPLICATE DE TAO JUNCTION VA OUTFALL 
## y tuong sau khi ve or buffer 
con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/xb.shp",
proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
df = con@data
df = unique(df)

xy <- df[,c(1,2)]
coords = xy
sp = SpatialPoints(coords)
spdf <- SpatialPointsDataFrame(sp,data = df)
# gan toa do 
Utm48N = " +proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(spdf) = CRS(Utm48N)

writeOGR(spdf, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'xb1', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


#2. GOP CAC FILE POINT LOC RA 1 SHP GOM JUNCTION VA OUTFALL
p1 = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/point-Intersect/linkconduit.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

p3 = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/point-Intersect/cendstart.shp",
                      proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
## xac dinh toa do cac diem
p1@data$x = coordinates(p1)[,1]
p1@data$y = coordinates(p1)[,2]
p3@data$x = coordinates(p3)[,1]
p3@data$y = coordinates(p3)[,2]

dfp1 = subset(p1@data, select= c(x,y))
dfp3 = subset(p3@data, select= c(x,y))

df = rbind(dfp1,dfp3)

df = unique(df)

xy <- df[,c(1,2)]
coords = xy
sp = SpatialPoints(coords)
spdf <- SpatialPointsDataFrame(sp,data = df)
# gan toa do 
Utm48N = " +proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(spdf) = CRS(Utm48N)

writeOGR(spdf, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'jun_outfall', driver = 'ESRI Shapefile',overwrite_layer= TRUE)



#3. Chuan bị số liệu H từ phương án của VRSAP to SWMM

## y tuong: load outfall vao lay danh sach id node; buoc 2 doc csv thoe phuong an vao gop lai va subset
## subset theo thoi gian va thoe node.

### dọc shp outfall dê lấy danh sách node subset gia trị
outf = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

ds = as.character(outf@data$StageData)
ds = as.numeric(unique(ds))

setwd("E:/Rdatabase/database/AutoMakeSWMM-model/csvVrsap")
library(readxl)
library(tidyr)
library(stringr)
library(dplyr)

##### xac dinh danh sach files, chu y pattern la xls, XLS, csv, txt.
files = list.files(pattern = "csv")

##### dat hai bien gia thiet
data <- numeric()
revised <- numeric()

for (i in 1:length(files))
{
  data=read.csv(files[i], header=T)
  data = gather(data,node, value, -Step,-Day,-Month,-Year)
  assign(x = files,value = data)
  ## ghep cac bang vao va remove NA element.
  revised <- rbind(revised, na.omit(data))
}
data = revised
data$node = gsub(pattern = "HC","",data$node)

#### Chọn ngày cần lọc để mô phỏng ở đây ví dụ ngày 18
data = subset(data, data$Day == 18 & data$Month == 10)

for (i in 1:length(ds))
{
df = subset(data,data$node == ds[i])
df = df[,c(5,6)]

## chon thoi gian dinh trieu hay sau dinh trieu; ve len và gan id vao xem dinh trieu bat dau tu khi nao
## loc tu phan do 
df$id  = seq(1,length(df$node))
df = subset(df,df$id > 8)
df$id  = seq(1,length(df$node))
names(df) = c("name","y-value","x-value")
df$type = "Tidal"
df$`y-value` = df$`y-value`/100
df = subset(df,select= c("name","type","x-value","y-value"))
name =file.path("E:/Rdatabase/database/AutoMakeSWMM-model/csvVrsap/curve", paste("PA23","-",ds[i],".csv",sep=""))
write.table(df,file = name, row.names = F,sep = ",")
}

#### doc phan 12 cua curve; truong hop nhieu curve link voi VRSAP
part12 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part12_curve.txt")
part12cach =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part12 dongcach.txt")

setwd("E:/Rdatabase/database/AutoMakeSWMM-model/csvVrsap/curve")
nodecsv = paste0(ds,".csv")

dff = ""
for (i in 1:length(nodecsv))
{
df_12 = readLines(nodecsv[i])[-1]

df_12[c(2:length(df_12))] = gsub(pattern="Tidal","",df_12[c(2:length(df_12))])
dff = c(dff,df_12,part12cach)
}
part12 = c(part12,dff)
part12 = gsub(pattern = ",", "      ", part12)
## gop text vao mot line, cai nay xuong nhat
part12 = gsub(pattern = "\"", "      ", part12)



#4. Chuan bị số liệu mua cho vao model
## tu so lieu mua copy file muamodel thu muc AutoMakeSWMM-model\csvmua\ load vao dang dung format cua timeseries
##

df = read_excel("e:/Rdatabase/database/AutoMakeSWMM-model/csvmua/muainput.xlsx",sheet = 1, col_names = T, skip = 0)

df$time = paste(df$h,df$p,sep= ":")
df$h = NULL
df$p = NULL
df = gather(df,name,value,-date,-time)
ds = unique(df$name)
### vong lap ghi cac tran mua ra csv de dua vao swmm
for (i in 1:length(ds))
{
  dfsub = subset(df,df$name == ds[i])
  dfsub = subset(dfsub,select= c("name","date","time","value"))
  dfsub$date <- format(as.Date(dfsub$date), "%d/%m/%Y")
  name =file.path("E:/Rdatabase/database/AutoMakeSWMM-model/csvmua", paste(ds[i],".csv",sep=""))
  write.table(dfsub,file = name, row.names = F,sep = ",")
}

#### doc phan 13 cua curve; truong hop nhieu curve link voi VRSAP
part13 =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part13_timese.txt")
part12cach =  readLines("E:/Rdatabase/database/AutoMakeSWMM-model/Part12 dongcach.txt")

setwd("E:/Rdatabase/database/AutoMakeSWMM-model/csvmua")
ds = list.files(pattern = "csv")
dff = ""
for (i in 1:length(ds))
{
  df_13 = readLines(ds[i])[-1]
  dff = c(dff,df_13,part12cach)
}
part13 = c(part13,dff)
part13 = gsub(pattern = ",", "", part13)
## gop text vao mot line, cai nay xuong nhat
part13 = gsub(pattern = "\"", "      ", part13)



# NHOM SCRIP REMOVE COT NEU UPDATE SAI THONG TIN VA GHI LẠI

## fix cua junction
jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

df_j = jun@data[,c(3:10)]
jun@data[,c(1:2)] = NULL
jun@data[,-c(1:8)] = NULL
writeOGR(jun, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='junction', driver = 'ESRI Shapefile',overwrite_layer= TRUE)

## fix cua outfall
outf = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

outf@data[,-c(3:10)] = NULL
writeOGR(outf, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='outfall', driver = 'ESRI Shapefile',overwrite_layer= TRUE)

## fix cua conduit
con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/conduit.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

con@data[,-c(2:17)] = NULL
writeOGR(con, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='conduit', driver = 'ESRI Shapefile',overwrite_layer= TRUE)



# NHOM SCRIPt combine cột cần lấy và thay tên jun thanh hg
jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction_obao.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

df = jun@data[,c(1,2,3,4,9)]

junht = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction-HT.shp",
                          proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

dfht = subset(junht@data, select = c("name","flhour","mponded"))


colnames(dfht) =  c("name","ht_fhou","ht_ponded")

junPA1 = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction-PA1.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
dfpa1 = subset(junPA1@data, select = c("name","flhour","mponded"))

colnames(dfpa1) =  c("name","pa1_fhou","pa1_ponded")


junPA21 = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction-PA21.shp",
                          proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
dfpa21 = subset(junPA21@data, select = c("name","flhour","mponded"))
colnames(dfpa21) =  c("name","pa21_fhou","pa21_ponded")


junPA22 = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction-PA22.shp",
                          proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
dfpa22 = subset(junPA22@data, select = c("name","flhour","mponded"))
colnames(dfpa22) =  c("name","pa22_fhou","pa22_ponded")


junPA23 = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction-PA23.shp",
                           proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
dfpa23 = subset(junPA23@data, select = c("name","flhour","mponded"))
colnames(dfpa23) =  c("name","pa23_fhou","pa23_ponded")

df = merge(df,dfht,by="name",all.x = TRUE)
df = merge(df,dfpa1,by="name")
df = merge(df,dfpa21,by="name")
df = merge(df,dfpa22,by="name")
df = merge(df,dfpa23,by="name")

df$name = gsub(pattern = "jun","hg",df$name)
df = na.omit(df)
write.table(df,file = "df_LXSWMM.csv", row.names = F,sep = ",")






# NHOM SCRIPt plot Biên mực nước tại các node từ chú Đông, chọn riêng tháng 10
setwd("E:/Rdatabase/database/AutoMakeSWMM-model/csvVrsap")
library(readxl)
library(tidyr)
library(stringr)
library(dplyr)

##### xac dinh danh sach files, chu y pattern la xls, XLS, csv, txt.
files = list.files(pattern = "csv")

##### dat hai bien gia thiet
data <- numeric()
revised <- numeric()

for (i in 1:length(files))
{
  data=read.csv(files[i], header=T)
  data = gather(data,node, value, -Step,-Day,-Month,-Year)
  data$case = files[i]
  assign(x = files,value = data)
  ## ghep cac bang vao va remove NA element.
  revised <- rbind(revised, na.omit(data))
}
data = revised
data = subset(data, data$Month == 10)
data$value = data$value/100
data$case = gsub(pattern = ".csv","",data$case) 
data = subset(data,data$Step >= 7000 & data$Step < 7028)
## Ve mat cat ra 
library(ggplot2)
plot <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$node)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$node==sta_list[i]),aes(Step,value,color = case))
    plots = p + geom_line()+
      ggtitle(paste("Ve tai node",sta_list[i]))+
      scale_y_continuous("Level(m)")+
      theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))
    name =file.path("E:/Rdatabase/database/AutoMakeSWMM-model/csvVrsap/Image", paste(sta_list[i],".png",sep=""))
    ggsave(plots,filename=name,scale = 1)
  }
}
plot(data) 



### KIEM TRA DANH SACH NODE CO TRONG KET QUA XUAT VRSAP KO?
outf = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
data$node = gsub(pattern = "HC","",data$node)
df = merge(outf@data,data,by.x = "StageData",by.y = "node",all.x = TRUE)


# NHOM SCRIPt REMOVING JUNCTION KHONG NOI
## fix cua junction
jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/conduit.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))

df = con@data
dffrom = subset(df,select = c("frompoint"))
dffrom$from = 1

dfto = subset(df,select = c("topoint"))
dfto$to = 1

df = jun@data
df = merge(df,dffrom, by.x = "name", by.y = "frompoint", all.x = TRUE)
df = merge(df,dfto, by.x = "name", by.y = "topoint", all.x = TRUE)

df = subset(df,df$from == 1|df$to == 1)
df$from = NULL
df$to = NULL

# tao xy to point va set wgs84 - 48N.
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)  # spatial dependence

## tao point tu xy, gom 2 phan tao geometry (coords), tao data (df), va toa do CRS
df = unique(df)
xy <- df[,c(2,3)]
coords = xy
sp = SpatialPoints(coords)
spdf <- SpatialPointsDataFrame(sp,data = df)

# gan toa do 
Utm48N = " +proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(spdf) = CRS(Utm48N)

# ghi ra shapefile

writeOGR(spdf, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'junction1', driver = 'ESRI Shapefile',overwrite_layer= TRUE)





#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# nhom script tinh cac thong so cho conduit; junciton va outfall tham chieu code nen
## tinh cho conduit
con = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/conduit.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
### chu y phai thay ten shapefil o moi phuong an khac nhau
obao = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/OBao_PAI.shp",
                        proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
con@data = merge(con@data,obao@data, by = "pa1", all.x = TRUE)
con@data = con@data%>%select(-pa1,everything())
con@data = subset(con@data,select = c(name:znen,pa1))  # xep lai cot
### tinh cao trinh inset and outletoffset
con@data$Geom1 = as.numeric(con@data$Geom1)
con@data <- con@data %>% mutate(InOffset = znen-0.7-Geom1)
con@data <- con@data %>% mutate(OutOffset = znen-0.7-Geom1)
con@data$znen = NULL
writeOGR(con,dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='conduit', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


## tinh cho junction
jun = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/junction.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
### lay cot name conduit va inOffset,Geom1 de tinh cho cao trinh evlevation cua juction
df = con@data[,c(1,4,9)]
jun@data = merge(jun@data,df,by.x ="conduit",by.y = "name",all.x = TRUE)
jun@data = jun@data%>%select(-conduit,everything())   ## xep cot conduit ve cuoi
jun@data <- jun@data %>% mutate(elevation = InOffset- 0.03)
jun@data <- jun@data %>% mutate(maxdepth = 0.03 +Geom1+0.7)
jun@data$InOffset = NULL
jun@data$Geom1 = NULL
writeOGR(jun,dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='junction', driver = 'ESRI Shapefile',overwrite_layer= TRUE)


## tinh cho outfall
out = readShapeSpatial("E:/Rdatabase/database/AutoMakeSWMM-model/gis/outfall.shp",
                       proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
### lay cot name conduit va inOffset de tinh cho cao trinh evlevation cua juction
df = con@data[,c(1,4)]
out@data = merge(out@data,df,by.x ="conduit",by.y = "name",all.x = TRUE)
out@data = out@data%>%select(-conduit,everything()) 
out@data <- out@data %>% mutate(elevation = InOffset- 0.05)
out@data$InOffset = NULL
writeOGR(con,dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer ='conduit', driver = 'ESRI Shapefile',overwrite_layer= TRUE)



# nhom script xong bo
con = readShapeSpatial(file.choose(),
                     proj4string=CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"))
head(xb@data)
xb@data = subset(xb@data,select = c(name:Aponded,pa1,pa2,pa3,name_1))
head(xb@data)
names(xb@data) = c("pa3","znen")
writeOGR(con, dsn = 'E:/Rdatabase/database/AutoMakeSWMM-model/gis', 
         layer = 'conduit', driver = 'ESRI Shapefile',overwrite_layer= TRUE)

xb@data[,c(1)] = NULL
xb@data = subset(xb@data, select = c(name:RouteTo,name_1,pa1,pa2,pa3))
xb@data$conduit = xb@data$name_1
xb@data$name_1 = NULL

df = xb@data[,c(26,27)]
df = unique(df)
df$id = seq(1,length(df$x))

names(con@data)[names(con@data) == 'Roghnss'] <- 'Roughness'
names(con@data)[names(con@data) == 'InOffst'] <- 'InOffset'
names(con@data)[names(con@data) == 'OtOffst'] <- 'OutOffset'
names(con@data)[names(con@data) == 'Roghnss'] <- 'Roughness'
names(con@data)[names(con@data) == 'frmpnt_x'] <- 'frompoint'
names(con@data)[names(con@data) == 'topnt_x'] <- 'topoint'
con@data$InOffset = NULL
con@data$OutOffset = NULL

con@data = subset(con@data, select= c(name:Culvert,frompoint,topoint,pa0,pa1,pa2))
