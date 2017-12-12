#@ muc tieu chuan bi mat chat cho SWMM dang irregular 
#@ huong giai quyet (1) tu so lieu tho dua ve dang cong don nhu MIKE11 va paste vao SWMM;
#@ (2) tao ra format dang trong file ini cua SWMM.

#@ Huong xu ly: tach bang thanh phan theo thong so rui gop lai

setwd("E:/Rdatabase/database/xb")
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
## xac dinh danh sach files, chu y pattern la xls, XLS, csv, txt.
files = list.files(pattern = "xls")

for (i in 1:length(files))
{
  filename=files[1]
  df =read_excel(filename, sheet = 1, col_names = F)
  df = t(df)
  ## lay kich thuoc bang de gan cot
  d = dim(df)
  a = rep(seq(1,d[2]/3,1),each = 3)
  b = c("id","x","z")
  name = paste0(b,a)
  colnames(df) = name
  df = as.data.frame(df)
  no = seq(1,d[1],1)
  df$no = no
  ## tach bang thanh phan de tinh
  df = gather(df,type, value, -no)
  dfid = df %>%
    filter(str_detect(type, "id"))
  dfx = df %>%
    filter(str_detect(type, "x"))  
  dfz = df %>%
    filter(str_detect(type, "z")) 
  dfx$value <- as.numeric(dfx$value)
  dfx = dfx%>% group_by(type)%>% mutate(value = cumsum(value))
  
  df = rbind(dfid,dfx,dfz)
  df = spread(df,type,value)
  df$no = NULL
  df = subset(df, select=c(name))
  name1 =file.path("e:/Rdatabase/database/xb", paste(files[i],".csv",sep=""))
  write.table(df,file = name1, row.names = F,sep = ",")
}



df =read_excel(file.choose(), sheet = 1, col_names = F)
df =as.data.frame(df)


#SCRIPT: TRUONG HOP MAT CAT CAC KENH NAM TRONG MOT SHEET EXCEL
setwd("E:/Rdatabase/database/xb")
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

## xac dinh danh sach files, chu y pattern la xls, XLS, csv, txt.
files = list.files(pattern = "xls")

for (i in 1:length(files))
{
  df =read_excel(files[i], sheet = 1, col_names = F)
  df = as.data.frame(t(df))
  ## lay kich thuoc bang de gan cot
  d = dim(df)
  a = rep(seq(1,d[2]/3,1),each =3)
  b = c("id","x","z")
  name = paste0(b,a)
  colnames(df) = name
  df = as.data.frame(df)
  no = seq(1,d[1],1)
  df$no = no
  ## tach bang thanh phan de tinh
  df = gather(df,type, value, -no)
  dfid = df %>%
    filter(str_detect(type, "id"))
  dfx = df %>%
    filter(str_detect(type, "x"))  
  dfz = df %>%
    filter(str_detect(type, "z")) 
  dfx$value <- as.numeric(dfx$value)
  dfx = dfx%>% group_by(type)%>% mutate(value = cumsum(value))
  
  df = plyr::rbind.fill(dfid,dfx,dfz)
  df = spread(df,type,value)
  df$no = NULL
  df = subset(df, select=c(name))
  
  ### xac dinh chainage trong df
  km = as.character(df[1,])
  km = toupper(km)
  id = grep(pattern = "K",km)
  km = as.character(km[id])
  canal = gsub(pattern = ".xls","",files)
  ### xac dinh dataframe cho for lop tach csv
  d = dim(df)
  id = seq(1,d[2],3)
  x = seq(2,d[2],3)
  z = seq(3,d[2],3)
  
  for (j in 1:length(id))
  {
    data = df[,c(x[j]:z[j])]
    data = na.omit(data)
    colnames(data) = c("kc","giatri")
    name1 =file.path("e:/Rdatabase/database/xb", paste(canal[i],"-",km[j],".csv",sep=""))
    write.table(data,file = name1, row.names = F,sep = ",")
  }
}














































































































