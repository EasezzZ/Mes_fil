#Filter data with no variation in five continous PM2.5 measurements
#REASON: by referring to hourly and daily time series of measurements from all sites, patterns of data with no variation are withness
# 93429 measurements are filtered in total by filtering conditions: #1. PM2_5 and TimePoint is not NULL #2. 3<PM2_5<1000 and 
# #3. no variation in five continous PM2.5 measurements #4. measurements with same StationCode and TimePoint are regarded as duplicated data and filtered
# filter numner #1:71133   #2: 19881    #3:2415  #4:161
#Yixuan Zheng,2014/11/28

library(plyr)
library(RNetCDF)

#define function to convert factor to numeric (There is not a function can be used to do this)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#=======set input & output directory=========

indir <- "F:/Data_Source/0.Data_Source/2.观测数据/2.污染物观测"
outdir <- "D:/1.科研/2.BJ_PM2.5/4.结果/Mes_fil_out"

infile <- paste(indir,"BTH2013.csv",sep="/")

#========read input csv==========

inmes <- read.csv(infile)
req.col <- colnames(inmes)[c(1,2,3,4,5,16,17,20,21)]

#========primary rearrange & filter input data==========


#----------seletct required columns and valid dat----------
mes.pm.n <- subset(inmes, PM2_5 != "NULL"  & TimePoint != "NULL", select=req.col) 
mes.pm <- subset(mes.pm.n,as.numeric.factor(PM2_5) > 3 & as.numeric.factor(PM2_5) < 1000 )
mes.pm$PM2_5 <- as.numeric.factor(mes.pm$PM2_5) #warning "NAs introduced by coercion" donot matter

rm(mes.pm.n) # free variable


#---------------filter duplicate data in mes.pm----------------------
#data which with same StationCond TimePoint is regard as duplicate data

mes.dup <- subset(mes.pm,select=c(StationCode,TimePoint))
ind.dup <- duplicated(mes.dup)
mes.pm <- mes.pm[!ind.dup,]

#-----------TimePoint manipulate------------
timetag <- strsplit(as.character(mes.pm$TimePoint), " ")
datetag <- unlist(timetag)[c((1:length(timetag))*2-1)]
hourtag <- unlist(timetag)[c((1:length(timetag))*2)]
mes.pm$Hour <- as.numeric(unlist(strsplit(hourtag, ":"))[c((1:length(hourtag))*2-1)])
mes.pm$Date <- as.Date(datetag)

rm(timetag,datetag,hourtag) # free variable
#--------------StationCode manipulate---------

#turn $StationCode into numeric and store in column $Station
mes.pm$Station <- as.numeric(gsub("A", "", mes.pm$StationCode))

#sort dataframe by station, date, and hour
mes.sort <- mes.pm[with(mes.pm, order(Station, Date, Hour)), ]

#indentify anomalies index
mes.num <- dim(mes.pm)[1]
vec <- c()
for (mes.ind in 1:(mes.num-4)){
  mes.sd <- sd(mes.sort$PM2_5[mes.ind:(mes.ind+4)])
  if (mes.sd == 0){
    vec <- c(vec,mes.ind:(mes.ind+4))
  }
}

vec.uni <- unique(vec) #length(vec.uni) = 2415
ori.ind <- as.numeric(rownames(mes.sort[vec.uni,])) #anomalies index in inmes
#=================Refilter====================
mes.out.o <- inmes[setdiff(1:dim(inmes)[1],ori.ind),]
mes.out.n <- subset(mes.out.o, PM2_5 != "NULL"  & TimePoint != "NULL") 
mes.out <- subset(mes.out.n,as.numeric.factor(PM2_5) > 3 & as.numeric.factor(PM2_5) < 1000 )

mes.out.dup <- subset(mes.out,select=c(StationCode,TimePoint))
ind.out.dup <- duplicated(mes.out.dup)
mes.out <- mes.out[!ind.out.dup,]

#write.csv(mes.out,paste(outdir,'BTH2013_filter.csv',sep="/"),row.names = FALSE,quote=FALSE)

#==================store into netCDF========================

BTH.domain <- c(35.7,113.4,42.8,120.0)
grid.res <- 0.1
bth.cols <- ( BTH.domain[4] - BTH.domain[2] )/grid.res
bth.rows <- ( BTH.domain[3] - BTH.domain[1] )/grid.res
date.org <- as.Date("2013-01-01")
#------------------read coordinate file--------------------------

incor <- paste(indir,"BTH_Coor_2013.csv",sep="/")
site.cor <- read.csv(incor)

#----------------get site index in projected grids------------------

site.cor$latind <- ceiling((site.cor$Latitude-BTH.domain[1])/grid.res)
site.cor$lonind <- ceiling((site.cor$Longitude-BTH.domain[2])/grid.res)
site.cor$StationCode <- as.numeric(site.cor$StationCode)
#--------------project measurements from coordinate to grids-------------
mes.arr <- array(0,dim=c(bth.cols,bth.rows,365))
mes.nod <- array(0,dim=c(bth.cols,bth.rows,365))
mes.out$Station <- as.numeric(gsub("A", "", mes.out$StationCode))
for (mes.count in 1:dim(mes.out)[1]){
  dayofy <- as.numeric(strftime(mes.out$TimePoint[mes.count],format="%j"))
  print(dayofy)
  site.ind <- which(mes.out$Station[mes.count] == site.cor$StationCode)
  mes.arr[site.cor$lonind[site.ind],site.cor$lonind[site.ind],dayofy]
}

#-------------write to netCDF------------
outnc <- paste(outdir,"BTH_PM25_Coor_2013_24h_filter.nc",sep="/")
outnc.id <- create.nc(outnc)

dim.def.nc(outnc.id, "lon", bth.cols)
dim.def.nc(outnc.id, "lat", bth.rows)
dim.def.nc(outnc.id, "day", 365)

var.def.nc(outnc.id, "PM25", "NC_DOUBLE", c("lon","lat","day"))

var.put.nc(outnc.id, "PM25", mes.arr,c(1,1,1),c(bth.cols,bth.rows,365))

close.nc(outnc.id)

# timetag <- strsplit(as.character(mes.out$TimePoint), " ")
# datetag <- unlist(timetag)[c((1:length(timetag))*2-1)]
# hourtag <- unlist(timetag)[c((1:length(timetag))*2)]
# mes.out$Hour <- as.numeric(unlist(strsplit(hourtag, ":"))[c((1:length(hourtag))*2-1)])
# mes.out$Date <- as.Date(datetag)
# mes.sort2 <- mes.pm[with(mes.out, order(StationCode, Date, Hour)), ]
# 
# 
# plot.ts(mes.out$PM2_5[mes.out$StationCode=='1054A'])
