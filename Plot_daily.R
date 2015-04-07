#Read Measurement data of BTH from a single file and 
#process these data into daily averaged PM2.5 concentration for identifying the anomalies

#By Yixuan Zheng,2014/11/27

library(plyr)

#define function to convert factor to numeric (There is not a function can be used to do this)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#=======set input & output directory=========

indir <- "F:/Data_Source/0.Data_Source/2.观测数据/2.污染物观测"
outdir <- "D:/1.科研/2.BJ_PM2.5/4.结果/Mes_fil_out"

infile <- paste(indir,"BTH2013.csv",sep="/")
#infile <- paste(outdir,"BTH2013_filter.csv",sep="/")

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

rm(mes.dup,ind.dup) # free variable

#-----------TimePoint manipulate------------

timetag <- strsplit(as.character(mes.pm$TimePoint), " ")
datetag <- unlist(timetag)[c((1:length(timetag))*2-1)]
hourtag <- unlist(timetag)[c((1:length(timetag))*2)]
mes.pm$Year <- as.numeric(unlist(strsplit(datetag, "/"))[c((1:length(hourtag))*3-2)])
mes.pm$Month <- as.numeric(unlist(strsplit(datetag, "/"))[c((1:length(hourtag))*3-1)])
mes.pm$Day<- as.numeric(unlist(strsplit(datetag, "/"))[c((1:length(hourtag))*3)])
mes.pm$Hour <- as.numeric(unlist(strsplit(hourtag, ":"))[c((1:length(hourtag))*2-1)])
mes.pm$Date <- as.Date(datetag)

rm(timetag,datetag,hourtag) # free variable
#--------------StationCode manipulate---------

#turn $StationCode into numeric and store in column $Station
mes.pm$Station <- as.numeric(gsub("A", "", mes.pm$StationCode))
mes.pm <- subset(mes.pm,select=c(Area,Area_code,PositionName,Station,Latitude,Longitude,PM2_5,PM2_5_24h,Year, Month, Day, Hour,Date))

#=================average measurement on daily basis=====================

mes.avg <- ddply(mes.pm,.(Station,Date),function(.ele) mean(.ele$PM2_5)) #ddply reture dataframe

site <- unique(mes.avg$Station) #site name
for ( site.ind in 1:length(site) ){
  #daily averaged
  jpeg(filename=paste(outdir,"/gt3/time_series",as.character(site[site.ind]),".jpg",sep=""))
  plot.ts(mes.avg$V1[mes.avg$Station==site[site.ind]])
  dev.off()
  #hourly 
  jpeg(filename=paste(outdir,"/gt3/hour_time_series",as.character(site[site.ind]),".jpg",sep=""),width = 3840, height = 960)
  plot.ts(mes.pm$PM2_5[mes.pm$Station==site[site.ind]])
  dev.off()
}








