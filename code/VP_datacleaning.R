install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(stringr)
library(dplyr)
getwd()
vp<-read.csv("~/Cheadle-Center-Vernal-Pools/CCBER_Vernal_Pool_Hydrology_0.csv",strip.white = TRUE)
vp<-vp[,c(3,4,7,8)]
vp$Date<-as.POSIXct(vp$Date, format="%m/%d/%Y %I:%M:%S %p")
vp<-vp[vp$Location!="more_mesa" & vp$Location!="copr" & 
         vp$Location!="Ellwood_mesa"& vp$Location!="sierra_madre"
       & vp$Location!="ellwood_mesa",]
vp$VP_Name<-str_to_lower(vp$VP_Name)
colnames(vp)<-c("Date.Time","Location","VP_Name","Water_Level")
vp$Date<-paste0(substr(vp$Date.Time,1,10))
vp$Date<-as.Date(vp$Date,format="%Y-%m-%d")
unique(vp$Location)
unique(vp$VP_Name)


vp<- within(vp, VP_Name[VP_Name == "5!"] <- '5')
vp<- within(vp, VP_Name[VP_Name == "2."] <- '2')
vp<- within(vp, VP_Name[VP_Name == "ph 1"] <- 'ph1')
vp<- within(vp, VP_Name[VP_Name == "west ponder"] <- 'west pond')
vp<- within(vp, VP_Name[VP_Name == "south vernal pool"] <- 'south')
vp<- within(vp, VP_Name[VP_Name == "south pool"] <- 'south') 
vp<- within(vp, VP_Name[VP_Name == "east s pool"] <- 'east s') 
vp<- within(vp, VP_Name[VP_Name == "western pond"] <- 'west pond')
vp<- within(vp, VP_Name[VP_Name == "01"] <- '1')
vp<- within(vp, VP_Name[VP_Name == "manzanita wetland"] <- 'manzanita')
vp<- within(vp, VP_Name[VP_Name == "east - north"] <- 'east n')
vp<- within(vp, VP_Name[VP_Name == "east north"] <- 'east n')
vp<- within(vp, VP_Name[VP_Name == "east - south"] <- 'east s')
vp<- within(vp, VP_Name[VP_Name == "north pool"] <- 'north')
vp<- within(vp, VP_Name[VP_Name == "main pool"] <- 'main')
vp<- within(vp, VP_Name[VP_Name == "sww 01"] <- 'sww')
vp<- within(vp, VP_Name[VP_Name == "chumash wetland"] <- 'chumash')
vp<- within(vp, VP_Name[VP_Name == "chumash pool"] <- 'chumash')
vp<- within(vp, VP_Name[VP_Name == "8 in"] <- '8')
vp<- within(vp, VP_Name[VP_Name == "manzanita lake"] <- 'manzanita')
vp<- within(vp, VP_Name[VP_Name == "sww1"] <- 'sww 1')
vp<- within(vp, VP_Name[VP_Name == "catalina"] <- 'santa catalina')
vp<- within(vp, VP_Name[VP_Name == "phase 1"] <- 'ph1')
vp<- within(vp, VP_Name[VP_Name == "mini south"] <- '9')
vp<- within(vp, VP_Name[VP_Name == "ms"] <- '9')
vp<- within(vp, VP_Name[VP_Name == "nww 1"] <- '1')
vp<- within(vp, VP_Name[VP_Name == "nww 2"] <- '2')
vp<- within(vp, VP_Name[VP_Name == "nww 3"] <- '3')
vp<- within(vp, VP_Name[VP_Name == "nww 4"] <- '4')
vp<- within(vp, VP_Name[VP_Name == "nww 7"] <- '7')
vp<- within(vp, VP_Name[VP_Name == "nww 8"] <- '8')
vp<- within(vp, VP_Name[VP_Name == "nww 9"] <- '9')
vp<- within(vp, VP_Name[VP_Name == "sww 1"] <- 'sww 1')
vp<- within(vp, VP_Name[VP_Name == "6.75"] <- '7')
vp<- within(vp, VP_Name[VP_Name == "redtail 3"] <- '3')
vp<- within(vp, VP_Name[VP_Name == "redtail3"] <- '3')
vp<- within(vp, VP_Name[VP_Name == "tadpole 6"] <- '6')
vp<- within(vp, VP_Name[VP_Name == "tadpole 7"] <- '7')
vp<- within(vp, VP_Name[VP_Name == "tadpol 7"] <- '7')
vp<- within(vp, VP_Name[VP_Name == "whitetail 2"] <- '2')
vp<- within(vp, VP_Name[VP_Name == "whitetail 1"] <- '1')
vp<- within(vp, VP_Name[VP_Name == "creekside"] <- '19')
vp<- within(vp, VP_Name[VP_Name == "cs"] <- '19')
vp<- within(vp, VP_Name[VP_Name == "mini south"] <- '9')
vp<- within(vp, VP_Name[VP_Name == "wt1"] <- '14')
vp<- within(vp, VP_Name[VP_Name == "wt2"] <- '16')
#Setting values we want to remove to 'R'
vp<- within(vp, VP_Name[Location=="ncos" & VP_Name == "9"] <- 'r')
vp<- within(vp, VP_Name[Location=="ncos" & VP_Name == "10"] <- 'r')

vp<- within(vp, VP_Name[Location=="ncos" & VP_Name == "16"] <- 'r')
vp<- within(vp, VP_Name[Location=="north_parcel" & VP_Name == "1"] <- 'r')
vp<- within(vp, VP_Name[Location=="north_parcel" & VP_Name == "11"] <- 'r')
vp<- within(vp, VP_Name[Location=="south_parcel" & VP_Name == "13"] <- 'r')
vp<- within(vp, VP_Name[Location=="south_parcel" & VP_Name == "2"] <- 'r')
vp<- within(vp, VP_Name[Location=="west_campus" & VP_Name == "2"] <- 'r')
vp<- within(vp, VP_Name[Location=="west_campus" & VP_Name == "3"] <- 'r')
vp<- within(vp, VP_Name[Location=="west_campus" & VP_Name == "5"] <- 'r')
vp<- within(vp, VP_Name[Location=="west_campus" & VP_Name == "7"] <- 'r')
vp<- within(vp, VP_Name[Location=="west_campus" & VP_Name == "12"] <- 'r')


vp<-vp[vp$VP_Name!="3-9" &vp$VP_Name!="created pool by trees"
       &vp$VP_Name!="redtail 2"&vp$VP_Name!="18"&vp$VP_Name!="tadpole 2"
       &vp$VP_Name!="south"&vp$VP_Name!="west pond"&vp$Location!="storke_ranch"
       &vp$VP_Name!="anacapa"&vp$VP_Name!="chumash"&vp$VP_Name!="manzanita"
       &vp$VP_Name!="anacapa east"&vp$VP_Name!="anacapa west"&vp$VP_Name!="redtail 1"
       &vp$VP_Name!="ssw"&vp$VP_Name!="I"&vp$VP_Name!="j"&vp$VP_Name!="k"
       &vp$VP_Name!="l"&vp$VP_Name!="r"&vp$VP_Name!="redtail 4"&vp$VP_Name!="sww",]

vp_locations<-vp[,2:3]

vp_locations<-unique(vp_locations[,c('Location','VP_Name')])
write.csv(vp_locations,"C:/Users/rickard/Documents/Cheadle-Center-Vernal-Pools/vp_locations.csv")

#Get wateryear function
getYearQuarter <- function(x,
                           firstMonth=10,
                           fy.prefix='WY',
                           quarter.prefix='Q',
                           sep='-',
                           level.range=c(min(x), max(x)) ) {
  if(level.range[1] > min(x) | level.range[2] < max(x)) {
    warning(paste0('The range of x is greater than level.range. Values ',
                   'outside level.range will be returned as NA.'))
  }
  quarterString <- function(d) {
    year <- as.integer(format(d, format='%Y'))
    month <- as.integer(format(d, format='%m'))
    y <- ifelse(firstMonth > 1 & month >= firstMonth, year+1, year)
    q <- cut( (month - firstMonth) %% 12, breaks=c(-Inf,2,5,8,Inf),
              labels=paste0(quarter.prefix, 1:4))
    return(paste0(fy.prefix, substring(y,3,4)))
  }
  vals <- quarterString(x)
  levels <- unique(quarterString(seq(
    as.Date(format(level.range[1], '%Y-%m-01')),
    as.Date(format(level.range[2], '%Y-%m-28')), by='month')))
  return(factor(vals, levels=levels, ordered=TRUE))
} 
unique(vp$VP_Name)
#Run function with our data
vp$date<-format(vp$Date.Time,format="%Y-%m-%d")
vp$date<-as.POSIXct(vp$date,format="%Y-%m-%d")
vp$wtr_yr <- getYearQuarter(vp$date, firstMonth=10)
vp$wk<-format(vp$Date.Time,format="%W")

write.csv(vp,"C:/Users/rickard/Documents/Cheadle-Center-Vernal-Pools/vp_clean.csv")


##################################################################################
#########################Water Quality############################################


WQ<-read.csv("VP_hydrology/Vernal_Pool_Water_Quality_Monitoring_0.csv")
WQ<-WQ[,c(3,4,7,9,10,11,12,13)]
colnames(WQ)<-c("Date","Location","VP_Name","DO_mg_L","DO_per","Conductivity_us_cm",
                "Salinity_ppt","Temp_C")
WQ$Date<-as.POSIXct(WQ$Date, format="%m/%d/%Y %I:%M:%S %p")
WQ<-WQ[WQ$Location!="more_mesa" & WQ$Location!="copr" & 
         WQ$Location!="Ellwood_mesa"& WQ$Location!="sierra_madre"
       & WQ$Location!="ellwood_mesa",]

WQ<- within(WQ, VP_Name[VP_Name == "Santa Rosa (MSRA, MSRB)"] <- 'Santa Rosa')
WQ<- within(WQ, VP_Name[VP_Name == "Santa Catalina (MSCA, MSCB)"] <- 'Santa Catalina')
WQ<- within(WQ, VP_Name[VP_Name == "Santa Barbara (MSBA, MSBB)"] <- 'Santa Barbara')
WQ<- within(WQ, VP_Name[VP_Name == "San Miguel (MSMA,MSMB)"] <- 'San Miguel')
WQ<- within(WQ, VP_Name[VP_Name == "NCOS1"] <- '1')
WQ<- within(WQ, VP_Name[VP_Name == "NCOS8"] <- '8')
WQ<- within(WQ, VP_Name[VP_Name == "Pool 6"] <- '6')
WQ<- within(WQ, VP_Name[VP_Name == "Pool E"] <- 'E')
WQ<- within(WQ, VP_Name[VP_Name == "Pool W"] <- 'W')
WQ<- within(WQ, VP_Name[VP_Name == "NP9"] <- '9')
WQ<- within(WQ, VP_Name[VP_Name == "NCOS4"] <- '4')
WQ<- within(WQ, VP_Name[VP_Name == "Pool N"] <- 'N')
WQ<- within(WQ, VP_Name[VP_Name == "NCOS2"] <- '2')
WQ<- within(WQ, VP_Name[VP_Name == "NP19"] <- '19')
WQ<- within(WQ, VP_Name[VP_Name == "Santa Catalina "] <- 'Santa Catalina')

write.csv(WQ,"VP_hydrology/VP_WQ_Clean.csv")

