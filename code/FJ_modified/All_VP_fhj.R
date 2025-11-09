library(ggplot2)
getwd()
vp<-read.csv("~/VP_hydrology/CCBER_Vernal_Pool_Hydrology_0.csv")
vp<-vp[,c(3,4,5,7,8)]
vp<-subset(vp,vp$Location=="ncos")
vp$Date<-as.POSIXct(vp$Date, format="%m/%d/%Y %H:%M")
vp<-subset(vp,vp$Vernal.Pool.Name.or.ID!="West pond"&vp$Vernal.Pool.Name.or.ID!="West Pond" &
vp$Vernal.Pool.Name.or.ID!="West pond ")
colnames(vp)<-c("Date.Time","Location","Unlisted_location","VP_Name","Water_Level")
vp$Date<-paste0(substr(vp$Date.Time,1,10))
vp$Date<-as.Date(vp$Date,format="%Y-%m-%d")
vpalllyears<-vp
vp<-vp[vp$Date>="2020-10-01 20:00:00"&vp$Date<="2021-10-01 20:00:00",]

Rain<-read.csv("NOAAhourly_precip_WY2021.csv")
dailyrain<-aggregate(Precip_Calc_mm~Date,Rain,FUN=sum)
dailyrain$Date<-as.Date(dailyrain$Date,format="%Y-%m-%d")
vprain<-dailyrain[dailyrain$Date>="2021-01-27"& dailyrain$Date<="2021-04-09",]
vprain$Precip_Inch<-vprain$Precip_Calc_mm*0.0393701
vpall<-merge(vp,vprain,by="Date",all.x=TRUE,all.y=TRUE)
vpall<-vpall[!is.na(vpall$VP_Name),]
str(vpall)
coeff<-0.2
ggplot(vpall,aes(x=Date))+
  geom_line(aes(y=Water_Level,color=VP_Name), size=2)+theme_bw()+ggtitle("NCOS Vernal Pool Hydrology- 2021 Water Year")+
  ylab("Water Level (in)")+geom_bar(data=vprain,aes(y=Precip_Inch/coeff),stat="identity",size=.1, fill="blue", color="black",alpha=0.8)+
  scale_y_continuous( name = "Water Depth (Inches)",
                      sec.axis = sec_axis(~.*coeff, name=""))+ylab("Date")+
  theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
        axis.title.x=element_text(size=18), axis.text.x=element_text(size=15),
        axis.text.y = element_text(size=18),title =element_text(size=24, face='bold'),
        strip.text.y = element_text(size = 20),
        legend.text = element_text(size=15),
        legend.position = c(.95,.8))
vpalllyears
write.csv(vpalllyears,"~/NCOS Hydrology/Vernal Pools/vpallyears.csv")
str(vpalllyears)
vpalllyears$year<-format(vpalllyears$Date,format="%Y")
vpalllyears<-vpalllyears[vpalllyears$VP_Name!="9",]

ggplot(data=vpalllyears, aes(x=VP_Name, y=Water_Level, fill=year)) +
  geom_bar(stat="identity", position=position_dodge())


test1 <- as.Date("2019-12-06")
test2 <- strptime("2020-04-16", format="%Y-%m-%d", tz="UTC")
difftime(as.POSIXct(test2), as.POSIXct(test1, tz="UTC"), units="days")
# Time difference of 1 days
getYearQuarter <- function(x,
                           firstMonth=7,
                           fy.prefix='FY',
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

vpalllyears$wtr_yr <- getYearQuarter(vpalllyears$Date, firstMonth=10)


vpinundation<-read.csv("VP Days inundated.csv")
vpinundation$VP.Name<-as.character(vpinundation$VP.Name)

ggplot(data=vpinundation, aes(x=VP.Name, y=Total.days.inudated, fill=Water.Year)) +
  geom_bar(stat="identity", position=position_dodge())+ylab("Total Days Inundated")+xlab("Vernal Pool Name")+ theme_bw()+
  theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
        axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
        axis.text.y = element_text(size=18),title =element_text(size=22, face='bold'),
        strip.text.y = element_text(size = 22),
        legend.text = element_text(size=18),
        legend.position = c(0.75, 0.85))+ggtitle("Vernal Pool Inundation by Year")


