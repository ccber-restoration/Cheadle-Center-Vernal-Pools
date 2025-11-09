install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(dplyr)
#First upload rain data and get max and total
noaa<-read.csv("Monthly_NOAA.csv")
noaayrly<-aggregate(Precip_mm~Rain.Year,noaa,FUN=sum)
noaayrly<-noaayrly[noaayrly$Rain.Year>=2019,]
noaayrly$wtr_yr<-c("WY19","WY20","WY21","WY22","WY23","WY24")

#Load clean vp data
vp<-read.csv("vp_clean.csv")
vpq<-vp[vp$Water_Level!=0,]
#############################
##Temperary until 2024 water year is over
vpq<-vpq[vpq$wtr_yr!="WY24",]

#Water depth is supposed to be taken once a week, so 7 days is applied to each measurement
#In the future we should change this to actually count the days between measurements
vpq$days<-7
#Summing the total number of days that is inundated
vpqag<-aggregate(days~Location+VP_Name+wtr_yr,vpq,FUN=sum)
#Finding the max depth per year per pool
vpmax<-aggregate(Water_Level~Location+VP_Name+wtr_yr,vp,FUN=max)
vpqag


#Merge vp and rainfall data
vpall<-merge(vpqag,noaayrly,by="wtr_yr",all.x=TRUE,all.y=TRUE)
vpall<-vpall[!is.na(vpall$VP_Name),]
str(vpall)
coeff<-0.2
vpall$Precip_in<-vpall$Precip_mm/25.4
vpall$Precip_cm<-vpall$Precip_mm/10
lubridate::ymd(vpall$Rain.Year, truncated = 2L)

coeff<-0.5
ggplot(vpall,aes(x=Rain.Year)) +
  geom_bar(stat="identity", position=position_dodge(),aes(y=days, fill=VP_Name))+ylab("Total Days Inundated")+xlab("Vernal Pool Name")+ 
  scale_y_continuous(limits=c(0,250),name = "First Axis",sec.axis = sec_axis( trans=~.*coeff, name="Second Axis"))+
  theme_bw()+geom_line(color="lightblue",size=1.5,aes(y=Precip_cm/coeff))+geom_point(aes(y=Precip_cm/coeff))+
theme(axis.title.y = element_text(size=18), 
      axis.title.y.right = element_text(size=15),
      axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
      axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
      strip.text.y = element_text(size = 18),
      legend.text = element_text(size=12),
      legend.title = element_text(size=12),
      legend.position = "right")+ggtitle(p$Location)

for (i in unique(vpall$Location)){
  p<-vpall[vpall$Location==i,]
  q<- ggplot(p,aes(x=Rain.Year)) +
    geom_bar(stat="identity", position=position_dodge(),aes(y=days, fill=VP_Name))+ylab("Total Days Inundated")+xlab("Vernal Pool Name")+ 
    scale_y_continuous( name = "Water Year Rainfall (Inches)")+
  theme_bw()+geom_line(color="lightblue",size=1.5,aes(y=Precip_cm/coeff))+geom_point(aes(y=Precip_cm))+
    theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
          axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
          axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
          axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
          strip.text.y = element_text(size = 18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "right")+ggtitle(p$Location)+
    guides(fill=guide_legend(ncol=2))
  print(q)
}

for (i in unique(vpall$Location)){
  p<-vpall[vpall$Location==i,]
  q<- ggplot(p,aes(x=Rain.Year)) +
    geom_bar(stat="identity", position=position_dodge(),aes(y=days, fill=VP_Name))+ylab("Total Days Inundated")+xlab("Vernal Pool Name")+ 
    theme_bw()+geom_line(color="lightblue",size=1.5,aes(y=Precip_cm))+geom_point(aes(y=Precip_cm))+
    theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
          axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
          axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
          axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
          strip.text.y = element_text(size = 18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "right")+ggtitle(p$Location)+
    guides(fill=guide_legend(ncol=2))
  print(q)
}


vpmax<-vpmax[vpmax$Water_Level<90,]
for (i in unique(vpmax$Location)){
  p<-vpmax[vpmax$Location==i,]
  q<- ggplot(p,aes(x=wtr_yr)) +
    geom_bar(stat="identity", position=position_dodge(),aes(y=Water_Level, fill=VP_Name))+ylab("Maximum Water Level")+xlab("Vernal Pool Name")+ 
    theme_bw()+
    theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
          axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
          axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
          axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
          strip.text.y = element_text(size = 18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "right")+ggtitle(p$Location)+
    guides(fill=guide_legend(ncol=2))
  print(q)
}



vpall
vpmax
head(vpall)
head(vpmax)
vp2<-merge(vpall,vpmax,by=c("Location","VP_Name","wtr_yr"))
vp2<-vp2[vp2$Water_Level<100,]
vp3max<-aggregate(cbind(days,Water_Level)~Location,vp2,FUN=mean)
dsvp<-vp2[vp2$Location=="delsol_caminocorto",]

plot(vp2$Water_Level~vp2$days)
plot(dsvp$Water_Level~dsvp$days)

for (i in unique(vp2$wtr_yr)){
  p<-vp2[vp2$wtr_yr==i,]
  q<- ggplot(p,aes(x=Water_Level)) +
    geom_point(aes(y=days, color=Location))+ylab("Maximum Water Level")+xlab("Days inundated")+ 
    theme_bw()+
    theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
          axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
          axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
          axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
          strip.text.y = element_text(size = 18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "right")+ggtitle(p$wtr_yr)+
    guides(fill=guide_legend(ncol=2))
  print(q)
}


#Now get max water level ever
max(vp2$Water_Level)
vp2 %>% slice_max(Water_Level)

vp3<-vp2 %>%
  select(Water_Level, days, wtr_yr,Location,VP_Name) %>%
  group_by(Location,VP_Name) %>%
  filter(Water_Level==max(Water_Level)) %>%
  ungroup()
vp3<-vp3[vp3$Location!="storke_ranch",]

vp4<-vp2 %>%
  select(Water_Level, days, wtr_yr,Location) %>%
  group_by(Location) %>%
  filter(Water_Level==max(Water_Level)) %>%
  ungroup()

ggplot(vp3,aes(x=Water_Level)) +
  geom_point(aes(y=days, shape=Location,color=wtr_yr,size=2),size=2)+ylab("Days inundated")+xlab("Maximum Water Level")+ 
  theme_bw()+
  theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
        axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
        axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
        axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
        strip.text.y = element_text(size = 18),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "right")+ggtitle("Maximum Water Level over all years")

ggplot(vp3,aes(x=Water_Level)) +
  geom_point(aes(y=days, color=wtr_yr,size=2),size=2)+ylab("Days inundated")+xlab("Maximum Water Level")+ 
  theme_bw()+
  theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
        axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
        axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
        axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
        strip.text.y = element_text(size = 18),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "right")+ggtitle("Maximum Water Level over all years")
