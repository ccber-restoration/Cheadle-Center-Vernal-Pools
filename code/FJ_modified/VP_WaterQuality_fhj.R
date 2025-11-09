WQclean<-read.csv("VP_hydrology/VP_WQ_Clean.csv")
str(WQclean)
head(WQclean)
WQclean$Date<-as.POSIXct(WQclean$Date,format<-"%Y-%m-%d %H:%M:%S")
WQclean<-WQclean[WQclean$Salinity_ppt<100&WQclean$DO_mg_L<300&WQclean$Temp_C<100,]
str(WQclean)
unique(WQclean$Salinity_ppt)
WQclean<-WQclean[!is.na(WQclean$Salinity_ppt),]

for (i in unique(WQclean$Location)){
  p<-WQclean[WQclean$Location==i,]
  q<- ggplot(p,aes(x=Date)) +
    geom_line(size=1.5,aes(y=Salinity_ppt, color=VP_Name))+geom_point(aes(y=Salinity_ppt))+
    ylab("Salinity (ppt)")+xlab("Date")+theme_bw()+ggtitle(p$Location)+
    theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
          axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
          axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
          axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
          strip.text.y = element_text(size = 18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "right")+ggtitle(p$Location)
  print(q)
}


for (i in unique(WQclean$Location)){
  p<-WQclean[WQclean$Location==i,]
  q<- ggplot(p,aes(x=Date)) +
    geom_line(size=1.5,aes(y=DO_mg_L, color=VP_Name))+geom_point(aes(y=DO_mg_L))+
    ylab("Dissolved Oxygen (mg/L)")+xlab("Date")+theme_bw()+ggtitle(p$Location)+
    theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
          axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
          axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
          axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
          strip.text.y = element_text(size = 18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "right")+ggtitle(p$Location)
  print(q)
}



for (i in unique(WQclean$Location)){
  p<-WQclean[WQclean$Location==i,]
  q<- ggplot(p,aes(x=Date)) +
    geom_line(size=1.5,aes(y=Temp_C, color=VP_Name))+geom_point(aes(y=Temp_C))+
    ylab("Temperature (C)")+xlab("Date")+theme_bw()+ggtitle(p$Location)+
    theme(axis.title.y = element_text(size=18,margin = margin(t = 0, r = 10, b = 0, l = 20)), 
          axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
          axis.title.x=element_text(size=18), axis.text.x=element_text(size=18),
          axis.text.y = element_text(size=18),title =element_text(size=18, face='bold'),
          strip.text.y = element_text(size = 18),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12),
          legend.position = "right")+ggtitle(p$Location)
  print(q)
}

