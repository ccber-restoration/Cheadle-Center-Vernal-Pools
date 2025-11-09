library(tidyverse)


#read in (nominally) clean water quality data
WQclean<-read.csv("VP_WQ_Clean.csv") %>% 
  #reformat date column
  #note, gives warnings because does not specify timezone
   mutate(Date = as.POSIXct(Date,format<-"%Y-%m-%d %H:%M:%S")) %>% 
  #do some filtering out extreme values
  filter(
    Salinity_ppt < 100 &
    WQclean$DO_mg_L<300 &
    #keep only temps less than 100
    Temp_C<100
           ) %>% 
  #remove NA values for Salinity
  filter(!is.na(Salinity_ppt))
  
#explore
str(WQclean)
head(WQclean)
  
#check unique values of salinity
unique(WQclean$Salinity_ppt)

# loop for each location, make a plot of salinity
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


#for each location, make a plot of DO
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


#for each location, make a figure of temperature
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

