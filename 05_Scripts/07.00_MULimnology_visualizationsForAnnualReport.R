
#Libraries
if (!require(maps)) {install.packages("maps")}
if (!require(ggmap)) {install.packages("ggmap")}

library(maps)
library(ggmap)
library(tidyverse)

#Read in state data
state<-map_data('state')  
head(state)

#Read in county data
county<-map_data('county')

#Read in level 4 data####
Level4_data<-read_csv(file=paste0("06_Outputs/Level4Data_1989-2023.csv"))

#Summarize level 4 by site####
#This is averaging over all the years, could subset for 2023 here####
#Or merge with TP####
Level4_summary<-Level4_data%>%group_by(MULakeNumber)%>%summarize(maxDepth_m=mean(maxDepth_m,na.rm=TRUE),
                                                 latitude_degree_best=mean(latitude_degree_best,na.rm=TRUE),
                                                 longitude_degree_best=mean(longitude_degree_best,na.rm=TRUE),
                                                 E24_umolpm2s=mean(E24_umolpm2s,na.rm=TRUE))

#Graph####
ggplot()+
  #geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='dark grey')+
  geom_polygon(data=state%>%filter(region=="missouri"),aes(x=long,y=lat),color='black',fill="light grey")+
  coord_quickmap()+
  geom_polygon(data=county%>%filter(region=="missouri"),aes(x=long,y=lat,group=subregion),color='black',fill="light grey")+
  geom_point(data=Level4_summary%>%filter(latitude_degree_best<41),aes(x=longitude_degree_best,y=latitude_degree_best,fill=E24_umolpm2s),shape=21,size=3)


