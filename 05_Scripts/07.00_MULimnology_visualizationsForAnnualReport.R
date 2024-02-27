##Visualizations for Slap final report for Missouri Reservoirs####
##Created 26Feb2024 by David Richardson (hereafter DCR)
##Creates figures and maps from annual data####

#Libraries
if (!require(maps)) {install.packages("maps")}
if (!require(ggmap)) {install.packages("ggmap")}
if (!require(ggspatial)) {install.packages("ggspatial")}
if (!require(ggrepel)) {install.packages("ggrepel")}


library(maps)
library(ggmap)
library(tidyverse)
library(ggspatial) #For the scale bar and compass on the map
library(ggrepel) #For fancy non-overlapping labels

#Read in state data
state<-map_data('state')  
head(state)

#Read in county data
county<-map_data('county')


#Read in the 2023 database here####
##NOTHING YET - THIS DATABASE COMES FROM RUNNING 06.00 script####
#Resave as waterchem2023
waterChem2023<-databaseImport

#collapse to unique MULakeNumbers
figure1map.df<-waterChem2023%>%
                mutate(MULakeNumber=ifelse(substr(MULakeNumber,start=1,stop=1)=="0",as.character(as.numeric(MULakeNumber)),MULakeNumber))%>% #clean up MULakeNumber - might have to do this earlier
                group_by(MULakeNumber)%>%
                filter(row_number()==1)%>% #get the first row of each group
                filter(nchar(MULakeNumber)<=3&MULakeNumber!="?"&MULakeNumber!="FB"&MULakeNumber!="401") #eliminate any sites with MULakeNumber > 3 characters - that includes FD and other sites, get rid of ?, and Field Blank

#REPORT FIGURE 1: Graph maps of sites with MULakeNumber as the site ID####
#Set the x and y limits
lat_lim <- c(36, 40.5)
lon_lim <- c(-96, -89)

# Let coord_quickmap figure out the correct aspect ratio for export:####
#https://community.rstudio.com/t/aspect-ratio-of-a-plot-produced-with-ggplot2-coord-quickmap/9282/2
coord <- coord_quickmap(xlim = lon_lim, ylim = lat_lim, expand = F)
asp <- coord$aspect(list(x.range = lon_lim, y.range = lat_lim))
asp
#> [1] 0.8185998

#Desired plot width in inches
plot.width<-6
# Calculate height
plot.height.new <- plot.width * asp

gg.MapMULakeNUmber<-ggplot()+
  #geom_polygon(data=state,aes(x=long,y=lat,group=group),fill='white',color='dark grey')+
  geom_polygon(data=state%>%filter(region=="missouri"),aes(x=long,y=lat),color='black',fill="light grey")+
  geom_polygon(data=county%>%filter(region=="missouri"),aes(x=long,y=lat,group=subregion),color='black',fill="white")+
  geom_point(data=figure1map.df,aes(x=samplingSiteLongitude,y=samplingSiteLatitude),shape=21,size=3,fill="dark grey")+
  annotation_scale(location = "bl", width_hint = 0.5) + 
  #coord_equal(xlim = lon_lim, ylim = lat_lim)+
  #coord_map(xlim = lon_lim, ylim = lat_lim)+
  coord_sf(crs = 4326,xlim = lon_lim, ylim = lat_lim)+
  ggspatial::annotation_north_arrow(location = "tr")+
  geom_label_repel(data=figure1map.df,aes(x=samplingSiteLongitude,y=samplingSiteLatitude,label=MULakeNumber),fill="white",size=2,min.segment.length = 0)+
  #coord_equal()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Export the plot as a jpg####
ggsave(gg.MapMULakeNUmber,file="06_Outputs/SLAPReport-Figure1-2023.jpg",width=plot.width,height=plot.height.new,units="in")
