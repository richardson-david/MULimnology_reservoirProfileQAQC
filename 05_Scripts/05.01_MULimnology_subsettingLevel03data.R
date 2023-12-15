##Script 04.01: Loading and selecting specific level3 profiles for MU students####
##Created 26Sep2023 by David Richardson (hereafter DCR)
##This loads level 3 summary data from all the years, then subsets specific profiles####

#Libraries
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(lubridate)) {install.packages("lubridate")}
if (!require(stringr)) {install.packages("stringr")}

#Load packages
library(tidyverse)
library(lubridate)
library(stringr)

#Run functions script to upload all the user defined functions####
source("05_Scripts/00_MULimnology_reservoirProfileQAQC_Functions.R")

#*Set the directory path here####
dirPath<-paste0("03_Level3_Data/")

#Identify all the individual .csv files####
Level3_files<-list.files(dirPath,pattern = "*.csv")

#Initialize storage location####
List_Level3<-list()

#Loop through all the level3 files and load them individually####
#debug fileIndex<-1
for(fileIndex in 1:length(Level3_files)){
  List_Level3[[fileIndex]]<-read_csv(file=paste0(dirPath,Level3_files[fileIndex]), col_types = cols()) #last argument suppresses the message on input about column types, helpful for mass upload
}

#Bind them all together####
Level3_allData<-do.call(bind_rows, List_Level3)


############################Lat log matching#####################
#Read in lat/long data#####
latlongMetadata<-read_csv("07_MiscFiles/MissouriReservoir_Metadata_SiteData.csv")%>%mutate(MULakeNumber=as.character(sprintf("%03d", MULakeNumber)))%>%mutate(notes=NA)

#Find the unique list of the Level3 data####
#Take the mean of lat and long
handheldLatLong<-Level3_allData%>%group_by(MULakeNumber)%>%summarize(site_latitude=mean(site_latitude,na.rm=TRUE), site_longitude=mean(site_longitude,na.rm=TRUE))

#Find ones that are not in the main lat/longs#####
Extras<-handheldLatLong%>%
        filter(!(MULakeNumber %in% latlongMetadata$MULakeNumber))%>%
        rename(waterBodyLatitude=site_latitude,
               waterBodyLongitude=site_longitude)%>%
        mutate(notes=ifelse(is.nan(waterBodyLatitude),paste("See MULakeNumber: ",substr(MULakeNumber,1,3)," for lake location. Email corresponding author for exact site location",sep=""),NA))


#Stack and sort the two lat/longs, export the file####
combined_LatLong<-bind_rows(latlongMetadata,Extras)%>%arrange(MULakeNumber)

#Export the file to 07_MiscFiles folder####
write_csv(x=combined_LatLong,file="07_MiscFiles/MissouriReservoir_Metadata_SiteData_full.csv")

###################################################################################

#Load in Yusuf's list (or any other list here)####
Yusuf_profiles_list<-read_csv("07_MiscFiles/Updated_Specific_Lakes_and_Dates_YusufOlaleye.csv")%>%
                rename(MULakeNumber=`Lake#`,
                       ReservoirName=`Reservoir Name`,
                       date=Dates)%>%
                mutate(MULakeNumber=sprintf("%03d",MULakeNumber),
                       date=mdy(date))

#Merge all them together keeping Yusuf's profiles####              
Yusuf_profiles<-left_join(Yusuf_profiles_list,Level3_allData,by=c("MULakeNumber","date"))

#Write out Yusuf's profiles####
write_csv(Yusuf_profiles,file="06_Outputs/YusufOlaleye_Level3Profiles.csv")



##################################################
#Subset to data with lats/longs#########################
filterLatLongs<-Level3_allData%>%
  filter((!is.na(site_latitude))&(!is.na(site_longitude)))%>% #reduce to ones with Lat/Longs in there
  dplyr::select(MULakeNumber,site_latitude,site_longitude)%>% 
  group_by(MULakeNumber)%>%
  summarize(site_latitude=mean(site_latitude),
            site_longitude=mean(site_longitude))
write_csv(x=filterLatLongs,file="06_Outputs/LakeYSIHandheldLatLong.csv")

#Read in metadata from the database####
metaDataFromDatabase<-read_csv("07_MiscFiles/MissouriReservoir_Metadata_SiteData.csv")%>%mutate(MULakeNumber=as.character(sprintf("%03d", MULakeNumber)))

#Merge them together
all_latlong<-left_join(metaDataFromDatabase,filterLatLongs,by="MULakeNumber")

#Output the merged lat/longs####
write_csv(x=all_latlong%>%dplyr::select(-notes),file="06_Outputs/LakeLatLongMetadata.csv")

#Subset to the non-NA data####
#There are are 97 of them####
##Find ones where the lats OR longs are more than 0.005 away######
all_latlong%>%
  filter((!is.na(site_latitude))&(!is.na(site_longitude)))%>%
  filter((abs(waterBodyLatitude-site_latitude)>0.005)|(abs(waterBodyLongitude-site_longitude)>0.005))

#10 are identified:
#The following are problematic c("043","139")
temp<-Level3_allData%>%filter(MULakeNumber%in%c("139"))%>%dplyr::select(date,site_latitude,site_longitude)%>%print(n=Inf)


########################Figure out the distributions of the depths for each lake####################
#Get teh unique list of sites####
uniqueLakeNumbers<-sort(unique(Level3_allData$MULakeNumber))

#Lake.index=1
#Export a histogram of the maxdepth for each profile by site as a page####
pdf(paste0("06_Outputs/Level3_MaxDepth_plots.pdf"), onefile = TRUE,width=8.5,height=11)

#Go through all the lakes####
#lake.index=1
for(lake.index in 1:length(uniqueLakeNumbers)){
  temp<-Level3_allData%>%filter(MULakeNumber==uniqueLakeNumbers[lake.index]) #Extract the data just for that lake
  gg.temp<-ggplot(data=temp,aes(x=maxDepth_m))+geom_histogram()+labs(title=paste0("MULakeNumber: ",uniqueLakeNumbers[lake.index])) #create the histogram plot####
  print(gg.temp)
}

dev.off()

#Check 149####
Level3_allData%>%filter(MULakeNumber%in%uniqueLakeNumbers[which(substr(uniqueLakeNumbers,1,3)=="149")])%>%print(n=Inf)
####
