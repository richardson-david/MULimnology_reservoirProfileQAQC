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

#17 are identified:
#The following are problematic c("011","070","089","098","139","179","180","185","213")
temp<-Level3_allData%>%filter(MULakeNumber%in%c("139"))%>%dplyr::select(date,site_latitude,site_longitude)%>%print(n=Inf)

