##Level4: Merging in other datasets to the summary level 03 data Missouri Reservoirs####
##Created 15Jan2024 by David Richardson (hereafter DCR)
##Reads in level 3 data from GIT, reads in Secchi and merges together####

#Libraries
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(elevatr)) {install.packages("elevatr")}

#Load packages
library(tidyverse)
library(elevatr)

#*Set the directory path here####
dirPathSecchi<-paste0("00_Level0_Data/Secchi_Level0_RawData/")

#Run functions script to upload all the user defined functions####
source("05_Scripts/00_MULimnology_reservoirProfileQAQC_Functions.R")

#Read in the Secchi data from the past####
secchiDF_1976_2022<-read_csv(file=paste0(dirPathSecchi,"/","1976-2022_SLAP_SecchiDepth_Masterfile_01-11-2024.csv"), col_types = cols())%>% #last argument suppresses the message on input about column types, helpful for mass upload
                    mutate(
                          MULakeNumber=sprintf("%03d", MULakeNumber),
                          date=as_date(mdy_hm(beginDateTime))
                          )%>%
                    dplyr::select(MULakeNumber,date,SecchiDepth_m)

  #Check the original Secchi upload to see about some individual data rows####
  tempSecchi<-read_csv(file=paste0(dirPathSecchi,"/","1976-2022_SLAP_SecchiDepth_Masterfile_01-11-2024.csv"), col_types = cols())%>% #last argument suppresses the message on input about column types, helpful for mass upload
    mutate(
      MULakeNumber=sprintf("%03d", MULakeNumber),
      date=as_date(mdy_hm(beginDateTime))
    )
  #Filter out specific sites here####
  tempSecchi%>%filter(MULakeNumber==438)%>%print(n=Inf)

#Read in the Secchi data from 2023 only####
secchiDF_2023<-read_csv(file=paste0(dirPathSecchi,"/","2023_SLAP_SecchiDepth_Masterfile_01-11-2024.csv"), col_types = cols())%>% #last argument suppresses the message on input about column types, helpful for mass upload
  mutate(
    MULakeNumber=sprintf("%03d", MULakeNumber),
    date=as_date(mdy(Date))
  )%>%
  dplyr::select(MULakeNumber,date,SecchiDepth_m)

#Merge the two Secchis together (include other years here)####
secchiDF<-bind_rows(secchiDF_1976_2022,secchiDF_2023)%>%
            group_by(MULakeNumber,date)%>%
            summarize(SecchiDepth_m=mean(SecchiDepth_m))%>% #*Collapse any replicates for date/MULakeNumber by taking the mean####       
            ungroup()

#*Set the directory path here####
dirPath_Level03<-paste0("03_Level3_Data/")

#Identify all the individual .csv files####
Level3_files<-list.files(dirPath_Level03,pattern = "*.csv")

#Initialize storage location####
List_Level3<-list()

#Loop through all the level3 files and load them individually####
#debug fileIndex<-1
for(fileIndex in 1:length(Level3_files)){
  List_Level3[[fileIndex]]<-read_csv(file=paste0(dirPath_Level03,Level3_files[fileIndex]), col_types = cols()) #last argument suppresses the message on input about column types, helpful for mass upload
}

#Bind them all together####
Level3_allData<-do.call(bind_rows, List_Level3)

#Merge Secchi with Level3_alldata (Level4_alldata)####
  #*Check to see what secchis have a profile####
  temp<-left_join(secchiDF,Level3_allData%>%dplyr::select(MULakeNumber,date,numberOfMeasurements_temperature),by=c("MULakeNumber","date"))
  temp%>%filter(is.na(numberOfMeasurements_temperature))

#Join the level 3 data with Secchi data frame  
Level4_allData<-left_join(Level3_allData,secchiDF,by=c("MULakeNumber","date"))
#Check to see what profiles are missing a secchi####
  #*Only 194 out of 10235 profiles have no Secchi
  Level4_allData%>%filter(is.na(SecchiDepth_m))%>%print(n=Inf)

#############################################################
####This code to get elevations for individual sites takes awhile to run####
####It was run through EDI 2023 and was exported. It can be uncommented and rerun as needed####
####For now, the code below this reads in the csv file that was exported####
#############################################################
#Join in the lat/longs####
#*Read in lat/long data#####
#latlongMetadata<-read_csv("04_EDI/MissouriReservoirs_Metadata_SiteData.csv")
# #*Create a data frame of only the lats and longs####                                        
# latlongdf<-data.frame(x=latlongMetadata$waterBodyLongitude_degree,y=latlongMetadata$waterBodyLatitude_degree)                                                             
# 
# #Using the elevatr package, we are going to pull the elevation for each lat/long in the metadata file####
# set.seed(65.7)
# crs_dd <- 4326 #sets the projection for the map
# elevation_m_vector<-rep(NA,nrow(latlongdf)) #makes an NA vector
# #*Goes through all the lats and longs####
# for(i in 1:nrow(latlongdf)){
#   #*Check first to see if lat/long exist, if not, then leave the elevation as NA####  
#   if(is.na(latlongdf$x[i])|is.na(latlongdf$y[i])){}else{
#     #**Get the elevation, suppress messages so that it streamlines a little. This will take a few minutes####
#     elevation_m_vector[i]<-suppressMessages(get_elev_point(latlongdf[i,],prj = crs_dd, src = "epqs")$elevation)
#   }  #End of if else 
#   print(i)
# } #end of for loop
# 
# #Bind them together
# latlongelevMetadata<-bind_cols(latlongMetadata,tibble(elevation_m_extract=elevation_m_vector))
# 
# #Write it out
# write_csv(latlongelevMetadata,"06_Outputs/MissouriReservoirs_Metadata_SiteData_LatLongElev.csv")
#########################################################################

#Read in the exported csv file that includes EDI lat longs plus elevations
latlongelevMetadata<-read_csv("06_Outputs/MissouriReservoirs_Metadata_SiteData_LatLongElev.csv")

#*Join lat/long metadata with all the level4 data#### 
Level4_allData<-left_join(Level4_allData,latlongelevMetadata,by="MULakeNumber")

#*Create column of latitude and longitude that uses handheld first if it exists, if not then site data####
Level4_allData<-Level4_allData%>%mutate(latitude_degree_best=ifelse(!is.na(site_latitude),site_latitude,waterBodyLatitude_degree),
                                        longitude_degree_best=ifelse(!is.na(site_longitude),site_longitude,waterBodyLongitude_degree),
                                        elevation_m_best=ifelse(!is.na(site_altitude_m),site_altitude_m,elevation_m_extract)
                                        )
#Check to see if the extracted elevations from the library elevatr match the handheld####
    #ggplot(data=Level4_allData%>%dplyr::select(site_altitude_m,elevation_m_extract,elevation_m_best),aes(x=site_altitude_m,y=elevation_m_extract))+geom_point()+geom_abline(slope=1,intercept=0)

#Calculate E0 for all the days in the data frame####
#*This step takes a few minutes to run through#
Level4_allData$E0_umolpm2s<-NA #Initialize E0 column
for(dateIndex in 1:nrow(Level4_allData)){
  Level4_allData$E0_umolpm2s[dateIndex]<-EO_dailyMean_single(date=Level4_allData$date[dateIndex],latitude=Level4_allData$latitude_degree_best[dateIndex],longitude=Level4_allData$longitude_degree_best[dateIndex],elevation_m=Level4_allData$elevation_m_best[dateIndex])
} #End of loop through all the rows

#Calculate kd for each secchi####
Level4_allData<-Level4_allData%>%
                mutate(kd_1_m=1.44/SecchiDepth_m)%>%
                mutate(E24_umolpm2s=E0_umolpm2s*(1-exp(-1*kd_1_m*top_metalimnion_m))*((kd_1_m*top_metalimnion_m)^-1))

  #Stats for E24####
  sum(!is.na(Level4_allData$E24_umolpm2s))
  mean(Level4_allData$E24_umolpm2s,na.rm=TRUE)
  median(Level4_allData$E24_umolpm2s,na.rm=TRUE)
  max(Level4_allData$E24_umolpm2s,na.rm=TRUE)
  min(Level4_allData$E24_umolpm2s,na.rm=TRUE)
ggplot(data=Level4_allData,aes(x=E24_umolpm2s))+geom_histogram(color="black",fill="white")


#Create a file name with year range####
Level4_fileName<-paste0("Level4Data_",min(year(Level4_allData$date)),"-",max(year(Level4_allData$date)),".csv")
#Export the Level 4 data 
write_csv(Level4_allData,file=paste0("06_Outputs/",Level4_fileName))
          