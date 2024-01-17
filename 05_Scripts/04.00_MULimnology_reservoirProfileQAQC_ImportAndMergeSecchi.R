##Level4: Merging in other datasets to the summary level 03 data Missouri Reservoirs####
##Created 15Jan2024 by David Richardson (hereafter DCR)
##Reads in level 3 data from GIT, reads in Secchi and merges together####

#Libraries
if (!require(tidyverse)) {install.packages("tidyverse")}

#Load packages
library(tidyverse)

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


#Calculate E0 for all the days in the data frame####
#*This step takes a few minutes to run through#
Level4_allData$E0_umolpm2s<-NA #Initialize E0 column
for(dateIndex in 1:nrow(Level4_allData)){
  Level4_allData$E0_umolpm2s[dateIndex]<-EO_dailyMean_single(date=Level4_allData$date[dateIndex],latitude=Level4_allData$site_latitude[dateIndex],longitude=Level4_allData$site_longitude[dateIndex],elevation_m=Level4_allData$site_altitude_m[dateIndex])
} #End of loop through all the rows

###STOPPED HERE####
#Next things to do####
#Prior to calculating E0 - merge with Lat/Long metadata to get better estimates of Lat Long
  #Create new column that gets lat/long as the handheld if it exists or the site data if not
  #Use elevation if it exists or default elevation (244) if not
#Calculate E0 for each day
#Calculate kd based on secchi: 
  #kd_1_m=1.44/Secchi_m
#Calculate E24 for each day
  #E24_umolpm2s=E0_umolpm2s*(1-exp(-1*kd_1_m*top_metalimnion_m))*((kd_1_m*top_metalimnion_m)^-1)