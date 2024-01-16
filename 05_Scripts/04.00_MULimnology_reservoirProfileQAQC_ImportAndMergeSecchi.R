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

#Read in the Secchi data from 2023 only####
secchiDF_2023<-read_csv(file=paste0(dirPathSecchi,"/","2023_SLAP_SecchiDepth_Masterfile_01-11-2024.csv"), col_types = cols())%>% #last argument suppresses the message on input about column types, helpful for mass upload
  mutate(
    MULakeNumber=sprintf("%03d", MULakeNumber),
    date=as_date(mdy(Date))
  )%>%
  dplyr::select(MULakeNumber,date,SecchiDepth_m)

#Merge the two Secchis together (include other years here)####
secchiDF<-bind_rows(secchiDF_1976_2022,secchiDF_2023)

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

###STOPPED HERE####
#Next things to do####
#Merge Secchi with Level3_alldata (Level4_alldata)
#Calculate E0 for each day
#Calculate E24 for each day