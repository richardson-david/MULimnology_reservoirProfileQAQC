##Script 04: Profile validation with record from chemistry sampling####
##Created 26Sep2023 by David Richardson (hereafter DCR)
##This does a check between the logs, profile data from level 3, and the chemistry log from Tony####

#Libraries
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(lubridate)) {install.packages("lubridate")}

#Load packages
library(tidyverse)
library(lubridate)

#Read in the chemistry record####
chemistry_record<-read_csv("07_MiscFiles/Profile validation check_19Sep2023.csv")

#Format the records to include a merged MUlakenumber, dateTime stamp####
chemistry_record<-chemistry_record%>%
              rename(dateTime=beginDateTime)%>%
              mutate(MULakeNumber=sprintf("%03d",MULakeNumber),
                     merged_ProfileID=paste0(MULakeNumber,"-",dateTime))%>%
              mutate(year=year(dateTime))

#Specify a year, can maybe loop
#year.index<-2017

#Initialize storage location####
List_inChemistrynotLog<-list() #in chemistry but not log
List_inLognotChemistry<-list() #in log but not in chemistry
List_inChemistrynotLevel3<-list() #in Chemistry but not in Level 3
List_inLevel3notChemistry<-list() #in Level 3 but not in chemistry

#Loop through the years and find the mismatched profiles####
for(year.index in 2017:2022){
list.index<-year.index-2016 #THis just makes the list go from 1:6 
#Find the chemistry records for this year####
chemistry_record_thisyear<-chemistry_record%>%filter(year==year.index)

#Read in a log####
year_log<-read_csv(paste0("06_Outputs/",year.index,"_QAQC_log.csv"))%>%mutate(merged_ProfileID=paste0(MULakeNumber,"-",year,"-",month,"-",day))
          
#Read in Level 3 data####
level3_data<-read_csv(paste0("03_Level3_Data/",year.index,"_Level3.csv"))%>%mutate(merged_ProfileID=paste0(MULakeNumber,"-",date))

#Compare chemistry to log - 
  #Is in the log but not in chemistry
  List_inLognotChemistry[[list.index]]<-full_join(chemistry_record_thisyear,year_log,by="merged_ProfileID")%>%filter(is.na(waterBody))%>%dplyr::select(merged_ProfileID,Level0_profiles)
  #Is in the chemistry but not in the log
  List_inChemistrynotLog[[list.index]]<-full_join(chemistry_record_thisyear,year_log,by="merged_ProfileID")%>%filter(is.na(Level0_profiles))%>%dplyr::select(merged_ProfileID,sampleID)

#Compare to the Level3 summary data
  #Is in Level3 but not in chemistry
  List_inLevel3notChemistry[[list.index]]<-full_join(chemistry_record_thisyear,level3_data,by="merged_ProfileID")%>%filter(is.na(waterBody))%>%dplyr::select(merged_ProfileID)
  #Is in the chemistry but not in the Level3
  List_inChemistrynotLevel3[[list.index]]<-full_join(chemistry_record_thisyear,level3_data,by="merged_ProfileID")%>%filter(is.na(date))%>%dplyr::select(merged_ProfileID,sampleID)

}  

#Bind the columns with data in the logs/level3 but not chemistry and vice versa####

inLognotChemistry<-do.call(bind_rows, List_inLognotChemistry)
inChemistrynotLog<-do.call(bind_rows, List_inChemistrynotLog)

inLevel3notChemistry<-do.call(bind_rows, List_inLevel3notChemistry)
inChemistrynotLevel3<-do.call(bind_rows, List_inChemistrynotLevel3)
