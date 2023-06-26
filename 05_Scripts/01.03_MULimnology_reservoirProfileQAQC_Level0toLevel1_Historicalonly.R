##Level0 for historical only: DataInput, Base QA/QC Missouri Reservoirs####
##Created 26Jun2023 by David Richardson (hereafter DCR)
##Reads in level 0 data from the past data, formats the files, and does a basic QA/QC

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

#Set year here####
year<-"Historical"

#Read in the sensor limits file####
sensorLimits<-read_csv("00_Level0_Data/MissouriReservoirs-YSI_EXO3_SensorLimits.csv")


#*Set the directory path here####
dirPath<-paste0("00_Level0_Data/",year,"_Level0_RawData")

#Identify all the individual .csv files####
Level0_files<-list.files(dirPath,pattern = "*.csv")

#Read in historical data####
historicalData<-read_csv(paste0(dirPath,"/HistoricalProfileData.csv"))%>%
                mutate(dateTime=mdy_hm(beginDateTime), #create a dateTime variable - posixCT
                       year=year(dateTime), #extract the year
                       month=sprintf("%02d",month(dateTime)), #extract and create a month variable with leading 0s
                       day=sprintf("%02d",day(dateTime)), #extract and create a month variable with leading 0s
                       MULakeNumber=sprintf("%03d", MULakeNumber),  #Make MULakeNumber have 3 digits with leading zeroes
                       fileName=paste(MULakeNumber,year,month,day,sep="_"))

#Create log with row for each profile
Level0_files_log<-tibble(Level0_profiles=unique(historicalData$fileName),Level0to1_done="No",Level0to1_done_date=as_date(NA))%>% #Marks that Level 0 to 1 is done and what date
                  separate(Level0_profiles,c("MULakeNumber","year","month","day","csv"),remove=FALSE)%>% #use separate function to pull out each component of the 
                  mutate(profile_date=ymd(paste(year,month,day,sep="-")),
                         maxDepth_m=NA,
                         latitude=NA,
                         longitude=NA,
                         altitude_m=NA,
                         barometerAirHandheld_mbars=NA,
                         doConcentration_mgpL_bottom=NA,
                         doConcentration_mgpL_bottom5mean=NA,
                         nrow_original_Level0=NA, #number of rows in the read in file
                         nrow_readProfile=NA, #number of rows in the read profile
                         nrow_Level1=NA, #Number of Level1 profile 
                         Level1FileName=paste0(MULakeNumber,"_",year,"_",month,"_",day,"_Level1.csv") #new file name which is old file name with _Level1 ammended
                        )%>%
                      arrange(Level0_profiles)

#Modify the data frame to go from long to wide####  
#add columns with NAs
#Convert DO to DO sat (add function to function script)
#Sensor QAQC for just temp and DO
#Store details for each profile

#Initialize storage location
List_historical<-list()

#Debug fileIndex<-3
for(fileIndex in 1:nrow(Level0_files_log)){
  singleProfile<-historicalData%>%filter(fileName==Level0_files_log$Level0_profiles[fileIndex])%>% #pull out the first profile with all values
                  dplyr::select(-any_of(c("waterBody","beginDateTime","parameterTypeID","projectID")))
  DO_df<-singleProfile%>%filter(parameterType=="DO")%>%rename(doConcentration_mgpL=parameterValue)%>%dplyr::select(-parameterType) ##Identify just the DO values
  temp_df<-singleProfile%>%filter(parameterType=="TEMP")%>%rename(temp_degC=parameterValue)%>%dplyr::select(-parameterType) ##Identify just the temperature values
  #Merge them back together with all from both df
  reformatted_DF<-full_join(temp_df,DO_df,by=c("prof_Depth","MULakeNumber","dateTime","year","month","day","fileName"))%>%
                  rename(depth_m=prof_Depth)%>% #rename depth
                  mutate(verticalPosition_m=depth_m, #add vertical position
                          doSaturation_percent=(doConcentration_mgpL*100)/DOsat_function(temp_degC))%>%  #Add in DO saturation from calculation, only calculates where there is a DO concentration and temperature
                  mutate(chlorophyll_RFU=NA, #Add a bunch of missing columns as NA
                         orp_mV=NA,
                         waterPressure_barA=NA, 
                         salinity_psu=NA,
                         specificConductivity_uSpcm=NA,
                         phycocyaninBGA_RFU=NA,
                         tds_mgpL=NA,
                         turbidity_FNU=NA,
                         latitude=NA,
                         longitude=NA,
                         altitude_m=NA,
                         barometerAirHandheld_mbars=NA)
                
 
  #Store the number of rows in the log####
  Level0_files_log$nrow_original_Level0[fileIndex]<-nrow(reformatted_DF)
  
  #Basic checks for error codes and makes any NA data above or below the sensor bounds as NA####
  #only need to check the few that exist for the historical data
  qaqcProfile<-reformatted_DF%>%mutate(
    depth_m=qaqc_bounds(depth_m,sensorLimits),
    doSaturation_percent=qaqc_bounds(doSaturation_percent,sensorLimits),
    doConcentration_mgpL=qaqc_bounds(doConcentration_mgpL,sensorLimits),
    temp_degC=qaqc_bounds(temp_degC,sensorLimits),
    verticalPosition_m=qaqc_bounds(verticalPosition_m,sensorLimits),
    )
    
  
  #Store any details in the log####
  Level0_files_log$maxDepth_m[fileIndex]<-max(qaqcProfile$verticalPosition_m,na.rm=TRUE)
  Level0_files_log$nrow_Level1[fileIndex]<-nrow(qaqcProfile)    #Store number of rows in qaqcProfile    

  #Set the log as done####
  Level0_files_log$Level0to1_done[fileIndex]<-"Yes" #This profile has been exported
  Level0_files_log$Level0to1_done_date[fileIndex]<-as_date(today()) #Set the date run as today
  
  #Store them each as an entry in a list of tibbles
  List_historical[[fileIndex]]<-qaqcProfile
  
  }

#Bind all the items in the list together and select appropriate columns
qaqc_historical<-do.call(bind_rows, List_historical)

#Write out the dataset to the level1 folder
write_csv(qaqc_historical,file=paste0("01_Level1_Data/",year,"_Level1_Data/HistoricalProfileData_Level1.csv"))


#Export the log####
write_csv(Level0_files_log,file=paste0("06_Outputs/",year,"_QAQC_log.csv"))

