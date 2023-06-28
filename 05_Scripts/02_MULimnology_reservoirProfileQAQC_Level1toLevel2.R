##Level1: DataInput, QA/QC and merge Missouri Reservoirs####
##Created 26Jun2023 by David Richardson (hereafter DCR)
##Reads in level 1 data from GIT formats the files, and does a basic QA/QC, exports each year as 1 file####

#Level1 to 2 outline:####
#  Check for high salinity to use for density conversion
#Convert temperature to density
#Check for non-monotonically decreasing densities
#Check each column for large jumps from depth to depth
#Vary +/- 0.1x to 0.5x to 1x
#Check for valid sensor values that are extreme biologically (<4deg C or all pH 4.0 or 7.0)
#Create flag that looks at that column or profile (make a graph)
#??Comparisons between sensor and lab chlorophyll RFU (rarely agree) 
#Row bind all profiles together
#Reorder the columns to make more sense (general variables on the left, change vertical position to depth, get rid of vertical position, temp, DO, DO%, then others in ABC order)

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
year<-2022

#*Set the directory path here####
dirPath<-paste0("01_Level1_Data/",year,"_Level1_Data")

#Read in the log####
#Creates slots for the level1 to 2 done and date
Level1_files_log<-read_csv(paste0("06_Outputs/",year,"_QAQC_log.csv"))%>%
                mutate(Level1to2_done="No",Level1to2_done_date=as_date(NA),
                       flag_monotonicDensity=NA, #Gives a column to flag all the non-monotonic profiles 
                       flag_decreasingTemp=NA, #Gives a column to flag all the non-decreasing temperatures
                       flag_lowTemps=NA, #flags the number of temperatures below 5 C
                       flag_lowpH=NA, #flags the number of pHs below 5
                       flag_jumps_chlorophyll_RFU=NA,
                       flag_jumps_depth_m=NA,
                       flag_jumps_doSaturation_percent=NA,
                       flag_jumps_doConcentration_mgpL=NA,
                       flag_jumps_orp_mV=NA,
                       flag_jumps_waterPressure_barA=NA,
                       flag_jumps_salinity_psu=NA,
                       flag_jumps_specificConductivity_uSpcm=NA,
                       flag_jumps_phycocyaninBGA_RFU=NA,
                       flag_jumps_tds_mgpL=NA,
                       flag_jumps_turbidity_FNU=NA,
                       flag_jumps_pH=NA,
                       flag_jumps_temp_degC=NA,
                       flag_jumps_verticalPosition_m=NA,
                       flag_jumps_latitude=NA,
                       flag_jumps_longitude=NA,
                       flag_jumps_altitude_m=NA,
                       flag_jumps_barometerAirHandheld_mbars=NA
                       ) 

#Identify all the individual .csv files####
Level1_files<-list.files(dirPath,pattern = "*.csv")

#Set thresholds for water density differences and temperature differences to set flags
waterDensityDifference_threshold<- -0.02
temperatureDifference_threshold<-0.1

#Initialize storage location####
List_qaqc1<-list()

#Loop through each file####
#Debug fileIndex<-145
#Debug: fileIndex 
#       Level1_files_log$Level0_profiles[fileIndex]
for(fileIndex in 1:length(Level1_files)){
  
  #read in file####
  readProfile1<-read_csv(file=paste0(dirPath,"/",Level1_files_log$Level1FileName[fileIndex]), col_types = cols()) #last argument suppresses the message on input about column types, helpful for mass upload
  #Calculate a water density column
  qaqcProfile1<-readProfile1%>%mutate(waterDensity_kgpm3=WaterDensity_function_vectorize(temp_degC))%>%
                mutate(waterDensityDiff_kgpm3=c(0,diff(waterDensity_kgpm3)),
                       tempDiff_degC=c(0,diff(temp_degC)))
  
  #Spits out the number of non-monotically increasing densities or non-decreasing temperatures####
  #can change the numbers here to account for some wiggle room
  Level1_files_log$flag_monotonicDensity[fileIndex]<-sum(qaqcProfile1$waterDensityDiff_kgpm3<waterDensityDifference_threshold)
  Level1_files_log$flag_decreasingTemp[fileIndex]<-sum(qaqcProfile1$tempDiff_degC>temperatureDifference_threshold)
  
  #Flag abnormal numbers####
  #how many values are below 5C
  Level1_files_log$flag_lowTemps[fileIndex]<-sum(qaqcProfile1$temp_degC<5)
  Level1_files_log$flag_lowpH[fileIndex]<-sum(qaqcProfile1$pH<5)
  
  #establish the scalar value for jumps up or down
  jump<-3
  #Check all columns for large jumps (100% up or 50% down)####
  Level1_files_log$flag_jumps_chlorophyll_RFU[fileIndex]<-sum(flag_jumps(qaqcProfile1$chlorophyll_RFU,scalar=jump))
  Level1_files_log$flag_jumps_depth_m[fileIndex]<-sum(flag_jumps(qaqcProfile1$depth_m,scalar=jump))
  Level1_files_log$flag_jumps_doSaturation_percent[fileIndex]<-sum(flag_jumps(qaqcProfile1$doSaturation_percent,scalar=jump))
  Level1_files_log$flag_jumps_doConcentration_mgpL[fileIndex]<-sum(flag_jumps(qaqcProfile1$doConcentration_mgpL,scalar=jump))
  Level1_files_log$flag_jumps_orp_mV[fileIndex]<-sum(flag_jumps(qaqcProfile1$orp_mV,scalar=jump))
  Level1_files_log$flag_jumps_waterPressure_barA[fileIndex]<-sum(flag_jumps(qaqcProfile1$waterPressure_barA,scalar=jump))
  Level1_files_log$flag_jumps_salinity_psu[fileIndex]<-sum(flag_jumps(qaqcProfile1$salinity_psu,scalar=jump))
  Level1_files_log$flag_jumps_specificConductivity_uSpcm[fileIndex]<-sum(flag_jumps(qaqcProfile1$specificConductivity_uSpcm,scalar=jump))
  Level1_files_log$flag_jumps_phycocyaninBGA_RFU[fileIndex]<-sum(flag_jumps(qaqcProfile1$phycocyaninBGA_RFU,scalar=jump))
  Level1_files_log$flag_jumps_tds_mgpL[fileIndex]<-sum(flag_jumps(qaqcProfile1$tds_mgpL,scalar=jump))
  Level1_files_log$flag_jumps_turbidity_FNU[fileIndex]<-sum(flag_jumps(qaqcProfile1$turbidity_FNU,scalar=jump))
  Level1_files_log$flag_jumps_pH[fileIndex]<-sum(flag_jumps(qaqcProfile1$pH,scalar=jump))
  Level1_files_log$flag_jumps_temp_degC[fileIndex]<-sum(flag_jumps(qaqcProfile1$temp_degC,scalar=jump))
  Level1_files_log$flag_jumps_verticalPosition_m[fileIndex]<-sum(flag_jumps(qaqcProfile1$verticalPosition_m,scalar=jump))
  Level1_files_log$flag_jumps_latitude[fileIndex]<-sum(flag_jumps(qaqcProfile1$latitude,scalar=jump))
  Level1_files_log$flag_jumps_longitude[fileIndex]<-sum(flag_jumps(qaqcProfile1$longitude,scalar=jump))
  Level1_files_log$flag_jumps_altitude_m[fileIndex]<-sum(flag_jumps(qaqcProfile1$altitude_m,scalar=jump))
  Level1_files_log$flag_jumps_barometerAirHandheld_mbars[fileIndex]<-sum(flag_jumps(qaqcProfile1$barometerAirHandheld_mbars,scalar=jump))
  
  #Store them each as an entry in a list of tibbles
  List_qaqc1[[fileIndex]]<-qaqcProfile1

} #end of for loop

#STOPPED HERE: SEARCH THROUGH THE LIST FOR ANY AND DO SOME DIAGNOSTIC FIGURES/QAQC profiles####
#THEN MERGE THEM TOGETHER AND OUTPUT####
#OUTPUT LOG1 too####

Level1_files_log%>%filter(flag_lowpH>20)

#Some plots to look at the data if it is flagged####
ggplot(data=qaqcProfile1,aes(x=waterDensity_kgpm3,y=verticalPosition_m,color=as.factor(waterDensityDiff_kgpm3<waterDensityDifference_threshold)))+geom_point()+scale_y_reverse()+labs(col="flag")

ggplot(data=qaqcProfile1,aes(x=dateTime,y=verticalPosition_m))+geom_point()
ggplot(data=qaqcProfile1,aes(x=turbidity_FNU,y=verticalPosition_m))+geom_point()+scale_y_reverse()
ggplot(data=qaqcProfile1,aes(x=longitude,y=verticalPosition_m))+geom_point()+scale_y_reverse()
ggplot(data=qaqcProfile1,aes(x=phycocyaninBGA_RFU,y=verticalPosition_m))+geom_point()+scale_y_reverse()
ggplot(data=qaqcProfile1,aes(x=pH,y=verticalPosition_m))+geom_point()+scale_y_reverse()






















#Export the log####
write_csv(Level1_files_log,file=paste0("06_Outputs/",year,"_QAQC_log.csv"))
