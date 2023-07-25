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
                mutate(nrow_Level2=NA,
                       Level2_maxDepth_m=NA,
                       Level1to2_done="No",Level1to2_done_date=as_date(NA),
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
                       flag_jumps_barometerAirHandheld_mbars=NA,
                       
                       Level1to2_some_depths_removed=NA, #indicates that some points were removed during the level 1 to 2 process
                       Level1to2_profileRemoved=NA #yes/no indicating the whole profile was removed.
                       ) 

#Identify all the individual .csv files####
Level1_files<-list.files(dirPath,pattern = "*.csv")

#Set thresholds for water density differences and temperature differences to set flags
waterDensityDifference_threshold<- -0.02
temperatureDifference_threshold<-0.1
lowTemperature_threshold<-4.2
lowpH_threshold<-5
#establish the scalar value for jumps up or down
jump<-2.5
#Establish the bottom number of rows to examine for big jumps in chl, bga, or turbidity
#nrow_bottom<-10 #used in old method
nrow_bottom_prop<-0.20 #checks the bottom 20% for jumps
nrow_top<-3
#Establish the minimum number of rows to move forward
nrow_min<-5

#Initialize storage location####
List_qaqc1<-list()

#Loop through each file####
#Debug fileIndex<-45
#Debug: fileIndex 
#       Level1_files_log$Level0_profiles[fileIndex]
for(fileIndex in 1:length(Level1_files)){
  
  #read in file####
  readProfile1<-read_csv(file=paste0(dirPath,"/",Level1_files_log$Level1FileName[fileIndex]), col_types = cols()) #last argument suppresses the message on input about column types, helpful for mass upload
  #Calculate a water density column
  qaqcProfile1<-readProfile1%>%arrange(verticalPosition_m)%>% #arrange by vertical position
                mutate(waterDensity_kgpm3=WaterDensity_function_vectorize(temp_degC))%>%
                mutate(waterDensityDiff_kgpm3=c(0,diff(waterDensity_kgpm3)),
                       tempDiff_degC=c(0,diff(temp_degC)))%>%
                mutate(MULakeNumber=as.character(MULakeNumber)) #make MULakeNumber a character since there are differing versions
  
  #Spits out the number of non-monotically increasing densities or non-decreasing temperatures####
  #can change the numbers here to account for some wiggle room
  Level1_files_log$flag_monotonicDensity[fileIndex]<-sum(qaqcProfile1$waterDensityDiff_kgpm3<waterDensityDifference_threshold)
  Level1_files_log$flag_decreasingTemp[fileIndex]<-sum(qaqcProfile1$tempDiff_degC>temperatureDifference_threshold)
  
  #Flag abnormal numbers####
  #how many values are below 5C
  Level1_files_log$flag_lowTemps[fileIndex]<-sum(qaqcProfile1$temp_degC<lowTemperature_threshold,na.rm=TRUE)
  Level1_files_log$flag_lowpH[fileIndex]<-sum(qaqcProfile1$pH<lowpH_threshold,na.rm=TRUE)
  
  #If any of the pH's are below 5, remove the whole column
  if(Level1_files_log$flag_lowpH[fileIndex]>0){qaqcProfile1$pH<-NA
                                              Level1_files_log$Level1to2_some_depths_removed[fileIndex]<-"yes"} #indicates some numbers were removed
  

  #Check all columns for large jumps ####
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
  
  #Find the locations of each of the turbidity, chl, and bga using flag_jumps function####
  #If any of those are in the bottom 10 measurements, remove those rows
  #Finds rows with jumps across the three variables
  rows_with_jumps<-unique(c(which(flag_jumps(qaqcProfile1$turbidity_FNU,scalar=jump)==TRUE),
  which(flag_jumps(qaqcProfile1$phycocyaninBGA_RFU,scalar=jump)==TRUE),
  which(flag_jumps(qaqcProfile1$chlorophyll_RFU,scalar=jump)==TRUE)))
  
  #Alternate. check the last 10% of rows and if there are any matching, find the lowest one, delete from there to the end####
  #*find all the jumps in the bottom 10% of the profile####
  rows_with_jumps_bottom<-rows_with_jumps[rows_with_jumps>=round(Level1_files_log$nrow_Level1[fileIndex]*(1-nrow_bottom_prop),0)]
  
  #*check if there are any rows to delete, then 
  if(!identical(rows_with_jumps_bottom, integer(0))){ #If there are no rows to delete, then this will return false because the vector will be integer(0) OR if the rows are all above the bottom
    qaqcProfile1<-qaqcProfile1%>%
      slice(-c(min(rows_with_jumps_bottom):Level1_files_log$nrow_Level1[fileIndex])) #Remove from the lowest point all the way to the bottom
    Level1_files_log$Level1to2_some_depths_removed[fileIndex]<-"yes" #indicate that some rows were removed
  }
  
  #OLD METHOD THAT RELIES ON nrow_bottom
  #chck if there is integer(0) then do nothing
  #sort the vector biggest to smallest
  #check the differences
  #Stop when difference is !=1
  #
  # 
  # #Check if there are rows to delete, if so, delete those rows####
  # if(!identical(rows_with_jumps, integer(0))&!identical(rows_with_jumps[rows_with_jumps>=Level1_files_log$nrow_Level1[fileIndex]-nrow_bottom],integer(0))){ #If there are no rows to delete, then this will return false because the vector will be integer(0) OR if the rows are all above the bottom
  #   qaqcProfile1<-qaqcProfile1%>%
  #     slice(-c(maxN(rows_with_jumps):Level1_files_log$nrow_Level1[fileIndex])) #Remove from the lowest point all the way to the bottom
  #   Level1_files_log$Level1to2_some_depths_removed[fileIndex]<-"yes" #indicate that some rows were removed
  # }
  
  #Check if there are rows to delete at the top, if so, delete those rows####
  if(!identical(rows_with_jumps, integer(0))&!identical(rows_with_jumps[rows_with_jumps<nrow_top],integer(0))){ #If there are no rows to delete, then this will return false because the vector will be integer(0) OR if the rows are all above the bottom
    qaqcProfile1<-qaqcProfile1%>%
      slice(-c(1:(min(rows_with_jumps)-1))) #Remove from the lowest point all the way to the top
    Level1_files_log$Level1to2_some_depths_removed[fileIndex]<-"yes" #indicate that some rows were removed
  }
    

  #Store them each as an entry in a list of tibbles####
    #*check if there are less than nrow_min rows in the profile####
    #*If there are then remov that profile
  if(Level1_files_log$nrow_Level1[fileIndex]<nrow_min){
    List_qaqc1[fileIndex]<-list(NULL) #make sure this profile is null
    Level1_files_log$Level1to2_profileRemoved[fileIndex]<-"yes" #indicate the profile has been removed
  }else if(Level1_files_log$flag_lowTemps[fileIndex]>0&as.numeric(Level1_files_log$month[fileIndex])%in%c(5:10)){
    List_qaqc1[fileIndex]<-list(NULL) #make sure this profile is null
    Level1_files_log$Level1to2_profileRemoved[fileIndex]<-"yes" #indicate the profile has been removed
    
  }else{
    List_qaqc1[[fileIndex]]<-qaqcProfile1 
    Level1_files_log$Level2_maxDepth_m[fileIndex]<-max(qaqcProfile1$verticalPosition_m,na.rm=TRUE)
    Level1_files_log$nrow_Level2[fileIndex]<-nrow(qaqcProfile1)
    
  }
  

  
  #Set the log as done####
  Level1_files_log$Level1to2_done[fileIndex]<-"Yes" #This profile has been exported
  Level1_files_log$Level1to2_done_date[fileIndex]<-as_date(today()) #Set the date run as today
  
} #end of for loop




#visually assess any flagged profiles or values####
  #*export to a single file per year####
  pdf(paste0("06_Outputs/Level1to2_QAQC_plots_",year,".pdf"), onefile = TRUE)
  #*loop through the different profiles in the log file####
  #*debug: fileIndex2<-7
  for(fileIndex2 in 1:nrow(Level1_files_log)){
  temp_df<-List_qaqc1[[fileIndex2]]  
  if(is.null(temp_df)){
    #do nothing
  }else{
  
  #**Print out this one for sure####
  print(ggplot(data=temp_df,aes(x=dateTime,y=verticalPosition_m))+geom_point()+ggtitle(paste0(Level1_files_log$Level1FileName[fileIndex2]," ","****Depth vs. time")))
  #**Non-monotonic density
  if(Level1_files_log[fileIndex2,"flag_monotonicDensity"]>0){
      print(ggplot(data=temp_df,aes(x=waterDensity_kgpm3,y=verticalPosition_m,color=as.factor(waterDensityDiff_kgpm3<waterDensityDifference_threshold)))+geom_point()+scale_y_reverse()+labs(col="flag")+ggtitle(paste0(Level1_files_log$Level1FileName[fileIndex2]," ","****Non-monotonic densities")))
      }
  #**Non-monotonic density####
  if(Level1_files_log[fileIndex2,"flag_decreasingTemp"]>0){
    print(ggplot(data=temp_df,aes(x=temp_degC,y=verticalPosition_m,color=as.factor(tempDiff_degC<temperatureDifference_threshold)))+geom_point()+scale_y_reverse()+labs(col="flag")+ggtitle(paste0(Level1_files_log$Level1FileName[fileIndex2]," ","****Non-decreasing temps")))
  }
  
  #**Low temperatures####
  if(Level1_files_log[fileIndex2,"flag_lowTemps"]>0){
    print(ggplot(data=temp_df,aes(x=temp_degC,y=verticalPosition_m,color=as.factor(temp_degC<lowTemperature_threshold)))+geom_point()+scale_y_reverse()+labs(col="flag")+ggtitle(paste0(Level1_files_log$Level1FileName[fileIndex2]," ","****Too low temps")))
  }
  
  #**Low pH####
  #*This also checks to see if all the pH's are 0
  if(Level1_files_log[fileIndex2,"flag_lowpH"]>0&sum(!is.na(temp_df$pH))>0){
    print(ggplot(data=temp_df,aes(x=pH,y=verticalPosition_m,color=as.factor(pH<lowpH_threshold)))+geom_point()+scale_y_reverse()+labs(col="flag")+ggtitle(paste0(Level1_files_log$Level1FileName[fileIndex2]," ","****Too low pH")))
  }
  
  #**Turbidity jumps####
  if(Level1_files_log[fileIndex2,"flag_jumps_turbidity_FNU"]>0){
    print(ggplot(data=temp_df,aes(x=turbidity_FNU,y=verticalPosition_m))+geom_point(color="brown")+scale_y_reverse()+labs(col="flag")+ggtitle(paste0(Level1_files_log$Level1FileName[fileIndex2]," ","****Turbidity jumps")))
  }
  
  #**chl a jumps####
  if(Level1_files_log[fileIndex2,"flag_jumps_chlorophyll_RFU"]>0){
    print(ggplot(data=temp_df,aes(x=chlorophyll_RFU,y=verticalPosition_m))+geom_point(color="greenyellow")+scale_y_reverse()+labs(col="flag")+ggtitle(paste0(Level1_files_log$Level1FileName[fileIndex2]," ","****chl jumps")))
  }
  
  #**bga jumps####
  if(Level1_files_log[fileIndex2,"flag_jumps_phycocyaninBGA_RFU"]>0){
    print(ggplot(data=temp_df,aes(x=phycocyaninBGA_RFU,y=verticalPosition_m))+geom_point(color="cyan4")+scale_y_reverse()+labs(col="flag")+ggtitle(paste0(Level1_files_log$Level1FileName[fileIndex2]," ","****bga jumps")))
  }
  
    } #end of check for null
  } #end of for loop
  
  dev.off()
  
  #Look for any row with any of the flag columns greater than 5####
  #check_df<-Level1_files_log%>%filter(if_any(flag_lowTemps:flag_jumps_barometerAirHandheld_mbars,~.>5))%>%print(n=Inf)
  
  #Print out how many profiles were modified or removed####
  Level1_files_log%>%filter(Level1to2_some_depths_removed=="yes")%>%nrow()
  Level1_files_log%>%filter(Level1to2_profileRemoved=="yes")%>%nrow()
  
  ##########################################################################
  #####HERE IS WHRE YOU WOULD PUT CODE TO MODIFY ANY INDIVIDUAL PROFILES####
  ##########################################################################
  
#Bind all the rows together####
  qaqc2<-do.call(bind_rows, List_qaqc1)%>%
          dplyr::select(-depth_m)%>% #drop depth
          rename(depth_m=verticalPosition_m)%>%
          dplyr::select(MULakeNumber,date,dateTime,depth_m,temp_degC,doConcentration_mgpL,doSaturation_percent,chlorophyll_RFU,phycocyaninBGA_RFU,turbidity_FNU,salinity_psu,specificConductivity_uSpcm,tds_mgpL,orp_mV,pH,latitude,longitude,altitude_m,barometerAirHandheld_mbars)
  
#Export the level2 file####
  write_csv(qaqc2,file=paste0("02_Level2_Data/",year,"_Level2.csv"))
#Export the log####
write_csv(Level1_files_log,file=paste0("06_Outputs/",year,"_QAQC_log.csv"))
