##Level3: Summarizes individual profiles Missouri Reservoirs####
##Created 02Aug2023 by David Richardson (hereafter DCR)
##Reads in level 2 data from GIT, exports as a single file with each profile summarized into a row####

#Level2 to 3 outline:####
#Read in all the level 2 files for a year
#Calculate the following as derived data:
  # depth chlorophyll max, 
  # depth BG max, 
  # ratio of BG:chl maxes, 
  # oxycline, 
  # stratification metrics, 
  # depth of anoxia/hypoxia, 
  # site mean lat/long, 
  # max depth from profile, 
  # surface DO, (first reading or first 0.5m)
  # surface temperature, (first reading or first 0.5m)
  # surface pH (first reading or first 0.5m)

#Libraries
if (!require(tidyverse)) {install.packages("tidyverse")}

#Load packages
library(tidyverse)

#Run functions script to upload all the user defined functions####
source("05_Scripts/00_MULimnology_reservoirProfileQAQC_Functions.R")

#Read in level 2 files from a particular year####
#Set years here, update each year here####
yearIndex<-2017
  #possible years: c("Historical","2017","2018","2019","2020","2021","2022")

#*Set the directory path here####
dirPath<-paste0("02_Level2_Data/")


#Stack all the profiles together####
profiles2<-read_csv(file=paste0(dirPath,"/",yearIndex,"_Level2.csv"), col_types = cols()) #last argument suppresses the message on input about column types, helpful for mass upload

#Stack all the logs together####
logs2<-read_csv(file=paste0("06_Outputs/",yearIndex,"_QAQC_log.csv"), col_types = cols())

#Check for unqiue numbers in both the profiles and logs####
#The profiles should be more accurate because the logs include profiles that have been removed####
#This no longer works since we are doing it one at a time. Recreate old code - loop through, store in a list and then do.call to merge together to get all site names####
  #sort(unique(profiles2$MULakeNumber))
  #sort(unique(logs2$MULakeNumber))
#Write out that list of MULake numbers####
  #write_csv(tibble(MULakeNumber=sort(unique(profiles2$MULakeNumber))),file=paste0("06_Outputs/ListOfMULakeNumbers_through2022.csv"))

#Summary calculations - summarize by MULakeNumber and date, depth_m is profile depth, temp_degC is the temperature
  #Temperature/Stratification metrics metrics####
  #Oxygen metrics
  #
profiles2%>%
  group_by(MULakeNumber,date)%>%
  summarize(
    minDepth_m=min(depth_m,na.rm=TRUE), #smallest depth
    maxDepth_m=max(depth_m,na.rm=TRUE), #deepest depth
    numberOfMeasurements_temperature=sum(!is.na(temp_degC)), #number of temperature measurements
    thermoclineDepth_m_thresh0.3=thermocline.Depth(depth.array=depth_m,temp.array=temp_degC,thresh = 0.3), #thermocline depth at 0.3 density threshold
    top_metalimnion_m=meta.depths(wtr=temp_degC,depths=depth_m)[1], #Top of metalimnion using rLakeAnalyzer
    bottom_metalimnion_m=meta.depths(wtr=temp_degC,depths=depth_m)[2], #Bottom of metalimnion using rLakeAnalyzer
    epilimnion_temp_degC=mean(temp_degC[depth_m<=top_metalimnion_m],na.rm=TRUE), #average temperature above metalimnion top
    hypolimnion_temp_degC=mean(temp_degC[depth_m>=bottom_metalimnion_m],na.rm=TRUE), #average temperature below the metalimnion bottom
    above_thermocline_temp_degC=mean(temp_degC[depth_m<=thermoclineDepth_m_thresh0.3],na.rm=TRUE), #average temperature above or equal to the thermocline
    below_thermocline_temp_C=mean(temp_degC[depth_m>thermoclineDepth_m_thresh0.3],na.rm=TRUE), #average temperature below the thermocline
    delta_hypo_epi_waterDensity_kgperm3=water.density(hypolimnion_temp_degC)-water.density(epilimnion_temp_degC), #density difference between hypolimnion water and epilimnion water, bigger difference is stronger stratification
    buoyancyfrequency_1_s2=max(buoyancy.freq(wtr=temp_degC,depths=depth_m)), #generate the maximum buoyancy frequency using rLakeAnalyzer buoyancy frequency vector
    numberOfMeasurements_do=sum(!is.na(doConcentration_mgpL)), #number of depth measurements
    minDO_mgpL=minDO(doConcentration_mgpL), #lowest DO concentration
    maxDO_mgpL=maxDO(doConcentration_mgpL), #highest DO concentration
    minDO_percent=minDO(doSaturation_percent), #lowest DO percentage
    maxDO_percent=maxDO(doSaturation_percent), #highest DO percentage
    DO_mgpL_profileMean=mean(doConcentration_mgpL,na.rm=TRUE), #overall average DO mgpL
    DO_percent_profileMean=mean(doSaturation_percent,na.rm=TRUE), #overall average DO percentage
    epilimnion_DO_mgpL=mean(doConcentration_mgpL[depth_m<=top_metalimnion_m],na.rm=TRUE), #average temperature above metalimnion top
    hypolimnion_DO_mgpL=mean(doConcentration_mgpL[depth_m>=bottom_metalimnion_m],na.rm=TRUE), #average temperature below the metalimnion bottom
    epilimnion_DO_percent=mean(doSaturation_percent[depth_m<=top_metalimnion_m],na.rm=TRUE), #average temperature above metalimnion top
    hypolimnion_DO_percent=mean(doSaturation_percent[depth_m>=bottom_metalimnion_m],na.rm=TRUE), #average temperature below the metalimnion bottom
    depthMaxDOpercentage_m=depthDOmax(depth_vector=depth_m,do_vector=doSaturation_percent), #calculates the depth of the maximum DO
    Oxycline_m=thermocline.Depth.max(depth.array=depth_m,temp.array=doConcentration_mgpL), #Find where the fastest rate of DO change is
    Hypoxycline_m=Oxycline_threshold(depth.array=depth_m,DO.array=doConcentration_mgpL,threshold=2), #find the first depth where DO is less than or equal to the threshold of 2, NA means no DO values (all NAs) OR no values below threshold
    Anoxycline_m=Oxycline_threshold(depth.array=depth_m,DO.array=doConcentration_mgpL,threshold=1) #find the first depth where DO is less than or equal to the threshold of 2, NA means no DO values (all NAs) OR no values below threshold
    ) #end of summarize
