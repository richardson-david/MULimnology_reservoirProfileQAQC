##Level3: Summarizes individual profiles Missouri Reservoirs####
##Created 02Aug2023 by David Richardson (hereafter DCR)
##Reads in level 2 data from GIT, exports as a single file with each profile summarized into a row####

#Level2 to 3 outline:####
#Read in all the level 2 files
#Stack them all together
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

#Read in all level 2 files####
#Set years here, update each year here####
years<-c("Historical","2017","2018","2019","2020","2021","2022")

#*Set the directory path here####
dirPath<-paste0("02_Level2_Data/")

#Initialize storage locations for the logs and the data####
List_qaqc2<-list()
List_logs2<-list()

#Read in each of the logs and each of the level 2 data####
#debug yearIndex<-2
for(yearIndex in 1:length(years)){
  
  #read in Level 2 profiles####
  List_qaqc2[[yearIndex]]<-read_csv(file=paste0(dirPath,"/",years[yearIndex],"_Level2.csv"), col_types = cols()) #last argument suppresses the message on input about column types, helpful for mass upload
  
  #Read in Level 2 logs####
  List_logs2[[yearIndex]]<-read_csv(file=paste0("06_Outputs/",years[yearIndex],"_QAQC_log.csv"), col_types = cols())
  
  }

#Stack all the profiles together####
profiles2<-do.call(bind_rows, List_qaqc2)

#Stack all the logs together####
logs2<-do.call(bind_rows, List_logs2)

#Check for unqiue numbers in both the profiles and logs####
#The profiles should be more accurate because the logs include profiles that have been removed####
sort(unique(profiles2$MULakeNumber))
sort(unique(logs2$MULakeNumber))
#Write out that list of MULake numbers####
write_csv(tibble(MULakeNumber=sort(unique(profiles2$MULakeNumber))),file=paste0("06_Outputs/ListOfMULakeNumbers_through2022.csv"))

#Summary calculations - summarize by MULakeNumber and date, depth_m is profile depth, temp_degC is the temperature
  #Temperature/Stratification metrics metrics####
      # -Thermocline depth (metaTop in rLakeAnalyzer)
      # -Top of the hypolimnion (metaBottom in rLakeAnalyzer)
      # -Depth weight temperature - whole reservoir
      # -Epilimnion temp (meters 1-3, above metaTop)
      # -Hypolimnion temp (bottom 3 meters or below metaBottom)
      # -Stratification strength (buoyancy frequency, density differences from top to bottom or epi to hypo, density gradients)
profiles2%>%
  group_by(MULakeNumber,dateTime)%>%
  summarize(
    minDepth_m=min(depth_m,na.rm=TRUE), #smallest depth
    maxDepth_m=max(depth_m,na.rm=TRUE), #deepest depth
    numberOfMeasurements_temperature=sum(!is.na(temp_degC)), #number of depth measurements
    thermoclineDepth_m_thresh0.3=thermocline.Depth(depth.array=depth_m,temp.array=temp_degC,thresh = 0.3), #thermocline depth at 0.3 density threshold
    top_metalimnion_m=meta.depths(wtr=temp_degC,depths=depth_m)[1], #Top of metalimnion using rLakeAnalyzer
    bottom_metalimnion_m=meta.depths(wtr=temp_degC,depths=depth_m)[2], #Bottom of metalimnion using rLakeAnalyzer
    epilimnion_temp_C=mean(parameterValue[prof_Depth<=top_metalimnion_m],na.rm=TRUE), #average temperature above metalimnion top
    hypolimnion_temp_C=mean(parameterValue[prof_Depth>=bottom_metalimnion_m],na.rm=TRUE), #average temperature below the metalimnion bottom
    above_thermocline_temp_C=mean(parameterValue[prof_Depth<=thermoclineDepth_m_thresh0.3],na.rm=TRUE), #average temperature above or equal to the thermocline
    below_thermocline_temp_C=mean(parameterValue[prof_Depth>thermoclineDepth_m_thresh0.3],na.rm=TRUE), #average temperature below the thermocline
    delta_hypo_epi_waterDensity_kgperm3=water.density(hypolimnion_temp_C)-water.density(epilimnion_temp_C), #density delta (hypo-epi)
    buoyancyfrequency_1_s2=max(buoyancy.freq(wtr=parameterValue,depths=prof_Depth)) #generate the maximum buoyancy frequency using rLakeAnalyzer buoyancy frequency vector
  )