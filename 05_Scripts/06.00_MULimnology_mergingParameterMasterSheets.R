##Master Parameter Sheets: Merging in lab water quality data together for Missouri Reservoirs####
##Created 05Feb2024 by David Richardson (hereafter DCR)
##Reads in each year master parameter sheets from GIT and merges together####

#Libraries
if (!require(tidyverse)) {install.packages("tidyverse")}

#Load packages
library(tidyverse)
library(readxl) #read in excel files, need to load readxl explicity because it is not a core tidyverse package

#Set working directory
year<-2023
dir<-paste0("08_ParameterMasterSheets/",year,"_ParameterMasterSheets")

#Read in the functions####
source("05_Scripts/00_MULimnology_reservoirProfileQAQC_Functions.R")


#Read in master parameter sheets####
###############################
#*Read in secchi sheet####
#Gives secchi and date Time#
#**Read in Secchi from sheet 1####
secchiMaster<-read_excel(paste0(dir,"/2023 SLAP Secchi depth masterfile 01-11-2024.xlsx"),sheet="Sheet1")%>%
  rename(LakeName=`Lake name`, #rename some columns
         date=Date,
         time=Time,
         secchiDepth_m=`Depth (m)`)%>%
  mutate(date=ymd(date), #extract the date
         hour=hour(time), #extract the hour
         minute=minute(time), #extract the minute
         second=second(time), #extract the second
         time=paste(hour,minute,second,sep=":"), #put back together time without the artificial date
         dateTime=ymd(date)+hms(time))%>% #put a dateTime together
  dplyr::select(-hour,-minute,-second) #get rid of the extraneous hour, minute, second

#Secchi: create a subdata frame with the long format that will be merged####   
secchi_merge<-secchiMaster%>%
  mutate(
    MULakeNumber=as.character(MULakeNumber),
    Date=date,
    beginDepth=0,
    endDepth_char="ACTUAL",
    endDepth_m="0",
    parameterType="SECCHI",
    unit="m",
    parameterValue=secchiDepth_m,
    parameterValueQCCodeID=NA
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)


#Create a secchi dateTime to match with the MULakeNumber and Date columns####
dateTime<-secchiMaster%>%dplyr::select(MULakeNumber,date,dateTime)%>%
          rename(Date=date)%>%
          mutate(MULakeNumber=as.character(MULakeNumber))%>%
          distinct(.) #keep only the unique values for merging

#Create a latlon dataframe for all the possible Field duplicate files
dateTime_FD<-dateTime%>%
  mutate(MULakeNumber=paste0(MULakeNumber," FD"))          


#Read in ecoregion sheets####
###############################
#*Read in Sheet1 sheet####
#Gives file name#
ecoRegionMaster<-read_excel(paste0(dir,"/SLAP 2023 Ecoregion Table.xlsx"),sheet="Sheet1")%>%
  rename(MULakeNumber=`MU#`,
         waterBody=`Reservoir Name`, #rename some columns
         )%>%
  mutate(waterBody=ifelse(substr(waterBody,1,1)=="*",substr(waterBody,2,nchar(waterBody)),waterBody))%>% #remove a "*" at the beginning if it exists
  dplyr::select(MULakeNumber,waterBody)


#Create a waterBody name column####
waterBody_df<-ecoRegionMaster%>%
  mutate(MULakeNumber=as.character(MULakeNumber))%>%
  distinct(.)%>% #keep only the unique values for merging
  mutate(waterBody=case_when(
                  MULakeNumber=="3"~"Bowling Green (East)",
                  MULakeNumber=="454"~"Cedar Hill (Lake 1)",
                  MULakeNumber=="186"~"Carl DiSalvo",
                  MULakeNumber=="454"~"Cedar Hill (Lake 1)",
                  MULakeNumber=="179"~"Nodaway County",
                  .default=waterBody
         ))%>%
    mutate(samplingSite=paste0(waterBody," 1"))%>% #Add in sampling site for the dam, it is always the name with " 1" 
  add_row(MULakeNumber="401",waterBody="Field Blank",samplingSite="Field Blank")
#Matching waterbody df for field duplicates####
waterBody_df_FD<-waterBody_df%>%
  mutate(MULakeNumber=paste0(MULakeNumber," FD"))
  



###############################
#*Read in chlorophyll sheet####
  #Gives total chlorophyll, Chla corrected and Pheophytin#
#**Bring in the chlorophyll from the 'Final chl' sheet####
chlMaster_FinalChl<-read_excel(paste0(dir,"/2023 SLAP chl compiled 02-12-2024.xlsx"),sheet="Final Chl",skip=2)%>%
                rename(MULakeNumber=`Lake/Site`,
                       date=Date,
                       depthChl=Depth,
                       chlaLab_mgpL=`mg/L...11`,
                       ChlaCor_mgpL=`mg/L...13`,
                       Pheo_mgpL=`mg/L...15`)%>%
                mutate(date=ymd(date))

#Acid_fluor merge####   
Acid_fluor_merge<-chlMaster_FinalChl%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthChl,
         endDepth_m=NA,
         parameterType="Acid_fluor",
         unit="RFU",
         parameterValue=`Acid Fluor (RFU)`,
         parameterValueQCCodeID=NA
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#Tot_fluor merge####   
Tot_fluor_merge<-chlMaster_FinalChl%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthChl,
         endDepth_m=NA,
         parameterType="Tot_fluor",
         unit="RFU",
         parameterValue=`Tot Fluor (RFU)`,
         parameterValueQCCodeID=NA
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#CHL_A_COR merge####   
CHL_A_COR_merge<-chlMaster_FinalChl%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthChl,
         endDepth_m=NA,
         parameterType="CHL_A_COR",
         unit="mg/L",
         parameterValue=ChlaCor_mgpL,
         parameterValueQCCodeID=Flag...14
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#CHL_A_lab merge####   
CHL_A_lab_merge<-chlMaster_FinalChl%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthChl,
         endDepth_m=NA,
         parameterType="CHL_A_lab",
         unit="mg CHL_A/L",
         parameterValue=chlaLab_mgpL,
         parameterValueQCCodeID=Flag...12
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#PHEO merge####   
PHEO_merge<-chlMaster_FinalChl%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthChl,
         endDepth_m=NA,
         parameterType="PHEO",
         unit="mg PHEO/L",
         parameterValue=Pheo_mgpL,
         parameterValueQCCodeID=Flag...16
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)


#**Things to do: get rid of all with "Field Blank" in MULakeNumber
#**Things to do: get rid of field dupes (FD at the end of MULakeNumber)



###############################
#*Read in Ammonia from NH sheet####
#Gives total NH4 concentration#

#**Bring in the NH4 from the all sheet####
ammoniumMaster<-read_excel(paste0(dir,"/Ammonia SLAP 2023 mastersheet 02-08-24.xlsx"),sheet="Final NH4",skip=0)%>%
  rename(MULakeNumber=`Lake #`,
         date=Date,
         depthNH4=Depth,
         NH4_mgpL=`[NH4]` 
         )%>%
  mutate(date=ymd(date))

#NH4 merge####   
NH4_merge<-ammoniumMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthNH4,
         endDepth_m=`Depth of Epi (m)`,
         parameterType="NH4",
         unit="mg NH4-N/L",
         parameterValue=NH4_mgpL,
         parameterValueQCCodeID=`Flag (MDL 0.005)`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)


#**Things to do: check the units for NH4
#**Things2do: check for detection limits: rule if below?
#**Things2do: take average from duplicates
#**Things to do: get rid of all with "Field Blank" in MULakeNumber
#**Things to do: get rid of field dupes (FD at the end of MULakeNumber)

###############################
#*Read in chloride data from across all sheets####
#Gives anion Cl- concentrations#
#**Bring in the chloride from the 'Final Cl' sheet####
chlorideMaster<-read_excel(paste0(dir,"/Chloride SLAP 2023 masterfile 02-09-2024.xlsx"),sheet="Final Cl",skip=0)%>%
  rename(MULakeNumber=`Lake #`,
         date=`Date`,
         depthCl=Depth,
         cloride_mgpL=`[Cl]`
         )%>%
  mutate(date=ymd(date)) #%>%
  #dplyr::select(MULakeNumber,date,DepthTN,TN_mgpL)                

#CL merge####   
CL_merge<-chlorideMaster%>%
  mutate(
    Date=date,
    beginDepth=0,
    endDepth_char=depthCl,
    endDepth_m=`Depth of Epi (m)`,
    parameterType="CL",
    unit="mg/L",
    parameterValue=cloride_mgpL,
    parameterValueQCCodeID=`Flag (MDL 0.76`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)


###############################
#*Read in nitrate data####
#Gives anion NO3- concentrations#
#**Bring in the nitratre from the 'NO3 Final' sheet####
nitrateMaster<-read_excel(paste0(dir,"/Nitrate SLAP 2023 masterfile 02-08-2024.xlsx"),sheet="NO3 Final",skip=0)%>%
  rename(MULakeNumber=`Lake #`,
         date=`Date`,
         depthNO3=Depth,
         NO3_mgpL=`[NO3] mg/L`
  )%>%
  mutate(date=ymd(date)) #%>%
#dplyr::select(MULakeNumber,date,depthNO3,NO3_mgpL)                

#NO3 merge####   
NO3_merge<-nitrateMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthNO3,
         endDepth_m=`Depth of Epi (m)`,
         parameterType="NO3",
         unit="mg NO3-N/L",
         parameterValue=NO3_mgpL,
         parameterValueQCCodeID=`Flag (MDL = 0.004)`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)



#**Things to do: check the units for nitrate
#**Figure out what flags mean and what to do with them                     

###############################
#*Read in dissolved nitrogen data####
#Gives dissolved nitrogen and total nitrogen on two separate sheets#
#**Bring in the DN from the 'Final DN' sheet####
DNMaster<-read_excel(paste0(dir,"/TN + DN SLAP mastersheet 02-09-2024.xlsx"),sheet="Final DN",skip=0)%>%
  rename(MULakeNumber=`Lake #`,
         date=`Date`,
         depthDN=Depth,
         DN_mgpL=`[N]`
  )%>%
  mutate(date=ymd(date)) #%>%
#dplyr::select(MULakeNumber,date,depthNO3,NO3_mgpL)                

#TDN merge####   
TDN_merge<-DNMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthDN,
         endDepth_m=`Depth of Epi (m)`,
         parameterType="TDN",
         unit="mg N/L",
         parameterValue=DN_mgpL,
         parameterValueQCCodeID=`Flag (MDL 0.038)`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)


#**Bring in the TN from the 'Final DN' sheet####
TNMaster<-read_excel(paste0(dir,"/TN + DN SLAP mastersheet 02-09-2024.xlsx"),sheet="Final TN",skip=0)%>%
  rename(MULakeNumber=`Lake #`,
         date=`Date`,
         depthTN=Depth,
         TN_mgpL=`[N]`
  )%>%
  mutate(date=ymd(date)) #%>%
#dplyr::select(MULakeNumber,date,depthNO3,NO3_mgpL) 

#TDN merge####   
TN_merge<-TNMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthTN,
         endDepth_m=`Depth of Epi (m)`,
         parameterType="TN",
         unit="mg N/L",
         parameterValue=TN_mgpL,
         parameterValueQCCodeID=`Flag (MDL 0.038)`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)


#**Things to do: check the units for TDN and TN
#**Figure out what flags mean and what to do with them  

###############################
#*Read in Toxin data from across all sheets####
#Gives Microcystin, Cylindro, and Saxitoxin concentrations#
#**Bring in the Toxins from the Final toxins sheet####
toxinsMaster<-read_excel(paste0(dir,"/Toxin SLAP 2023 mastersheet 02-09-24.xlsx"),sheet="Final toxins",skip=0)%>%
  rename(MULakeNumber=`Lake #`,
         date=`Sample date`,
  )%>%
  mutate(date=as.Date(date)) #%>%
#dplyr::select(MULakeNumber,date,DepthTN,TN_mgpL)                

#ANA_tox merge####   
ANA_tox_merge<-toxinsMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char="SURF",
         endDepth_m=as.character(0),
         parameterType="ANA_tox",
         unit="ug/L",
         parameterValue=Anatoxin,
         parameterValueQCCodeID=`Anatoxin flag`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#CYL_tox merge####   
CYL_tox_merge<-toxinsMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char="SURF",
         endDepth_m=as.character(0),
         parameterType="CYL_tox",
         unit="ug/L",
         parameterValue=Cylindro,
         parameterValueQCCodeID=`Cylindro flag`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#MIC_tox merge####   
MIC_tox_merge<-toxinsMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char="SURF",
         endDepth_m=as.character(0),
         parameterType="MIC_tox",
         unit="ug/L",
         parameterValue=Microcystin,
         parameterValueQCCodeID=`Micro flag`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#SAX_tox merge####   
SAX_tox_merge<-toxinsMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char="SURF",
         endDepth_m=as.character(0),
         parameterType="SAX_tox",
         unit="ug/L",
         parameterValue=Saxitoxin, 
         parameterValueQCCodeID=`Saxitoxin flag`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#**Things to do: check the units for the toxins
#**Figure out what to do with the ?? for dates
#**Figure out what flags mean and what to do with them                 
                
###############################
#*Read in total phosphorus data####
#Gives total phosphorus#
#**Bring in the TP from the 'Final TP' sheet####
TPMaster<-read_excel(paste0(dir,"/TP SLAP 2023 mastersheet 02-09-2024.xlsx"),sheet="Final TP",skip=0)%>%
  rename(MULakeNumber=`Lake #`,
         date=`Date`,
         depthTP=Depth,
         TP_mgpL=`[P] mg/L`
  )%>%
  mutate(date=ymd(date)) #%>%
#dplyr::select(MULakeNumber,date,depthNO3,NO3_mgpL)                  

#TP merge####   
TP_merge<-TPMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthTP,
         endDepth_m=`Depth of Epi (m)`,
         parameterType="TP",
         unit="mg PO4-P/L",
         parameterValue=TP_mgpL,
         parameterValueQCCodeID=`Flag (MDL = 0.0016)`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)


###############################
#*Read in total suspended solids data####
#Gives total suspended solids#
#**Bring in the TSS from the 'Final TSS' sheet####
TSSMaster<-read_excel(paste0(dir,"/TSS SLAP 2023 masterfile 02-09-2024.xlsx"),sheet="Final TSS",skip=1)%>%
  rename(MULakeNumber=`Lake/Site`,
         date=`Date`,
         depthTSS=Depth,
         TSS_mgpL=`TSS/L...4`, #units?
         PIM_mgpL=`PIM/L...5`, #units?
         POM_mgpL=`POM/L...6`, #units?
  )%>%
  mutate(date=ymd(date)) #%>%
#dplyr::select(MULakeNumber,date,depthNO3,NO3_mgpL)                    

#PIM merge####   
PIM_merge<-TSSMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthTSS,
         endDepth_m=NA,
         parameterType="PIM",
         unit="mg/L",
         parameterValue=PIM_mgpL,
         parameterValueQCCodeID=`PIM/L...9`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#POM merge####   
POM_merge<-TSSMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthTSS,
         endDepth_m=NA,
         parameterType="POM",
         unit="mg/L",
         parameterValue=POM_mgpL,
         parameterValueQCCodeID=`POM/L...10`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#TSS merge####   
TSS_merge<-TSSMaster%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char=depthTSS,
         endDepth_m=NA,
         parameterType="TSS",
         unit="mg/L",
         parameterValue=TSS_mgpL,
         parameterValueQCCodeID=`TSS/L...8`
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#**Things to do: check the units for the TSS               

###############################
#*Read in the master filter log####
#Gives the correct depth of epi for each sampling date/time#
#**Bring in the filter file from the 'EPI' sheet####
filterMaster<-read_excel(paste0(dir,"/Master Spreadsheet Filter_Log_Sheet 2023 02-28-24.xlsx"),sheet="EPI",skip=0)%>%
  rename(MULakeNumber=`Lake #`,
         Date=Date,
         endDepth_m=`Depth of Epi (m)`,
     )%>%
  mutate(Date=ymd(Date),
         endDepth_char="EPI")%>%
  dplyr::select(MULakeNumber,Date,endDepth_char,endDepth_m)%>%
  distinct(.)%>%
  filter(!is.na(MULakeNumber))



###################################################################
#Read in the 2023 profile data for the surface DO, temp, and pH####
profileLevel3<-read_csv(paste0("03_Level3_Data/",year,"_Level3.csv"))

#DO merge####   
DO_merge<-profileLevel3%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char="SURF",
         endDepth_m="0.5",
         parameterType="DO",
         unit="mg/L",
         parameterValue=surface_doConcentration_mgpL,
         parameterValueQCCodeID=NA
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#Temp merge####   
Temp_merge<-profileLevel3%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char="SURF",
         endDepth_m="0.5",
         parameterType="Temp",
         unit="C",
         parameterValue=surface_temp_degC,
         parameterValueQCCodeID=NA
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#pH merge####   
pH_merge<-profileLevel3%>%
  mutate(
         Date=date,
         beginDepth=0,
         endDepth_char="SURF",
         endDepth_m="0.5",
         parameterType="pH",
         unit="unitless",
         parameterValue=surface_pH,
         parameterValueQCCodeID=NA
  )%>%
  dplyr::select(MULakeNumber,Date,beginDepth,endDepth_char,endDepth_m,parameterType,unit,parameterValue,parameterValueQCCodeID)

#############Lat/Long data####################
#Get out the handheld data for 2023########
handheld2023_LatLong<-profileLevel3%>%
                      group_by(MULakeNumber)%>%
                      summarize(site_latitude=mean(site_latitude,na.rm=TRUE), site_longitude=mean(site_longitude,na.rm=TRUE))

#Get in the dam locations for lat/long####  
latlongMetadata<-read_csv("07_MiscFiles/MissouriReservoir_Metadata_SiteData.csv")%>%
                 mutate(MULakeNumber=as.character(sprintf("%03d", MULakeNumber)))%>%
                 dplyr::select(-waterBody,-notes)

#Merge them together#####
latlong_best<-left_join(handheld2023_LatLong,latlongMetadata,by="MULakeNumber")%>%
  mutate(samplingSiteLatitude=ifelse(is.na(site_latitude),waterBodyLatitude,site_latitude), #If there is no handheld, use the dam location
         samplingSiteLongitude=ifelse(is.na(site_longitude),waterBodyLatitude,site_longitude))%>% #If there is no handheld, use the dam location
  dplyr::select(MULakeNumber,samplingSiteLatitude,samplingSiteLongitude)%>%
  mutate(MULakeNumber=ifelse(nchar(MULakeNumber)==3,as.character(as.numeric(MULakeNumber)),MULakeNumber)) #Make sure to convert 003 back to 3 here for merge

#Create a latlon dataframe for all the possible Field duplicate files
latlong_best_FD<-latlong_best%>%
                  mutate(MULakeNumber=paste0(MULakeNumber," FD"))


######################STACK ALL THE MERGE FILES IN LONG FORM###########################
#row_bind all the merge files:
databaseImport<-
          bind_rows(secchi_merge,
                    Acid_fluor_merge,
                    Tot_fluor_merge,
                    CHL_A_COR_merge,
                    CHL_A_lab_merge,
                    PHEO_merge,
                    NH4_merge,
                    NO3_merge,
                    TDN_merge,
                    TN_merge,
                    ANA_tox_merge,
                    CYL_tox_merge,
                    MIC_tox_merge,
                    SAX_tox_merge,
                    TP_merge,
                    PIM_merge,
                    POM_merge,
                    TSS_merge,
                    DO_merge,
                    Temp_merge,
                    pH_merge,
                    CL_merge
                    )%>%
            mutate(MULakeNumber=ifelse(!(is.na(as.numeric(MULakeNumber))),as.character(as.numeric(MULakeNumber)),MULakeNumber))%>% #This will make sure that 003 and 3 are both 3 for MULake number but also avoids converting Field Dupes, and other types of sites
            arrange(Date,MULakeNumber,parameterType)%>% #order by date, parameteType to match the 2022 import file
            mutate(endDepth_char=case_when( #Fix up all the endDepth_char
                    endDepth_char=="Epi"~"EPI",
                    endDepth_char=="SURF'"~"SURF",
                    endDepth_char=="Surf"~"SURF",
                    .default = endDepth_char
                    ))%>%
            mutate(endDepth_m=case_when( #fix the end depths, surface = 0; actual (secchi)= NA; anything from the profile is 0.5m
                            endDepth_char=="SURF"&parameterType=="DO"~as.character(0.5),
                            endDepth_char=="SURF"&parameterType=="pH"~as.character(0.5),
                            endDepth_char=="SURF"&parameterType=="Temp"~as.character(0.5),
                            endDepth_char=="SURF"~as.character(0),
                            .default=endDepth_m
                            ))%>%
            mutate(MULakeNumber=ifelse(substrRight(MULakeNumber,2)=="FD"&substrFromRight(MULakeNumber,3)!=" ",paste0(substr(MULakeNumber,1,3)," ",substrRight(MULakeNumber,2)),MULakeNumber))%>% #Check to see if the MULakeNumber field dupes have a space and add one if not
            left_join(.,bind_rows(latlong_best,latlong_best_FD),by=c("MULakeNumber"))%>% #add in the latitudes and longitudes, including for field dupes
            mutate(MULakeNumber=ifelse((MULakeNumber=="FB"|MULakeNumber=="Field Blank"),"401",MULakeNumber))%>% #rename all FB or FIeld blanks as 401
            left_join(.,bind_rows(dateTime,dateTime_FD),by=c("MULakeNumber","Date"))%>% #Add in the dateTime from the secchi
            left_join(.,bind_rows(waterBody_df,waterBody_df_FD),by=c("MULakeNumber"))%>% #Add in the waterBody name from the secchi
            left_join(.,filterMaster%>%rename(endDepth_m_2=endDepth_m)%>%mutate(MULakeNumber=ifelse((MULakeNumber=="FB"|MULakeNumber=="Field Blank"),"401",MULakeNumber)),by=c("MULakeNumber","Date","endDepth_char"))%>% #join in the correct endDepth_m from the filter master sheet. Have to rename it to avoid generating an endDepth_m.x column, also #Replace all field blank references with 401 (field blank equivelent MULakeNumber)
            mutate(endDepth_m=ifelse(is.na(endDepth_m_2),endDepth_m,endDepth_m_2))%>% #if the endDepth_m_2 from the filter master has a value, replace endDepth_m with that value
            mutate(endDepth_m=format(round(as.numeric(endDepth_m),digits=2),nsmall=2))%>% #make endDepth_m a numeric column now, no NAs introduced by coercion should pop up
            dplyr::select(-endDepth_m_2)%>% #Get rid of the duplicate endDepth_m_2 column
            #This converts to ug/L if needed and also rounds based on the correct number of reporting decimals, the format and nsmall is for display after rounding, these need to match each other, 
            #Warning: this converts the parameterValue into characters
            mutate(parameterValue=case_when(  
                                parameterType=="DO"&unit=="mg/L"~format(round(parameterValue,digits=2),nsmall=2),
                                parameterType=="Temp"&unit=="C"~format(round(parameterValue,digits=3),nsmall=3), 
                                parameterType=="pH"&unit=="unitless"~format(round(parameterValue,digits=2),nsmall=2), 
                                parameterType=="ANA_tox"&unit=="ug/L"~format(round(parameterValue,digits=3),nsmall=3),
                                parameterType=="CYL_tox"&unit=="ug/L"~format(round(parameterValue,digits=3),nsmall=3),
                                parameterType=="MIC_tox"&unit=="ug/L"~format(round(parameterValue,digits=3),nsmall=3),
                                parameterType=="SAX_tox"&unit=="ug/L"~format(round(parameterValue,digits=3),nsmall=3),
                                parameterType=="Acid_fluor"&unit=="RFU"~format(round(parameterValue,digits=2),nsmall=2),
                                parameterType=="Tot_fluor"&unit=="RFU"~format(round(parameterValue,digits=2),nsmall=2),
                                parameterType=="CHL_A_COR"&unit=="mg/L"~format(round(parameterValue*1000,digits=2),nsmall=2), #Convert to ug/L
                                parameterType=="CHL_A_lab"&unit=="mg CHL_A/L"~format(round(parameterValue*1000,digits=2),nsmall=2), #Convert to ug/L
                                parameterType=="PHEO"&unit=="mg PHEO/L"~format(round(parameterValue*1000,digits=2),nsmall=2), #Convert to ug/L
                                parameterType=="NH4"&unit=="mg NH4-N/L"~format(round(parameterValue*1000,digits=0),nsmall=0), #Convert to ug/L
                                parameterType=="NO3"&unit=="mg NO3-N/L"~format(round(parameterValue*1000,digits=0),nsmall=0), #Convert to ug/L
                                parameterType=="TDN"&unit=="mg N/L"~format(round(parameterValue*1000,digits=1),nsmall=1), #Convert to ug/L
                                parameterType=="TN"&unit=="mg N/L"~format(round(parameterValue*1000,digits=1),nsmall=1), #Convert to ug/L
                                parameterType=="TP"&unit=="mg PO4-P/L"~format(round(parameterValue*1000,digits=1),nsmall=1), #Convert to ug/L
                                parameterType=="TSS"&unit=="mg/L"~format(round(parameterValue,digits=1),nsmall=1),
                                parameterType=="PIM"&unit=="mg/L"~format(round(parameterValue,digits=1),nsmall=1),
                                parameterType=="POM"&unit=="mg/L"~format(round(parameterValue,digits=1),nsmall=1),
                                parameterType=="SECCHI"&unit=="m"~format(round(parameterValue,digits=2),nsmall=2),
                                TRUE~as.character(parameterValue)
                                ))%>%
            mutate(unit=case_when(  #This changes the unit column if the the numbers were converted
                                parameterType=="DO"&unit=="mg/L"~unit,
                                parameterType=="Temp"&unit=="C"~unit, 
                                parameterType=="pH"&unit=="unitless"~unit, 
                                parameterType=="ANA_tox"&unit=="ug/L"~unit,
                                parameterType=="CYL_tox"&unit=="ug/L"~unit,
                                parameterType=="MIC_tox"&unit=="ug/L"~unit,
                                parameterType=="SAX_tox"&unit=="ug/L"~unit,
                                parameterType=="Acid_fluor"&unit=="RFU"~unit,
                                parameterType=="Tot_fluor"&unit=="RFU"~unit,
                                parameterType=="CHL_A_COR"&unit=="mg/L"~"ug/L", #Convert to ug/L
                                parameterType=="CHL_A_lab"&unit=="mg CHL_A/L"~"ug/L", #Convert to ug/L
                                parameterType=="PHEO"&unit=="mg PHEO/L"~"ug PHEO/L", #Convert to ug/L
                                parameterType=="NH4"&unit=="mg NH4-N/L"~"ug/L", #Convert to ug/L
                                parameterType=="NO3"&unit=="mg NO3-N/L"~"ug/L", #Convert to ug/L
                                parameterType=="TDN"&unit=="mg N/L"~"ug/L", #Convert to ug/L
                                parameterType=="TN"&unit=="mg N/L"~"ug/L", #Convert to ug/L
                                parameterType=="TP"&unit=="mg PO4-P/L"~"ug/L", #Convert to ug/L
                                parameterType=="TSS"&unit=="mg/L"~unit,
                                parameterType=="PIM"&unit=="mg/L"~unit,
                                parameterType=="POM"&unit=="mg/L"~unit,
                                parameterType=="SECCHI"&unit=="m"~unit,
                                .default=unit
                                ))%>%
              #Create sample QAQC type column that is either Field blank, Field duplicate, or Sample
              mutate(sampleQAQCType=case_when(MULakeNumber=="401"~"Field blank", 
                                              substrRight(MULakeNumber,2)=="FD"~"Field duplicate",
                                              .default="Sample"))

#Gives all the current parameterType and units unique to the dataset
#databaseImport%>%dplyr::select(parameterType,unit)%>%distinct(.)%>%print(n=Inf)
  
#Specific to 2023, change dates and redo the join with dateTime####
databaseImport2<-databaseImport%>%mutate(Date=case_when(
                              MULakeNumber=="276"&Date==as.Date("2023-05-17")~as.Date("2023-05-18"),
                              MULakeNumber=="46"&Date==as.Date("2023-05-23")~as.Date("2023-05-22"),
                              MULakeNumber=="114"&Date==as.Date("2023-06-14")~as.Date("2023-06-15"),
                              .default=Date
                              ))%>%
                              mutate(endDepth_m=case_when(
                                MULakeNumber=="276"&Date==as.Date("2023-05-18")&endDepth_char=="EPI"~"2.04", #adjustment from MULake lab
                                MULakeNumber=="114"&Date==as.Date("2023-06-15")&endDepth_char=="EPI"~"1.80", #adjustment from MULake lab, check if this lake number is correct
                                MULakeNumber=="197 FD"&Date==as.Date("2023-06-26")&endDepth_char=="EPI"~"0.98", 
                                MULakeNumber=="211 FD"&Date==as.Date("2023-07-31")&endDepth_char=="EPI"~"3.31",
                                .default=endDepth_m
                              ))%>%
                              mutate(MULakeNumber=ifelse(MULakeNumber=="197FD","197 FD",MULakeNumber))%>%
              dplyr::select(-dateTime)%>%
              left_join(.,dateTime,by=c("MULakeNumber","Date"))





#Database DNR####
databaseDNR<-databaseImport2%>%
    #remove specific rows for isothermal
    filter(!(MULakeNumber=="440"&Date==as.Date("2023-04-27")))%>%
    filter(!(MULakeNumber=="46"&Date==as.Date("2023-06-12")))%>%
    filter(!(MULakeNumber=="163"&Date==as.Date("2023-06-13")))%>%
    filter(!(MULakeNumber=="121"&Date==as.Date("2023-07-19")))%>%
    filter(!(MULakeNumber=="121"&Date==as.Date("2023-08-10")))%>%
    filter(!(MULakeNumber=="30"&Date==as.Date("2023-08-15")))%>%
    filter(!(MULakeNumber=="446-01-00"&Date==as.Date("2023-09-19")))%>%
    #Only keep May to September
    filter(month(Date)>=5&month(Date)<=9)%>%
    
    filter(!(parameterType=="CL"))%>% #Remove chloride samples
    filter(!(parameterType=="Tot_fluor"))%>% #Remove fluorometer RFU samples
    filter(!(parameterType=="Acid_fluor")) #Remove fluorometer acidied RFU samples


 #Export database for DNR####
write_csv(databaseDNR,file=paste0("06_Outputs/",year,"_MissouriReservoirsForDNR_v1.csv"))


#RANDOM CODE TO HELP WITH MERGING AND IDing specific cases####
#databaseDNR%>%filter(MULakeNumber=="114"&Date==as.Date("2023-06-15"))
#unique(databaseImport2$MULakeNumber)
#databaseDNR%>%filter(MULakeNumber=="46"&Date==as.Date("2023-05-22"))%>%print(n=Inf)
#databaseImport%>%filter(MULakeNumber=="FB"&Date==as.Date("2023-07-10"))%>%print(n=Inf)
#databaseImport%>%filter(MULakeNumber=="211 FD")%>%print(n=Inf)
#filterMaster%>%filter(MULakeNumber=="211"&Date==as.Date("2023-07-31"))%>%dplyr::select(endDepth_m)%>%pull()
#databaseImport2%>%filter(MULakeNumber=="197 FD"|MULakeNumber=="197FD")%>%print(n=Inf)
