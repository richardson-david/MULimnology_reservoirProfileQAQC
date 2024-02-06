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


#Read in master parameter sheets####

#*Read in chlorophyll sheet####
  #Gives total chlorophyll, Chla corrected and Pheophytin#
#**Read in Chlorophyll master from data for pivot table sheet####
chlMaster<-read_excel(paste0(dir,"/2023 SLAP chl compiled.xlsx"),sheet="data for pivot table")%>%
            rename(MULakeNumber=`Lake/Site`,
                   dateTime=Date,
                   Total_chl_ugpL=TCHL,
                   Chla_ugpL=CHLa,
                   Pheo_ugpL=Pheo)

#**Bring in the chlorophyll from the all sheet####
chlMaster_All<-read_excel(paste0(dir,"/2023 SLAP chl compiled.xlsx"),sheet="All",skip=2)%>%
                rename(MULakeNumber=`Lake/Site`,
                       dateTime=Date,
                       DepthChl=Depth,
                       Total_chl_ugpL=`ug/L...16`,
                       Chla_ugpL=`ug/L...17`,
                       Pheo_ugpL=`ug/L...18`)%>%
                dplyr::select(MULakeNumber,dateTime,DepthChl,Total_chl_ugpL,Chla_ugpL,Pheo_ugpL)

#**Things to do: get rid of all with "Field Blank" in MULakeNumber
#**Things to do: get rid of field dupes (FD at the end of MULakeNumber)

#*Read in Ammonia from NH sheet####
#Gives total NH4 concentration#

#**Bring in the chlorophyll from the all sheet####
Ammonium_NH<-read_excel(paste0(dir,"/Ammonia SLAP 2023 mastersheet.xlsx"),sheet="NH",skip=0)%>%
  rename(MULakeNumber=`Lake #`,
         dateTime=Date,
         DepthNH4=Depth,
         NH4_ugpL=`[NH4]` 
         )%>%
  dplyr::select(MULakeNumber,dateTime,DepthChl,NH4_ugpL)

#**Things to do: check the units for NH4
#**Things2do: check for detection limits: rule if below?
#**Things2do: take average from duplicates
#**Things to do: get rid of all with "Field Blank" in MULakeNumber
#**Things to do: get rid of field dupes (FD at the end of MULakeNumber)