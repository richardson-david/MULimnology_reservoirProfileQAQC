##Level0 for 2017 only: DataInput, Decap, Base QA/QC Missouri Reservoirs####
##Created 22Jun2023 by David Richardson (hereafter DCR)
##Reads in level 0 data from the YSI sonde, formats the files, and does a basic QA/QC

#Pseudo Code####
  #Identify folders 
  #create a log file within 01_RawData/2022_sonde_profiles/Level0_RawData
  #Read in 1 week of files
  #Decap the headers
  #Do basic QA/QC, look for max/mins of each column (OTHER STUFF HERE)

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
year<-2017

#Read in the sensor limits file####
sensorLimits<-read_csv("00_Level0_Data/MissouriReservoirs-YSI_EXO3_SensorLimits.csv")


#*Set the directory path here####
dirPath<-paste0("00_Level0_Data/",year,"_Level0_RawData")

#Identify all the individual .csv files####
Level0_files<-list.files(dirPath,pattern = "*.csv")

#Create log of the existing files, can maybe refer back to this and amend as new files are added####
  #columns include when they have been loaded and the date they are done####
  #***This will not be created each time, just one initial and then loaded in####
Level0_files_log<-tibble(Level0_profiles=Level0_files,Level0to1_done="No",Level0to1_done_date=as_date(NA))%>% #Marks that Level 0 to 1 is done and what date
                  separate(Level0_profiles,c("MULakeNumber","year","month","day","csv"),remove=FALSE)%>% #use separate function to pull out each component of the 
                  #HERE IS WHERE TO TRY TO FIGURE OUT WHERE THE ARMS WOULD GO####
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
                         ) 


#For loop through the files####
  #Check the log to see if the file has already been loaded
  #If not, then read in each file, 
  #lop of the header
  #Do some basic QA/QC
  #export the file to the Level1 folder as csv
  #Update the week log folder
  #Update the profile log folder
  
                # #Figure out how to debug the different file types####
                # fileIndex<-12
                # #find the file encoding
                # fileEncoding_set<-guess_encoding(paste0(dirPath,"/",Level0_files_log$Level0_profiles[fileIndex]),n_max=1000)[1,1]%>%pull()
                # firstEntry<-read.table(paste0(dirPath,"/",Level0_files_log$Level0_profiles[fileIndex]),nrows=1,header=F,skipNul=TRUE,sep=",",fileEncoding=fileEncoding_set)$V1
                # 
                # if(firstEntry=="sep="){
                #   skip_rows=9
                # }else{
                #   skip_rows=8
                # }
                # 
                # read.csv(file=paste0(dirPath,"/",Level0_files_log$Level0_profiles[fileIndex]),skip=skip_rows,fileEncoding=fileEncoding_set)


#***This 1 can be subbed in with the new file index from the log####
    #Debug fileIndex<-57
    #Debug: fileIndex 
    #       Level0_files_log$Level0_profiles[fileIndex]
for(fileIndex in 1:length(Level0_files)){
  #Check to see if the file has been read in already####
  #Do some basic checks of the file name, can catch errors here####
  #Read in the file####
    #Start by checking what the file encoding each file has####
    fileEncoding_set<-guess_encoding(paste0(dirPath,"/",Level0_files_log$Level0_profiles[fileIndex]),n_max=1000)[1,1]%>%pull()
    
    #all skip_rows are 0
    skip_rows=0
    
    #print(fileIndex)
    
        
    #This also decaps the first bunch of rows to start with row skip_rows
    #The file encoding is necessary because there are some odd characters in the file that need to be bypassed
     readProfile<-tibble(read.csv(file=paste0(dirPath,"/",Level0_files_log$Level0_profiles[fileIndex]),skip=skip_rows,fileEncoding=fileEncoding_set))%>%
                                mutate(Time..HH.mm.ss.=if('Time..HH.mm.ss.'%in% colnames(.)){Time..HH.mm.ss.}else{if('Time..HH.MM.SS.'%in% colnames(.)){Time..HH.MM.SS.}else{if('Time'%in% colnames(.)){Time}else{"12:00:00"}}})%>% #Create a Time variable that selects from any of the column headers... if nothing exists, then assign 12:00
                                dplyr::select(-any_of(c("Time..HH.MM.SS.","Time","X")))%>% #also removes the X column which is row names
                                mutate(Date..MM.DD.YYYY.=if('Date..MM.DD.YYYY.'%in% colnames(.)){Date..MM.DD.YYYY.}else{if('Date'%in% colnames(.)){Date}else{paste0(Level0_files_log$month[fileIndex],"/", Level0_files_log$day[fileIndex],"/", Level0_files_log$year[fileIndex])}})%>% #Create a Time variable that selects from any of the column headers... if nothing exists, then assign 12:00
                                dplyr::select(-any_of(c("Date")))%>%
                                mutate(date=mdy(Date..MM.DD.YYYY.),  #Convert date to date, time to time, merge to a dateTime variable####
                                      dateTime=ymd_hms(paste(date,Time..HH.mm.ss.,sep=" ")),
                                      MULakeNumber=strsplit(Level0_files_log$Level0_profiles[fileIndex],'_')[[1]][1], #Extract the MULakeNumber from the file name
                                      )%>%
                              dplyr::select(-any_of(c("Site","Unit.ID","User.ID","Site.Name","Chlorophyll.ug.L","Chl.ug.L","BGA.PC.ug.L","pH.mV","Battery.V","Cable.Pwr.V","Date..MM.DD.YYYY.","Time..HH.mm.ss.","Time..Fract..Sec.","nLF.Cond.µS.cm","Cond.µS.cm")))%>% #get rid of a number of unneccessary columns
                              #Here is where units/column names can be corrected####
                              mutate(Pressure.bar.a=if("Pressure.bar.a" %in% names(.)){Pressure.bar.a}else{if("Pressure.psi.a" %in% names(.)){Pressure.psi.a/14.696}else{NA}}, #Check if there is pressure with different units and convert if it is in psi
                                     Barometer.mbars=if("Barometer.mbars" %in% names(.)){Barometer.mbars}else{if("Barometer.mmHg" %in% names(.)){Barometer.mmHg/0.75}else{if("mmHg" %in% names(.)){mmHg/0.75}else{NA}}} #Check if there is hand held pressure with different units and convert if it is in mmHg 
                                      )%>% 
                              #Here is sorting out all the different column names and making sure they are captured####
                              mutate(Temp..C=if("Temp..C" %in% names(.)){Temp..C}else{if("X.C" %in% names(.)){X.C}else{if("Temp" %in% names(.)){Temp}else{NA}}}, #Check if there is temperature in different column headers
                                     ODO...sat=if("ODO...sat" %in% names(.)){ODO...sat}else{if("DO.." %in% names(.)){DO..}else{if("DO." %in% names(.)){DO.}else{if("DO_percent" %in% names(.)){DO_percent}else{NA}}}}, #Check if there is DO in different column headers
                                     ODO.mg.L=if("ODO.mg.L" %in% names(.)){ODO.mg.L}else{if("DO.mg.L" %in% names(.)){DO.mg.L}else{if("DO.mg" %in% names(.)){DO.mg}else{if("DO_mgpL" %in% names(.)){DO_mgpL}else{NA}}}}, #Check if there is DO sat in different column headers
                                     SpCond.µS.cm=if("SpCond.µS.cm" %in% names(.)){SpCond.µS.cm}else{if("C.uS.cm" %in% names(.)){C.uS.cm}else{if("SPC.uS.cm" %in% names(.)){SPC.uS.cm}else{if("Cond" %in% names(.)){Cond}else{NA}}}}, #Check if there is Specific conductivity in different column headers
                                     Turbidity.FNU=if("Turbidity.FNU" %in% names(.)){Turbidity.FNU}else{if("FNU" %in% names(.)){FNU}else{if("Turb" %in% names(.)){Turb}else{NA}}}, #Check if there is turbidity in different column headers
                                     Chlorophyll.RFU=if("Chlorophyll.RFU" %in% names(.)){Chlorophyll.RFU}else{if("Chl.RFU" %in% names(.)){Chl.RFU}else{if("CHL.rfu" %in% names(.)){CHL.rfu}else{if("CHL" %in% names(.)){CHL}else{NA}}}}, #Check if there is chl rfu in different column headers
                                     BGA.PC.RFU=if("BGA.PC.RFU" %in% names(.)){BGA.PC.RFU }else{if("BGA-PC RFU" %in% names(.)){`BGA-PC RFU`}else{if("PC.rfu" %in% names(.)){PC.rfu}else{if("PC" %in% names(.)){PC}else{NA}}}}, #Check if there is BGA rfu in different column headers: RIGHT NOW THIS COLUMN NAME APPEARS TO BE THE SAME - COMMENTED IT OUT FOR NOW
                                     Sal.psu=if("Sal.psu" %in% names(.)){Sal.psu}else{NA}, #checks if salinity exists, if not, puts in column of NA
                                     TDS.mg.L=if("TDS.mg.L" %in% names(.)){TDS.mg.L}else{NA}, #checks if tds exists, if not, puts in column of NA
                                     ORP.mV=if("ORP.mV" %in% names(.)){ORP.mV}else{if("ORP" %in% names(.)){ORP}else{NA}}, #checks if orp mv exists, if not, puts in column of NA
                                     GPS.Latitude..=if("GPS.Latitude.." %in% names(.)){GPS.Latitude..}else{NA}, #checks if latitude exists, if not, puts in column of NA
                                     GPS.Longitude..=if("GPS.Longitude.." %in% names(.)){GPS.Longitude..}else{NA}, #checks if longitude exists, if not, puts in column of NA
                                     Altitude.m=if("Altitude.m" %in% names(.)){Altitude.m}else{NA}, #checks if altitude exists, if not, puts in column of NA
                                     pH=if("pH" %in% names(.)){pH}else{NA}, #checks if altitude exists, if not, puts in column of NA
                                     Depth.m=if("Depth.m" %in% names(.)){Depth.m}else{if("DEP.m" %in% names(.)){DEP.m}else{if("Depth" %in% names(.)){as.numeric(Depth)}else{NA}}} #Check if there is depth column in different column headers
                                     )%>%
                              mutate(Vertical.Position.m=if("Vertical.Position.m" %in% names(.)){Vertical.Position.m}else{if("VPos.m" %in% names(.)){VPos.m}else{if("Depth.m" %in% names(.)){Depth.m}else{NA}}})%>% #Check if there is vertical position column and if not, use the depth column that has been previously renamed
                              dplyr::select(-any_of(c("Depth","VPos.m","Pressure.psi.a","Barometer.mmHg","mmHg","X.C","Temp","DO..","DO.","ODO...local","DO.mg.L","DO.mg","C.uS.cm","SPC.uS.cm","Cond","ORP","FNU","Turb","PC.rfu","PC.ug","Chl.RFU","CHL.rfu","CHL.ug","NH4.N.mg.L","NO3.N.mg.L","Cl.mg.L","TSS.mg.L","BGA-PC RFU","phycocyaninBGA_RFU","BGA.PE.RFU","BGA.PE.ug.L","fDOM.RFU","fDOM.QSU","DEP.m")))%>% #removes the column if it exists
                              rename(chlorophyll_RFU=Chlorophyll.RFU, #Rename a number of variables to fit the convention with _ representing the distinction between label and units
                                     depth_m=Depth.m,
                                     doSaturation_percent=ODO...sat,
                                     doConcentration_mgpL=ODO.mg.L,
                                     orp_mV=ORP.mV,
                                     waterPressure_barA=Pressure.bar.a, #barA is absolute pressure where absolute zero is its zero point; bar(g) is gauge pressure that uses atmopsheric pressure as its zero point
                                     salinity_psu=Sal.psu,
                                     specificConductivity_uSpcm=SpCond.µS.cm,
                                     phycocyaninBGA_RFU=BGA.PC.RFU,
                                     tds_mgpL=TDS.mg.L,
                                     turbidity_FNU=Turbidity.FNU,
                                     temp_degC=Temp..C,
                                     verticalPosition_m=Vertical.Position.m,
                                     latitude=GPS.Latitude..,
                                     longitude=GPS.Longitude..,
                                     altitude_m=Altitude.m,
                                     barometerAirHandheld_mbars=Barometer.mbars 
                                     )%>%
                          mutate(depthDiff_m=c(99,diff(depth_m)), #Create a depth difference column that represents the difference of the depths for each consecutive reading, set the first difference at 99 so it will be kept
                                 verticalPositionDiff_m=c(99,diff(verticalPosition_m)) #Create a depth difference column that represents the difference of the depths for each consecutive reading, set the first difference at 99 so it will be kept
                                 )
     
     #Store the number of rows in the log####
     Level0_files_log$nrow_original_Level0[fileIndex]<-nrow(readProfile)
     
     #Just checking to see if there are wrong dates in here####
        #if(readProfile[1,"date"]=="2014-11-14"){print(Level0_files_log$Level0_profiles[fileIndex])}
     #Check to see if the dates match the file name
        #if(readProfile[1,"date"]==ymd(paste(Level0_files_log$year[fileIndex],"_",Level0_files_log$month[fileIndex],"_",Level0_files_log$day[fileIndex],sep=""))){}else{print(Level0_files_log$Level0_profiles[fileIndex])}
     ####################################################################
     #replace the dates further up in the read in tibble code (line 108) 
     
     
     #If there are more than 400 rows - this means the profile was lowered extremely slowly and the qaqc will delete all rows
     #If this is true, then average to the nearest 10 seconds. 
     #Create a column that is the nearest 10 seconds, then average by that####
     if(nrow(readProfile)>250){
                            readProfile<-readProfile%>%
                                         mutate(dateTime_Round=round_date(dateTime,"3 seconds"))%>%
                                         group_by(dateTime_Round)%>%
                                         summarise_each(funs(mean(., na.rm = TRUE)))%>%
                                         #recalculate the depthDiff
                                         mutate(depthDiff_m=c(99,diff(depth_m)), #Create a depth difference column that represents the difference of the depths for each consecutive reading
                                                verticalPositionDiff_m=c(99,diff(verticalPosition_m)) #Create a depth difference column that represents the difference of the depths for each consecutive reading
                                         )%>%
                                         mutate(MULakeNumber=Level0_files_log$MULakeNumber[fileIndex])%>% #replace the MULakeNumber from the log
                                         dplyr::select(-dateTime_Round)
     }else{} #do nothing
     
     #Store the number of rows after this averaging to see if it is reduced in size in the log####
     Level0_files_log$nrow_readProfile[fileIndex]<-nrow(readProfile)
     
    #Graph the depth_m and depth difference####
      #ggplot(data=readProfile,aes(x=dateTime,y=depth_m))+geom_point()
      #ggplot(data=readProfile,aes(x=dateTime,y=depthDiff_m))+geom_point()+geom_hline(yintercept=0.03) #Look at the depth differences; find a cutoff that works. In this case, you might lose some points in the middle of the profile
      #ggplot(data=readProfile,aes(x=dateTime,y=verticalPosition_m))+geom_point()
      #ggplot(data=readProfile,aes(x=dateTime,y=verticalPositionDiff_m))+geom_point()+geom_hline(yintercept=0.03) #Look at the depth differences; find a cutoff that works. In this case, you might lose some points in the middle of the profile
      
    #Chop off the bottom and top for anomalous values - trim based on . Perhaps use the difference (diff) of the depth. If the diff is <0.03 m, then remove the next one.#### 
    qaqcProfile<-readProfile%>%
                filter(verticalPosition_m>=0)%>% #Removes any readings from the top of the profile with negative depths
                filter(verticalPositionDiff_m>0.02) #Remove any readings from the profile with a vertical position difference >0.03, should set this globally. This is conservative and might lose some readings from the top (bouncing boat/waves), middle (not lowering sonde fast enough), and bottom (sonde hit the bottom and is not moving)
                
    
    #Basic checks for error codes and makes any NA data above or below the sensor bounds as NA####
    qaqcProfile<-qaqcProfile%>%mutate(
                           chlorophyll_RFU=qaqc_bounds(chlorophyll_RFU,sensorLimits),
                           depth_m=qaqc_bounds(depth_m,sensorLimits),
                           doSaturation_percent=qaqc_bounds(doSaturation_percent,sensorLimits),
                           doConcentration_mgpL=qaqc_bounds(doConcentration_mgpL,sensorLimits),
                           orp_mV=qaqc_bounds(orp_mV,sensorLimits),
                           waterPressure_barA=qaqc_bounds(waterPressure_barA,sensorLimits),
                           salinity_psu=qaqc_bounds(salinity_psu,sensorLimits),
                           specificConductivity_uSpcm=qaqc_bounds(specificConductivity_uSpcm,sensorLimits),
                           phycocyaninBGA_RFU=qaqc_bounds(phycocyaninBGA_RFU,sensorLimits),
                           tds_mgpL=qaqc_bounds(tds_mgpL,sensorLimits),
                           turbidity_FNU=qaqc_bounds(turbidity_FNU,sensorLimits),
                           pH=qaqc_bounds(pH,sensorLimits),
                           temp_degC=qaqc_bounds(temp_degC,sensorLimits),
                           verticalPosition_m=qaqc_bounds(verticalPosition_m,sensorLimits),
                           latitude=qaqc_bounds(latitude,sensorLimits),
                           longitude=qaqc_bounds(longitude,sensorLimits),
                           altitude_m=qaqc_bounds(altitude_m,sensorLimits),
                           barometerAirHandheld_mbars=qaqc_bounds(barometerAirHandheld_mbars,sensorLimits)
                           )%>%
                          mutate(altitude_m=ifelse(altitude_m==99.9,NA,altitude_m)) #99.9 seems to be an error code, only a little bit of the southeastern part of missouri is actually at that elevation of 99.9 m or 327.7559 feet (https://oembed-dnr.mo.gov/document-search/surface-elevation-map-mo-pub2874/pub2874)
      
                  
    
    #Store any details in the log####
    Level0_files_log$maxDepth_m[fileIndex]<-max(qaqcProfile$verticalPosition_m,na.rm=TRUE)
    Level0_files_log$latitude[fileIndex]<-mean(qaqcProfile$latitude,na.rm=TRUE)
    Level0_files_log$longitude[fileIndex]<-mean(qaqcProfile$longitude,na.rm=TRUE)
    Level0_files_log$altitude_m[fileIndex]<-mean(qaqcProfile$altitude_m,na.rm=TRUE)
    Level0_files_log$barometerAirHandheld_mbars[fileIndex]<-mean(qaqcProfile$barometerAirHandheld_mbars,na.rm=TRUE)
    Level0_files_log$doConcentration_mgpL_bottom[fileIndex]<-readProfile$doConcentration_mgpL[length(readProfile$doConcentration_mgpL)]
    Level0_files_log$doConcentration_mgpL_bottom5mean[fileIndex]<-mean(readProfile$doConcentration_mgpL[(length(readProfile$doConcentration_mgpL)-5):length(readProfile$doConcentration_mgpL)],na.rm=TRUE)
    Level0_files_log$nrow_Level1[fileIndex]<-nrow(qaqcProfile)    #Store number of rows in qaqcProfile    
              
    #Print each file to level1####
      #*level 1 directory####
      level1_dir<-paste0("01_Level1_Data/",year,"_Level1_Data/")
    
    #Remove the differencing columns and reorg the columns####
    qaqcProfile<-qaqcProfile%>%dplyr::select(-depthDiff_m,-verticalPositionDiff_m)%>%mutate(dateTime=ymd_hms(dateTime))%>%
                  dplyr::select(MULakeNumber,date,dateTime,depth_m,verticalPosition_m,temp_degC,doConcentration_mgpL,doSaturation_percent,chlorophyll_RFU,phycocyaninBGA_RFU,turbidity_FNU,pH,orp_mV,specificConductivity_uSpcm,salinity_psu,tds_mgpL,waterPressure_barA,latitude,longitude,altitude_m,barometerAirHandheld_mbars)

    #Check for duplicate depth readings here and in most cases, just take the first reading at each depth####
    if(nrow(qaqcProfile%>%group_by(verticalPosition_m)%>%filter(n()>1))>0){
      qaqcProfile<-qaqcProfile%>%group_by(verticalPosition_m)%>% #group by depth
        summarize(across(MULakeNumber:barometerAirHandheld_mbars,first),  #take the first value from character columns, average from numeric
        )%>% #end of summarize
        dplyr::select(MULakeNumber:depth_m,verticalPosition_m,temp_degC:barometerAirHandheld_mbars) #Put the columns back in order
      }
      
      
    #Write out Level1 csv in the file####
    write_csv(qaqcProfile,file=paste0(level1_dir,Level0_files_log$Level1FileName[fileIndex]))
    
    #Set the log as done####
    Level0_files_log$Level0to1_done[fileIndex]<-"Yes" #This profile has been exported
    Level0_files_log$Level0to1_done_date[fileIndex]<-as_date(today()) #Set the date run as today

    }
  
#Export the log####
write_csv(Level0_files_log,file=paste0("06_Outputs/",year,"_QAQC_log.csv"))
