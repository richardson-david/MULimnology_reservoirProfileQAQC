##Level2: Publishing level2 profiles with EDI####
##Created 28Jul2023 by David Richardson (hereafter DCR)
##Reads in files from the 04_EDI folder and creates EML file for publication in EDI####
##See here for additional instructions from Cayelan Carey's lab: https://github.com/CareyLabVT/Reservoirs/blob/master/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEMLChemistry/2022/MakeEMLChemistry.R

# Steps for setting up EML metadata ####
if(!require(EMLassemblyline)){install.packages("EMLassemblyline")}
if(!require(devtools)){install.packages("devtools")}
if(!require(maps)){install.packages("maps")}
if(!require(tidyverse)){install.packages("tidyverse")}

#Load packages
library(devtools)
#install_github("EDIorg/EMLassemblyline", force=T)
# Install from GitHub
remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

#Ggplot can work well with maps package
library(maps)
library(tidyverse)

#Create files and attribute tables####
  #*Read in metadata from the database####
  metaDataFromDatabase<-read_csv("07_MiscFiles/MissouriReservoir_Metadata_SiteData_full.csv")
  #*Check the dictionary for units####
    #view_unit_dictionary()
  #*modify the column headers, keep the notes column
  metaDataFromDatabase_edi<-metaDataFromDatabase%>%rename(waterBodyLatitude_degree=waterBodyLatitude,waterBodyLongitude_degree=waterBodyLongitude)
  #*write out the csv file####
  write_csv(x=metaDataFromDatabase_edi,file="04_EDI/MissouriReservoirs_Metadata_SiteData.csv")
  #*Create attribute table for the lat/long metadata####
  metaDataFromDatabase_attributeTable<-tibble(attributeName=names(metaDataFromDatabase_edi),
         attributeDefinition=c("Missouri University identifier for that lake or reservoir","Most commonly used name","Latitude in decimal degrees","Longitude in decimal degrees","Notes on locations"),
         class=c("character","character","numeric","numeric","character"),
         unit=c("","","degree","degree",""),
         dateTimeFormatString="",
         missingValueCode=NA,
         missingValueCodeExplanation=rep("MissingValue",length(names(metaDataFromDatabase_edi))))
  #*write out the attribute table####
  write_tsv(x=metaDataFromDatabase_attributeTable,file="04_EDI/attributes_MissouriReservoirs_Metadata_SiteData.txt")
  
  
#Create files and attribute tables for all level 2 data####
  #*Identify all the individual .csv files####
  Level2_files<-list.files("02_Level2_Data/",pattern = "*.csv")
  #Set the year - this will be in the for loop####
  #file.index<-5
  for(file.index in 1:length(Level2_files)){
  #*Read in a level 2 file####
  temp_level2<-read_csv(paste("02_Level2_Data/",Level2_files[file.index],sep=""))%>%
                mutate(date=as.character(date), #make sure the date exports correctly
                  dateTime=as.character(dateTime)) #make sure the dateTime exports correctly
  
  #*pull out the year####
  Extract_year<-sub("\\_.*", "", Level2_files[file.index])
  #*rename extract year for historical####
  if(Extract_year=="Historical"){Extract_year<-"1989-2016"}
  #Create a new EDI friendly file name####
  fileName<-paste("04_EDI/MissouriReservoirs_ProfileData_",Extract_year,".csv",sep="")
  #*Paste in EDI file####
  write_csv(x=temp_level2,file=fileName)

  #*Check the dictionary for units####
  #view_unit_dictionary()
  #*Create attribute definitions for all level2 data####
  attributeDefinition=c("Missouri University identifier for that lake or reservoir",
                        "Date of sampling",
                        "Date and time of sampling. All data were collected in the central time zone of the U.S.A., with daylight savings time observed",
                        "Water depth where sensor reading was measured",
                        "Water temperature",
                        "Dissolved oxygen concentration",
                        "Dissolved oxygen saturation",
                        "Total Chlorophyll a measured in RFU",
                        "Blue-Green Algae phycocyanin measured in RFU",
                        "Turbidity measured in FNU",
                        "Salinity measured in PSU",
                        "Specific conductivity",
                        "Total dissolved solids",
                        "Oxidation Reduction Potential",
                        "pH - potential of hydrogen",
                        "Latitude measured on the sonde handheld during sampling",
                        "Longitude measured on the sonde handheld during sampling",
                        "Altitude measured on the sonde handheld during sampling",
                        "Air barometer pressure measured on the sonde handheld during sampling"
                        )
  #*Create attribute class for all level2 data####
  attributeclass<-c("character","Date","Date",rep("numeric",16))
  
  #*Create attribute unit for all level2 data####
  attributeUnit<-c("",
                   "",
                   "",
                   "meter",
                   "celsius",
                   "milligramsPerLiter",
                   "percent",
                   "RelativeFluorescenceUnits", #Custom
                   "RelativeFluorescenceUnits", #Custom
                   "FormazinNephelometricUnits", #Custom
                   "PracticalSalinityUnit", #Custom
                   "microSiemensPerCentimeter", #Custom
                   "milligramsPerLiter",
                   "millivolt",
                   "dimensionless",
                   "degree",
                   "degree",
                   "meter",
                   "millibar"
                   )
  
  #*Create the column for the dateTime formats#### 
  attributeDateTimeFormatString<-c("","YYYY-MM-DD","YYYY-MM-DD hh:mm:ss",rep("",16))
  
  #*Create attribute table for the lat/long metadata####
  Level2_attributeTable<-tibble(attributeName=names(temp_level2),
                                              attributeDefinition=attributeDefinition,
                                              class=attributeclass,
                                              unit=attributeUnit,
                                              dateTimeFormatString=attributeDateTimeFormatString,
                                              missingValueCode=NA,
                                              missingValueCodeExplanation=rep("MissingValue",length(names(temp_level2)))
                                )
  #*write out the attribute table####
  write_tsv(x=Level2_attributeTable,file=paste("04_EDI/attributes_MissouriReservoirs_ProfileData_",Extract_year,".txt",sep=""))  
  } #End of loop through the different level 2 files 
  
  
#Read in state data
missouri_state<-map_data('state')%>%filter(region=="missouri")  
head(missouri_state)

#Plot state with state boundaries just for fun
#ggplot()+
#  geom_polygon(data=missouri_state,aes(x=long,y=lat,group=group),fill='white',color='black')

#find the bounding box of missouri
min(missouri_state$long) #west
max(missouri_state$long) #east
max(missouri_state$lat) #north
min(missouri_state$lat) #south


# Generate templates for dataset licensed under CCBY, with 3 tables.
template_core_metadata(path = "04_EDI/",
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

#Fill all csv files that start with 'attributes'####
attributeFiles<-str_subset(list.files("04_EDI/",pattern = "*.csv"),pattern="^attributes")
dataFiles<-str_subset(list.files("04_EDI/",pattern = "*.csv"),pattern="^Missouri")

#compile attribute template files - don't do this if they already exist####
template_table_attributes(path = "04_EDI/",
                         data.path = "04_EDI/",
                         data.table = dataFiles)
#Identify which variables are categorical####              
# template_categorical_variables(path = "04_EDI/",
#                                data.path = "04_EDI/",
#                                write.file = TRUE)

#Specify geographic coverage for all our data files####
#Not needed if geograph_coverage.txt is already existing
#template_geographic_coverage(path = "04_EDI/",
#                             data.path = "04_EDI/",
#                             data.table = dataFiles,
#                             empty = TRUE,
#                             write.file = TRUE)

################################
# Run this function
  #Make sure validation passes - address any issues here and rerun####
  #Notes on issues
    #make sure the keywords are tab delimited, have column headers of keyword and keywordThesaurus - some can come from the LTER controlled vocabulary: https://emily.lternet.edu/vocab/vocab/index.php
make_eml(path = "04_EDI/",
         dataset.title = "Missouri reservoir profile data including temperature, depth, and oxygen profiles (1989-current)", 
         data.path = "04_EDI/",
         eml.path = "04_EDI/",
         data.table  = dataFiles,
         data.table.name=sub(".csv$","",dataFiles),
         data.table.description = sub(".csv$","",dataFiles),
         temporal.coverage = c("1989-01-01", paste(max(as.numeric(sub("\\_.*", "", Level2_files)),na.rm=TRUE),"-12-31",sep="")), #gives the end date as the last year of the list 31Dec
         maintenance.description = "complete", 
         user.domain = "EDI",
         user.id = "richardsond",
         package.id='edi.1079.2'
         )

## Step 8: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to 
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked 
# for errors. If there are no errors, your data product is now published! 
# If there were errors, click the link to see what they were, then fix errors 
# in the xml file. 
# Note that each revision results in the xml file increasing one value 
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the 
# evaluation check again, until you receive a message with no errors.


