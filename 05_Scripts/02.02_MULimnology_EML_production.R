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


# Import templates for dataset licensed under CCBY, with 3 tables.
template_core_metadata(path = "EDI/",
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = "EDI/",
                          data.path = "EDI/",
                          data.table = c("pond_data.csv",
                                         "pond_latlon.csv",
                                         "pond_definitions.csv",
                                         "state_definitions.csv"))
              
template_categorical_variables(path = "EDI/",
                               data.path = "EDI/",
                               write.file = TRUE)

template_geographic_coverage(path = "EDI/",
                             data.path = "EDI/",
                             data.table = c("pond_data.csv",
                                            "pond_latlon.csv",
                                            "pond_definitions.csv",
                                            "state_definitions.csv"),
                             empty = TRUE,
                             write.file = TRUE)

################################
# Run this function
make_eml(path = "EDI/",
         dataset.title = "Pond data: physical, chemical, and biological characteristics with scientific and United States of America state definitions from literature and legislative surveys", 
         data.path = "EDI/",
         eml.path = "EDI/",
         data.table  = c("pond_data.csv",
                         "pond_latlon.csv",
                         "pond_definitions.csv",
                         "state_definitions.csv"),
         data.table.name=c("Pond data", 
                           "Pond latitudes and longitudes",
                           "Pond definitions",
                           "State definitions"),
         data.table.description = c("Pond data for single ponds",
                                    "Pond latitude and longitude for single ponds",
                                    "Pond definitions from scientific literature",
                                    "Aquatic waterbody definitions from U.S. states"),
         temporal.coverage = c("1946-01-01", "2019-04-30"),
         maintenance.description = "complete", 
         user.domain = "EDI",
         user.id = "richardsond",
         package.id='edi.1014.2'
         )

