# Steps for setting up EML metadata ####
if(!require(EMLassemblyline)){install.packages("EMLassemblyline")}
if(!require(devtools)){install.packages("devtools")}

library(devtools)
#install_github("EDIorg/EMLassemblyline", force=T)

# Install from GitHub
remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

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

