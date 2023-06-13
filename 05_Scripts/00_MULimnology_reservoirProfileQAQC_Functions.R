##FUNCTIONS-Missouri Reservoir analysis####
##Created 13Jun2023 by David Richardson (hereafter DCR)
##Uses functions from Mohonk Lake analysis and others pulled from packages or created

##Functions include
#qaqc_bounds that cuts files based on sensor limits in the table####

#Libraries####

#Load packages####


#Function for qaqc to create NAs based on being outside the bounds from a data table####
qaqc_bounds<-function(variable,qaqc_table){
  
  qaqc_table2<-data.frame(qaqc_table) #make the sensor tibble into a data frame
  max<-as.numeric(qaqc_table2[qaqc_table2$Param==deparse(substitute(variable)),"maxBound"]) #pull the max for the variable
  min<-as.numeric(qaqc_table2[qaqc_table2$Param==deparse(substitute(variable)),"minBound"]) #pull the min for the variable
  
  variable2<-ifelse(variable>max|variable<min,NA,variable) #If the variable is outside the bounds then set it to NA
  
  return(variable2) #Return the variable 
}