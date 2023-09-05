##FUNCTIONS-Missouri Reservoir analysis####
##Created 13Jun2023 by David Richardson (hereafter DCR)
##Uses functions from Mohonk Lake analysis and others pulled from packages or created

##Functions include
#qaqc_bounds that cuts files based on sensor limits in the table####

#Libraries####
if (!require(rLakeAnalyzer)) {install.packages("rLakeAnalyzer")}
if (!require(purrr)) {install.packages("purrr")}
if (!require(insol)) {install.packages("insol")}

#Load packages####
library(rLakeAnalyzer) #for heatmaps and for water saturation
library(purrr)
library(insol)


#FUNCTION: qaqc_bounds####
#for qaqc to create NAs based on being outside the bounds from a data table####
qaqc_bounds<-function(variable,qaqc_table){
  
  qaqc_table2<-data.frame(qaqc_table) #make the sensor tibble into a data frame
  max<-as.numeric(qaqc_table2[qaqc_table2$Param==deparse(substitute(variable)),"maxBound"]) #pull the max for the variable
  min<-as.numeric(qaqc_table2[qaqc_table2$Param==deparse(substitute(variable)),"minBound"]) #pull the min for the variable
  
  variable2<-ifelse(variable>max|variable<min,NA,variable) #If the variable is outside the bounds then set it to NA
  
  return(variable2) #Return the variable 
}

#FUNCTION: DOsat_function####
#Proof of concept test: reading from sensor 2020, water_temp_degC=29.431, DO=8.38, DO_saturation=109.8
#From the calculation, DO saturation_mgpL=7.607389, saturation=110.2875
#calculate DO saturation from temperature - assume 0 salinity####
#Need elevation too - here the default is 110m, elevation for site 30
DOsat_function<-function(water_temp_degC,elevation_m=237){
  ##
  #Calculate average atmospheric pressure at elevation of lake
  #Using the 'barometric formula' from Wikipedia - should double check
  #Values of Rstar, g0, M are according to US Standard Atmosphere 1976; use ISO or SI instead?
  
  #If the elevation is NA, then use the average across all the sites (237m)
  elevation_m<-ifelse(is.na(elevation_m),237,elevation_m)
  #Constants
  Pb <- 101325        #static pressure, pascals
  Tb <- 288.15        #standard temp, K
  Lb <- -0.0065       #standard temp lapse rate, K m-1
  h <- elevation_m           #elevation above sea level, m
  hb <- 0             #elevation at bottom of atmospheric layer 0, m (note layer 0 extends to 11000 masl)
  Rstar <-  8.31432   #universal gas constant, N m mol-1 K-1 (equiv to J K-1 mol-1)  SI: 8.314472
  g0 <- 9.80665       #acceleration of gravity, m s-1
  M <- 0.0289644      #molar mass of Earth's air, kg mol-1
  
  #Pressure, in Pa (pascals)
  P <- Pb * (Tb/(Tb+Lb*(h-hb)))^(g0*M/(Rstar*Lb))
  # In mmHg
  atmPres <- P*0.00750061683
  
  
  ##
  #Calculate DO saturation
  #Use eqn from Weiss 1970 Deep Sea Res. 17:721-735; simplified since salinity=0
  # ln DO = A1 + A2 100/T + A3 ln T/100 + A4 T/100
  
  #Convert sensor Temp to Kelvin
  Temp_K <- water_temp_degC + 273.15
  
  #Weiss equation
  A1 <- -173.4292;  A2 <- 249.6339;  A3 <- 143.3483;  A4 <- -21.8492
  DOSat <- exp(((A1 + (A2*100/Temp_K) + A3*log(Temp_K/100) + A4*(Temp_K/100))))
  
  #Correction for local average atmospheric pressure
  u <- 10^(8.10765 - (1750.286/(235+Temp_K)))
  DOSat <- (DOSat*((atmPres-u)/(760-u)))   #ml/L
  DOSat <- DOSat/1000                      #L/L
  
  #Convert using standard temperature and pressure. 
  #Similar to calculating saturation DO at STP in ml/L, converting to mg?L (at STP),
  #and then doing the above temperature and pressure conversions.
  R <- 0.082057  #L atm deg-1 mol-1
  O2molWt <- 15.999*2
  convFactor <- O2molWt*(1/R)*(1/273.15)*(760/760) #g/L
  DOSat <- DOSat*convFactor*1000                   #mg/L
  
}


#Calculate water density from temperature ignoring conductivity####
#https://hess.copernicus.org/preprints/hess-2016-36/hess-2016-36-manuscript-version3.pdf
#https://agupubs.onlinelibrary.wiley.com/doi/pdf/10.1029/2006RG000210
WaterDensity_function<-function(water_temp_degC){
  density_kgpm3<-1000 * (1 - ((water_temp_degC + 288.9414) / (508929.2 * (water_temp_degC + 68.12963))) *
            (water_temp_degC - 3.9863) ^ 2)
  
  return(density_kgpm3)
}

#FUNCTION: waterDensity_function_vectorize####
#create a vectorized version
WaterDensity_function_vectorize<-Vectorize(WaterDensity_function)

#FUNCTION: flag_jumps####
#Create function to check if the column has any jumps up or down####
flag_jumps<-function(data,scalar=2){
    #set the factor increase here (2=100% increase; 50% decrease)
  if(sum(!is.na(data))>0&sum(data<=0,na.rm=TRUE)>0){data<-data-(1.1*min(data,na.rm=TRUE))}else{} #if there are any negative numbers, make the minimum 0+10% and scale all others up based on that number
flag_column<-(data>scalar*lag(data,1)|data<(1/scalar)*lag(data,1)) #finds anything that increases by a scalar or decreases by 1/scalar
flag_column[1]<-FALSE #makes the first NA value into FALSE
flag_column[is.na(flag_column)] <- FALSE #convert any NA into FALSE 
return(flag_column)
}

#Find max sequence function####
#This function finds the lowest number in the sequence of numbers adjacent to the max
maxN<-function(vector){
  #if vector has no numbers, do nothing
  if(identical(rows_with_jumps, integer(0))){}else{
    vector_sort<-vector[order(vector,decreasing = TRUE)] #sort vector from largest to smallest
    vector_diff<-c(-1,diff(vector_sort))
    vector_diff_isnegOne<-vector_diff==-1
      index<-1
    while(index<=length(vector_sort)){
      if(vector_diff_isnegOne[index]==TRUE){index<-index+1}else{break}
    }
    return(vector_sort[index-1])
    
  }
}

#####Function: ThermoclineDepth#################
#THERMOCLINE DEPTH
#Calculates the thermocline depth based on a threshold for density (usually 0.3), temp profile (C) and depth profile (m)
#returns thermocline depth in m
thermocline.Depth <- function(depth.array, temp.array, thresh) {
  #Calculate density array
  Density <-
    1000 * (1 - (temp.array + 288.9414) / (508929.2 * (temp.array + 68.12963)) *
              (temp.array - 3.9863) ^ 2)
  
  #drho_dz is the change in density over depth
  numDepths = length(depth.array)
  dDensdz = rep(NA, numDepths - 1)
  for (i in 1:(numDepths - 1)) {
    dDensdz[i] = (Density[i + 1] - Density[i]) / (depth.array[i + 1] - depth.array[i])
    #end of for loop to calculating dDensitydz
  }
  
  dDensdz.unlist <- unlist(dDensdz)
  #if the maximum is NA, then return NA (all the values are NA)
  #else if the maximum is less than the threshold, then it is not stratified and return NA
  #Else find the maximum density difference to establish the thermocline depth
  if (max(dDensdz.unlist, na.rm = TRUE) == -Inf) {
    metaDepth <- NA
  } else if (max(dDensdz.unlist, na.rm = TRUE) < thresh) {
    metaDepth <- NA
    #Old code returns max depth; this might be helpful to know you had reading but not stratified
    #metaDepth<-max(depth.array,na.rm=TRUE)
  } else{
    maxBigChange <- which.max(dDensdz.unlist)
    metaDepth <-
      mean(c(depth.array[maxBigChange], depth.array[(maxBigChange + 1)]))
  }
  #return the max depth
  return(metaDepth)
}
####End of Thermocline function#

#####Function: minDO#################
#Gives the minimum DO - if the entire profile is NA, returns NA
minDO<-function(do_vector){
  if(sum(is.na(do_vector))==length(do_vector)|sum(!is.na(do_vector))<=3){return(NA)}else{
    return(min(do_vector,na.rm=TRUE))
  }
}
####End of minDO function#

#####Function: maxDO#################
#Gives the minimum DO - if the entire profile is NA, returns NA
maxDO<-function(do_vector){
  if(sum(is.na(do_vector))==length(do_vector)|sum(!is.na(do_vector))<=3){return(NA)}else{
    return(max(do_vector,na.rm=TRUE))
  }
}
####End of maxDO function#

#####Function: depthDOmax#################
#Gives the depth of the maximum DO if fed a DO vector and depth vector
#returns the first one in the event of ties, or NA in the event of less than 3 do values or all NA depths
depthDOmax<-function(depth_vector,do_vector){
  if(sum(is.na(depth_vector))==length(depth_vector)|sum(!is.na(do_vector))<3){return(NA) #check if the depth or do vector is all NA, then return NA
    }else{
    temp_vectorOfDepths<-depth_vector[do_vector==max(do_vector,na.rm=TRUE)]
    return(temp_vectorOfDepths[1])  
  }
}
####End of depthDOmax function#

#Thermocline depth function - max density####
#Calculates the thermocline depth based on max density rate of change, temp profile (C) and depth profile (m)
thermocline.Depth.max <- function(depth.array, temp.array) {
  #Calculate density array
  Density <-
    1000 * (1 - (temp.array + 288.9414) / (508929.2 * (temp.array + 68.12963)) *
              (temp.array - 3.9863) ^ 2)
  
  #Calculate the differences of depth
  dz <- diff(depth.array)
  #Calculate the differences of density
  dDens <-
    as.numeric(Density[(2):length(Density)] - Density[1:length(Density) - 1])
  #First derivative of the density profile
  dDensdz <- dDens / dz
  
  #Find the max rate of change
  maxBigChange <- which.max(dDensdz)
  metaDepth <-
    mean(c(depth.array[maxBigChange], depth.array[(maxBigChange + 1)]))
  
  return(metaDepth)
  #End of thermocline depth max density
}
####End of Thermocline function#

#Function Oxycline_threshold####
#Finds the first depth that is = or below a threshold
#depth.array=c(0,0.7,1,1.5,2)
#DO.array=c(10.3,7.7,6,3.3,0.01)
#DO.array=c(8.5,8.2,7.8,7.6,7,2.2,2.2,2.2,2.2)
#DO.array=c(8.5,NA,8.2,1,5,0.5)
Oxycline_threshold<-function(depth.array,DO.array,threshold=2){
  #Find the first depth reading that satisfied the threshold, returns NA if there are no DO values or if there is no value below that threshold####
  if(sum(is.na(DO.array))==length(DO.array)|sum(DO.array<=threshold,na.rm=TRUE)==0){
    Oxycline<-NA
  }else{
    Oxycline<-depth.array[min(which(DO.array<=threshold))]  
  }
  return(Oxycline)
}
####End of Oxycline_threshold function#

#Function densityGradientAcrossMetalimnion####
#find the water density at the top of the metalimn, the water density at the bottom from the closest measurements
#finds the depths for those measurements
#Does the difference (bottom density-top density)/(depth_bottom-depth_top####
densityGradientAcrossMetalimnion<-function(Depth_vector,Temp_vector,meta_top,meta_bottom){
  if(sum(is.na(Temp_vector))==length(Temp_vector)|sum(is.na(Depth_vector))==length(Depth_vector)|is.na(meta_top)|is.na(meta_bottom)|meta_top==meta_bottom){return(NA)}else{ #checks if there are NAs in any of the measurements and returns NA if so
    #Find the closest depth to the top
    top_depth<-Depth_vector[abs(Depth_vector - meta_top) == min(abs(Depth_vector - meta_top))]
    #Find the water density of that measurement closest to metalimnion top
    top_density<-water.density(Temp_vector[abs(Depth_vector - meta_top) == min(abs(Depth_vector - meta_top))])
    #Find the closest depth to the top
    bottom_depth<-Depth_vector[abs(Depth_vector - meta_bottom) == min(abs(Depth_vector - meta_bottom))]
    #Find the water density of that measurement closest to metalimnion top
    bottom_density<-water.density(Temp_vector[abs(Depth_vector - meta_bottom) == min(abs(Depth_vector - meta_bottom))])
    #calculate the density gradient in kg m-3 m-1#
    delta_dens<-(bottom_density-top_density)
    #Calculate the distance
    delta_depth<-(bottom_depth-top_depth)
    #Calculate the density gradient
    return(delta_dens/delta_depth)
    
  }
  }
####End of densityGradientAcrossMetalimnion####

#Function densityGradientEpiToHypo####
#find the water density at the top of the metalimn, the water density at the bottom from the closest measurements
#finds the depths for those measurements
#Does the difference (bottom density-top density)/(depth_bottom-depth_top####
densityGradientAcrossEpiToHypo<-function(maxDepth_m,epilimnion_temp_degC,hypolimnion_temp_degC,meta_top,meta_bottom){
  if(is.na(epilimnion_temp_degC)|is.na(hypolimnion_temp_degC)|is.na(maxDepth_m)|is.na(meta_top)|is.na(meta_bottom)){return(NA)}else{ #checks if there are NAs in any of the measurements and returns NA if so
    #Find the midpoint of the epilimnion
    epi_depth<-(meta_top+0)/2
    #Find the water density of the average epi temp
    epi_density<-water.density(epilimnion_temp_degC)
    #Find the midpoint of the epilimnion
    hypo_depth<-(meta_bottom+maxDepth_m)/2
    #Find the water density of the average hypo temp
    hypo_density<-water.density(hypolimnion_temp_degC)
    #calculate the density gradient in kg m-3 m-1#
    delta_dens<-(hypo_density-epi_density)
    #Calculate the distance
    delta_depth<-(hypo_depth-epi_depth)
    #Calculate the density gradient
    return(delta_dens/delta_depth)
    
  }
}
####End of densityGradientAcrossMetalimnion####


#Function pigmentRatios####
#ratios of BGA to chl at max depth#
pigmentRatios<-function(BGA_vector,CHL_vector,Depth_vector){
  depthChlMax_m<-depthDOmax(depth_vector=Depth_vector,do_vector=CHL_vector)
  depthBGMax_m=depthDOmax(depth_vector=Depth_vector,do_vector=BGA_vector)
  if(is.na(depthChlMax_m)|is.na(depthBGMax_m)){ratio<-NA}else{ratio<-BGA_vector[Depth_vector==depthBGMax_m]/CHL_vector[Depth_vector==depthChlMax_m]}
  return(ratio)
}
####End of pigmentRatios####


