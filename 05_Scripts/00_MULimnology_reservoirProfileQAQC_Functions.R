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

#Function DOsat_function####
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
