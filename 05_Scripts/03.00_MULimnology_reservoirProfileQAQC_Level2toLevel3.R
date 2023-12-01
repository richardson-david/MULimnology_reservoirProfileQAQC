##Level3: Summarizes individual profiles Missouri Reservoirs####
##Created 02Aug2023 by David Richardson (hereafter DCR)
##Reads in level 2 data from GIT, exports as a single file with each profile summarized into a row####

#Level2 to 3 outline:####
#Read in all the level 2 files for a year
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
if(!require(patchwork)){install.packages("patchwork")}

#Load packages
library(tidyverse)
library(patchwork) #laying out multipanel plots with the same size

#Run functions script to upload all the user defined functions####
source("05_Scripts/00_MULimnology_reservoirProfileQAQC_Functions.R")

#Read in level 2 files from a particular year####
#Set years here, update each year here####
yearIndex<-"2023"
  #possible years: c("Historical","2017","2018","2019","2020","2021","2022","2023")

#*Set the directory path here####
dirPath<-paste0("02_Level2_Data/")


#Stack all the profiles together####
profiles2<-read_csv(file=paste0(dirPath,"/",yearIndex,"_Level2.csv"), col_types = cols()) #last argument suppresses the message on input about column types, helpful for mass upload
  
  #Check how many profiles exist
    #merge MULakeNumber and dateTime
    length(unique(paste0(profiles2$MULakeNumber,"_",profiles2$date)))
    #8266 profiles for historical
    #Ok - this gets the same number of 8266
    profiles2%>%
    group_by(MULakeNumber,date)%>%summarize(count=n())
  
#Stack all the logs together####
logs2<-read_csv(file=paste0("06_Outputs/",yearIndex,"_QAQC_log.csv"), col_types = cols())

#Check for unqiue numbers in both the profiles and logs####
#The profiles should be more accurate because the logs include profiles that have been removed####
#This no longer works since we are doing it one at a time. Recreate old code - loop through, store in a list and then do.call to merge together to get all site names####
  #sort(unique(profiles2$MULakeNumber))
  #sort(unique(logs2$MULakeNumber))
#Write out that list of MULake numbers####
  #write_csv(tibble(MULakeNumber=sort(unique(profiles2$MULakeNumber))),file=paste0("06_Outputs/ListOfMULakeNumbers_through2022.csv"))

#Summary calculations - summarize by MULakeNumber and date, depth_m is profile depth, temp_degC is the temperature
  #Temperature/Stratification metrics metrics####
  #Oxygen metrics
  #Biological metrics
  #Some data for export for reports

summary3<-profiles2%>%
  group_by(MULakeNumber,date)%>%
  summarize(
    minDepth_m=min(depth_m,na.rm=TRUE), #smallest depth
    maxDepth_m=max(depth_m,na.rm=TRUE), #deepest depth
    numberOfMeasurements_temperature=sum(!is.na(temp_degC)), #number of temperature measurements
    thermoclineDepth_m_thresh0.3=thermocline.Depth(depth.array=depth_m,temp.array=temp_degC,thresh = 0.3), #thermocline depth at 0.3 density threshold
    top_metalimnion_m=meta.depths(wtr=temp_degC,depths=depth_m)[1], #Top of metalimnion using rLakeAnalyzer
    bottom_metalimnion_m=meta.depths(wtr=temp_degC,depths=depth_m)[2], #Bottom of metalimnion using rLakeAnalyzer
    epilimnion_temp_degC=mean(temp_degC[depth_m<=top_metalimnion_m],na.rm=TRUE), #average temperature above metalimnion top
    hypolimnion_temp_degC=mean(temp_degC[depth_m>=bottom_metalimnion_m],na.rm=TRUE), #average temperature below the metalimnion bottom
    above_thermocline_temp_degC=mean(temp_degC[depth_m<=thermoclineDepth_m_thresh0.3],na.rm=TRUE), #average temperature above or equal to the thermocline
    below_thermocline_temp_degC=mean(temp_degC[depth_m>thermoclineDepth_m_thresh0.3],na.rm=TRUE), #average temperature below the thermocline
    delta_hypo_epi_waterDensity_kgperm3=water.density(hypolimnion_temp_degC)-water.density(epilimnion_temp_degC), #density difference between hypolimnion water and epilimnion water, bigger difference is stronger stratification
    metalimnionDensityGradient_kgperm3perm=densityGradientAcrossMetalimnion(Depth_vector=depth_m,Temp_vector = temp_degC,meta_top =top_metalimnion_m,meta_bottom = bottom_metalimnion_m), #Function that calculates the water density gradient from the temperate at the top of the metalimnion relative to the bottom correcting for distance between those two measurements######
    epiToHypoDensityGradient_kgperm3perm=densityGradientAcrossEpiToHypo(maxDepth_m=maxDepth_m,epilimnion_temp_degC=epilimnion_temp_degC,hypolimnion_temp_degC=hypolimnion_temp_degC,meta_top =top_metalimnion_m,meta_bottom = bottom_metalimnion_m), #Function that calculates the water density gradient from average epilimnion temperature relative to the bottom, correcting for the distance between the midpoints of the two layers######
    buoyancyfrequency_1_s2=max(buoyancy.freq(wtr=temp_degC,depths=depth_m),na.rm=TRUE), #generate the maximum buoyancy frequency using rLakeAnalyzer buoyancy frequency vector
    numberOfMeasurements_do=sum(!is.na(doConcentration_mgpL)), #number of depth measurements
    minDO_mgpL=minDO(doConcentration_mgpL), #lowest DO concentration
    maxDO_mgpL=maxDO(doConcentration_mgpL), #highest DO concentration
    minDO_percent=minDO(doSaturation_percent), #lowest DO percentage
    maxDO_percent=maxDO(doSaturation_percent), #highest DO percentage
    DO_mgpL_profileMean=mean(doConcentration_mgpL,na.rm=TRUE), #overall average DO mgpL
    DO_percent_profileMean=mean(doSaturation_percent,na.rm=TRUE), #overall average DO percentage
    epilimnion_DO_mgpL=mean(doConcentration_mgpL[depth_m<=top_metalimnion_m],na.rm=TRUE), #average temperature above metalimnion top
    hypolimnion_DO_mgpL=mean(doConcentration_mgpL[depth_m>=bottom_metalimnion_m],na.rm=TRUE), #average temperature below the metalimnion bottom
    epilimnion_DO_percent=mean(doSaturation_percent[depth_m<=top_metalimnion_m],na.rm=TRUE), #average temperature above metalimnion top
    hypolimnion_DO_percent=mean(doSaturation_percent[depth_m>=bottom_metalimnion_m],na.rm=TRUE), #average temperature below the metalimnion bottom
    depthMaxDOpercentage_m=depthDOmax(depth_vector=depth_m,do_vector=doSaturation_percent), #calculates the depth of the maximum DO
    Oxycline_m=thermocline.Depth.max(depth.array=depth_m,temp.array=doConcentration_mgpL), #Find where the fastest rate of DO change is
    Hypoxycline_m=Oxycline_threshold(depth.array=depth_m,DO.array=doConcentration_mgpL,threshold=2), #find the first depth where DO is less than or equal to the threshold of 2, NA means no DO values (all NAs) OR no values below threshold
    Anoxycline_m=Oxycline_threshold(depth.array=depth_m,DO.array=doConcentration_mgpL,threshold=1), #find the first depth where DO is less than or equal to the threshold of 1, NA means no DO values (all NAs) OR no values below threshold
    depthChlMax_m=depthDOmax(depth_vector=depth_m,do_vector=chlorophyll_RFU), #depth of the chlorophyll maximum
    depthBGAMax_m=depthDOmax(depth_vector=depth_m,do_vector=phycocyaninBGA_RFU), #depth of the phycocyanin BGA maximum
    ratioMaxBGtochl_RFUperRFU=pigmentRatios(phycocyaninBGA_RFU,chlorophyll_RFU,depth_m), #the ratio of the BG value at its max to the chl value at its max
    epilimnion_chlorophyll_RFU=mean(chlorophyll_RFU[depth_m<=top_metalimnion_m],na.rm=TRUE), #average chlorophyll above the top of the metalimnion
    epilimnion_phycocyaninBGA_RFU=mean(phycocyaninBGA_RFU[depth_m<=top_metalimnion_m],na.rm=TRUE), #average chlorophyll above the top of the metalimnion
    ratioEpiBGtochl_RFUperRFU=epilimnion_phycocyaninBGA_RFU/epilimnion_chlorophyll_RFU, #Ratio of BGA to chl from the average of each above the top of the metalimnion,be warned, sometimes this is negative because one or the other is negative
    hypolimnion_orp_mV=mean(orp_mV[depth_m>=bottom_metalimnion_m],na.rm=TRUE), #average ORP below the metalimnion bottom
    bottom0.5m_orp_mV=mean(orp_mV[depth_m>=(maxDepth_m-0.5)],na.rm=TRUE), #average ORP in the bottom 0.5 m
    site_latitude=mean(latitude,na.rm=TRUE), #average latitude from handheld
    site_longitude=mean(longitude,na.rm=TRUE), #average longitude from handheld
    site_altitude_m=mean(altitude_m,na.rm=TRUE), #average elevation from handheld
    surface_doConcentration_mgpL=mean(doConcentration_mgpL[depth_m<=0.5],na.rm=TRUE), #average DO concentration in the top 0.5 m
    surface_temp_degC=mean(temp_degC[depth_m<=0.5],na.rm=TRUE), #average temperature in the top 0.5 m
    surface_pH=log10(mean(10^pH[depth_m<=0.5],na.rm=TRUE)), #average pH in the top 0.5 m, note, the average is taken by backtransforming first.
    ) #end of summarize
    
    #Convert any Inf or -Inf into NA across all numeric columns####
    summary3<-summary3 %>%
      mutate(across(where(is.numeric), ~na_if(., -Inf)))%>%
      mutate(across(where(is.numeric), ~na_if(., Inf)))%>%
      mutate(buoyancyfrequency_1_s2=ifelse(numberOfMeasurements_temperature<4,NA,buoyancyfrequency_1_s2)) #Remove buoyancy frequencies if there is less than 4 temperature measurements
      
#Export the level3 summary file####
write_csv(summary3,file=paste0("03_Level3_Data/",yearIndex,"_Level3.csv"))

#Export as a page per####
pdf(paste0("06_Outputs/Level3_QAQC_plots_",yearIndex,".pdf"), onefile = TRUE,width=8.5,height=11)
    
#tick through the profiles and graph them in a 4 panel graph####    
#profile.index<-1
for(profile.index in 1:nrow(summary3)){
  temp_summary<-summary3[profile.index,]
  temp_profile<-profiles2%>%filter(MULakeNumber==temp_summary$MULakeNumber&date==temp_summary$date)
  #GG1: Plot of temp vs. depth####
  gg1<-ggplot(temp_profile%>%arrange(depth_m)%>%drop_na(temp_degC),aes(y=depth_m,x=temp_degC))+geom_point()+geom_path()+scale_y_reverse()+
    theme_bw()+
    labs(x=bquote(Temp~(degree*C)),y="Depth (m)")+
    geom_hline(yintercept=c(temp_summary$top_metalimnion_m,temp_summary$thermoclineDepth_m_thresh0.3,temp_summary$bottom_metalimnion_m),color=c("red","purple","blue"))+
    geom_segment(data=temp_summary, aes(x=above_thermocline_temp_degC,xend=above_thermocline_temp_degC,y=thermoclineDepth_m_thresh0.3,yend=minDepth_m),color="purple")+
    geom_segment(data=temp_summary, aes(x=below_thermocline_temp_degC,xend=below_thermocline_temp_degC,y=thermoclineDepth_m_thresh0.3,yend=maxDepth_m),color="purple")+
    geom_segment(data=temp_summary, aes(x=epilimnion_temp_degC,xend=epilimnion_temp_degC,y=top_metalimnion_m,yend=minDepth_m),color="red")+
    geom_segment(data=temp_summary, aes(x=hypolimnion_temp_degC,xend=hypolimnion_temp_degC,y=bottom_metalimnion_m,yend=maxDepth_m),color="blue")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    geom_text(aes(x=-Inf,y=-Inf,hjust=-0.2,vjust=1.3,label=paste0(temp_summary$MULakeNumber,"-",temp_summary$date)))
  
  #GG2: Plot of DO vs. depth####
  gg2<-ggplot(temp_profile%>%arrange(depth_m)%>%drop_na(doConcentration_mgpL),aes(y=depth_m,x=doConcentration_mgpL))+geom_point()+geom_path()+scale_y_reverse()+ #geom_path to connect in order that data appear in data frame
    theme_bw()+
    labs(x=bquote(DO~(mg/L)),y="Depth (m)")+
    geom_hline(yintercept=c(temp_summary$top_metalimnion_m,temp_summary$thermoclineDepth_m_thresh0.3,temp_summary$bottom_metalimnion_m),color=c("red","purple","blue"))+
    geom_segment(data=temp_summary, aes(x=epilimnion_DO_mgpL,xend=epilimnion_DO_mgpL,y=top_metalimnion_m,yend=minDepth_m),color="red")+
    geom_segment(data=temp_summary, aes(x=hypolimnion_DO_mgpL,xend=hypolimnion_DO_mgpL,y=bottom_metalimnion_m,yend=maxDepth_m),color="blue")+
    geom_hline(yintercept=c(temp_summary$Oxycline_m,temp_summary$Hypoxycline_m,temp_summary$Anoxycline_m),color=c("light grey","grey","dark grey"))+
    geom_hline(yintercept=c(temp_summary$depthMaxDOpercentage_m),color=c("green"))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
    
  #GG3: Plot of chl/bga vs. depth####
  gg3<-ggplot(temp_profile%>%arrange(depth_m)%>%drop_na(chlorophyll_RFU,phycocyaninBGA_RFU),aes(y=depth_m,x=chlorophyll_RFU))+geom_point(color="seagreen3")+geom_path(color="seagreen3")+scale_y_reverse()+ #geom_path to connect in order that data appear in data frame
    theme_bw()+
    labs(x=bquote(Photopigment~(RFU)),y="Depth (m)")+
    geom_path(aes(x=phycocyaninBGA_RFU),color="turquoise")+
    geom_point(aes(x=phycocyaninBGA_RFU),color="turquoise")+
    geom_hline(yintercept=c(temp_summary$top_metalimnion_m,temp_summary$thermoclineDepth_m_thresh0.3,temp_summary$bottom_metalimnion_m),color=c("red","purple","blue"))+
    geom_hline(yintercept=c(temp_summary$depthMaxDOpercentage_m,temp_summary$depthChlMax_m,temp_summary$depthBGAMax_m),color=c("green","seagreen3","turquoise"),size=c(0.5,1.5,1.5))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
    
  #GG4: Plot of other potentially interesting things vs. depth####
  gg4<-ggplot(temp_profile%>%arrange(depth_m)%>%drop_na(turbidity_FNU,pH),aes(y=depth_m,x=turbidity_FNU))+geom_point(color="tan2")+geom_path(color="tan2")+scale_y_reverse()+ #geom_path to connect in order that data appear in data frame
    theme_bw()+
    labs(x=bquote(Turbidity~(RFU)~or~pH),y="Depth (m)")+
    geom_path(aes(x=pH),color="black")+
    geom_point(aes(x=pH),color="black")+
    geom_hline(yintercept=c(temp_summary$top_metalimnion_m,temp_summary$thermoclineDepth_m_thresh0.3,temp_summary$bottom_metalimnion_m),color=c("red","purple","blue"))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  List<-list(gg1,gg2,gg3,gg4)
  
  #Plot them using patchwork####
  gg.4panel<-wrap_plots(List,ncol = 2,nrow = 2)
  print(gg.4panel)
  ####STOPPED HERE: CONSTRUCT a 4 panel version, then print on multiple page pdf for each year####
}    
    
dev.off()    
