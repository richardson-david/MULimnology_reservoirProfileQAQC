##Visualizations for Slap final report for Missouri Reservoirs####
##Created 21Mar2024 by David Richardson (hereafter DCR)
##Creates figures and maps for individual lake reports####

#Libraries
if (!require(patchwork)) {install.packages("patchwork")}

library(tidyverse)
library(patchwork) #for multipanel plots
library(readxl) #read in excel files, need to load readxl explicity because it is not a core tidyverse package

#Read in the functions####
source("05_Scripts/00_MULimnology_reservoirProfileQAQC_Functions.R")

#establish the year here###
year<-2023
#Read in the 2023 database here####
waterChemDF<-read_csv(paste0("06_Outputs/",year,"_MissouriReservoirsForDNR_v1.csv"))

#Read in the historical data for the selected private lakes####
privateLakesHistoric<-read_excel(paste0("08_ParameterMasterSheets/",year,"_ParameterMasterSheets","/2023 SLAP private lakes reports.xlsx"),sheet="2023 SLAP private lakes reports")%>%
  mutate(Date=as.Date(beginDateTime),
         endDepth_char="SURF",
         MULakeNumber=as.character(MULakeNumber))%>%
  #filter(beginDepth==0&endDepth==0)%>% #Only pull the surface samples
  rename(endDepth_m=endDepth
  )%>%
  dplyr::select(MULakeNumber,waterBody,Date,parameterType,endDepth_char,unit,parameterValue,endDepth_m,samplingSiteLatitude,samplingSiteLongitude)%>%
  #Convert units to match the data from this year####
mutate(parameterValue=case_when(
  parameterType=="CHL_A_COR"~parameterValue*1000,
  parameterType=="CHL_A_lab"~parameterValue*1000,
  parameterType=="NH4"~parameterValue*1000,
  parameterType=="NO3"~parameterValue*1000,
  parameterType=="PHEO"~parameterValue*1000,
  parameterType=="TDN"~parameterValue*1000,
  parameterType=="TN"~parameterValue*1000,
  parameterType=="TP"~parameterValue*1000,
  .default = parameterValue
))%>%
  #Change the unit labels####
mutate(unit=case_when(
  parameterType=="CHL_A_COR"~"ug/L",
  parameterType=="CHL_A_lab"~"ug/L",
  parameterType=="NH4"~"ug/L",
  parameterType=="NO3"~"ug/L",
  parameterType=="PHEO"~"ug PHEO/L",
  parameterType=="TDN"~"ug/L",
  parameterType=="TN"~"ug/L",
  parameterType=="TP"~"ug/L",
  parameterType=="SECCHI"~"m",
  parameterType=="PH_field"~"unitless",
  parameterType=="PH_lab"~"unitless",
  parameterType=="TEMP"~"C",
  .default = unit
))%>%
  #Change the parameter types####
mutate(parameterType=case_when(
  parameterType=="PH_field"~"pH",
  parameterType=="TEMP"~"Temp", #This should probably be fixed in 2023 data
  .default = parameterType
))


#Data munging the data frame####
waterChemDF_summary<-waterChemDF%>%
  filter(!(MULakeNumber=="401"))%>% #remove 401 (field blanks)
  filter(nchar(MULakeNumber)<=3)%>% #remove all field dupes and other sites
  group_by(MULakeNumber,Date,parameterType,endDepth_char,unit,waterBody)%>%
  summarize(parameterValue=mean(parameterValue,na.rm=TRUE),  
            endDepth_m=mean(endDepth_m,na.rm=TRUE),
            samplingSiteLatitude=mean(samplingSiteLatitude,na.rm=TRUE),
            samplingSiteLongitude=mean(samplingSiteLongitude,na.rm=TRUE)
  )


#Merge the two together via bind_rows####
#Merge the historical data with the 2023 data####
privateLakes_merged<-bind_rows(privateLakesHistoric,waterChemDF_summary%>%filter(MULakeNumber%in%unique(privateLakesHistoric$MULakeNumber)))

#Take the annual average and SE of that average####
privateLakes_merged_annual<-
  privateLakes_merged%>%
  ungroup()%>%
  mutate(year=year(Date))%>%
  group_by(year,MULakeNumber,waterBody,parameterType,unit)%>%
  dplyr::summarise(parameterValue_mean=mean(parameterValue,na.rm=TRUE),
                   parameterValue_sderr=sderr(parameterValue),
                   .groups="keep")

#Plot all the different variables over time for a specific lake####
#Lakes for individual reports: "18"  "21"  "85"  "45"  "11"  "112" "14"  "120"
#ggplot(data=privateLakes_merged_annual%>%filter(MULakeNumber=="18"),aes(x=year,y=parameterValue_mean))+geom_point()+geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+facet_wrap(~parameterType,scales="free_y")

#Find the sen slopes for these variables: CHL_A_COR, PIM, POM, SECCHI, TN, TP, TSS
#group by MULakeNumber, parameterType, unit
slopes<-privateLakes_merged_annual%>%
  filter(parameterType%in%c("CHL_A_COR", "PIM", "POM", "SECCHI", "TN", "TP", "TSS"))%>% #pull out these 7 variables of interest
  ungroup()%>%
  group_by(MULakeNumber,parameterType,unit)%>%
  summarize(sensSlope_slope=MTCC.sensSlope(year,parameterValue_mean)$coefficients["Year"],
            sensSlope_intercept=MTCC.sensSlope(year,parameterValue_mean)$coefficients["Intercept"],
            sensSlope_pval=MTCC.sensSlope(year,parameterValue_mean)$pval,
            sensSlope_df=n()-2)%>%
  mutate(significance=ifelse(sensSlope_pval<0.05,"*",""))%>%print(n=Inf)

#Export the table for the Sens Slope stats####
write_csv(slopes,file=paste0("06_Outputs/",year,"_MissouriIndividualReports_slopes.csv"))

#Join back in the slopes by MULakeNumber, parameterType, unit####
privateLakes_merged_annual_slopes<-left_join(privateLakes_merged_annual,slopes,by=c("MULakeNumber","parameterType","unit"))%>%
  mutate(parameterValue_mean_fit=ifelse(significance=="*",year*sensSlope_slope+sensSlope_intercept,NA))

#for each lake - do this set of 4 figures: TP, TN, CHl, Secchi
#Specify the lakes for the individual reports here###
#Might have to go further up in the future to subset the lakes from the overall dataframe####
individual.lakes<-c("18",  "21",  "85",  "45",  "11",  "112", "14",  "120")

#Loop through each of these individual lakes and create the two figures for them####
for(lake.index in 1:length(individual.lakes)){

  lake.id<-individual.lakes[lake.index] #specify the lake
  
#Desired plot width in inches
plot.width<-6
plot.height<-4.75
scale_to_mg<-(1/1000) #This scales everything to mg/L for TP and TN. Change labels and change this factor to 1 for ug/L

#TN specific graphs####
labels_TN<-tibble(x=rep(-Inf,4),y.line=c(350,550,1200,NA),y.label=c((350/2),450,(550+1200)/2,1250),y.label.text=c("0","M","E","HE"))
y.max_TN<-max(1255,max(privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="TN")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE),na.rm=TRUE)
(gg.TN.lake<-ggplot(data=privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="TN"),aes(x=year,y=parameterValue_mean*scale_to_mg))+
    geom_hline(yintercept=labels_TN$y.line*scale_to_mg,linetype=2,color="grey")+
    geom_text(data=labels_TN,aes(x=x,y=y.label*scale_to_mg,label=y.label.text),hjust=-0.2,color="grey")+
    geom_errorbar(aes(ymin = (parameterValue_mean-parameterValue_sderr)*scale_to_mg, ymax = (parameterValue_mean+parameterValue_sderr)*scale_to_mg))+
    geom_point(size=2,shape=21,fill="light grey")+
    geom_line(aes(x=year,y=parameterValue_mean_fit*scale_to_mg),color="blue")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(TN~(mg*'/'*L)))+
    scale_y_continuous(limits=c(0,y.max_TN*scale_to_mg)))

#TP specific graphs####
labels_TP<-tibble(x=rep(-Inf,4),y.line=c(10,25,100,NA),y.label=c((10/2),(10+25)/2,(25+100)/2,120),y.label.text=c("0","M","E","HE"))
y.max_TP<-max(125,max(privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="TP")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE),na.rm=TRUE)
(gg.TP.lake<-ggplot(data=privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="TP"),aes(x=year,y=parameterValue_mean*scale_to_mg))+
    geom_hline(yintercept=labels_TP$y.line*scale_to_mg,linetype=2,color="grey")+
    geom_text(data=labels_TP,aes(x=x,y=y.label*scale_to_mg,label=y.label.text),hjust=-0.2,color="grey")+
    geom_errorbar(aes(ymin = (parameterValue_mean-parameterValue_sderr)*scale_to_mg, ymax = (parameterValue_mean+parameterValue_sderr)*scale_to_mg))+
    geom_point(size=2,shape=21,fill="light grey")+
    geom_line(aes(x=year,y=parameterValue_mean_fit*scale_to_mg),color="blue")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(TP~(mg*'/'*L)))+
    scale_y_continuous(limits=c(0,y.max_TP*scale_to_mg)))

#CHL_A_COR specific graphs####
labels_CHL_A_COR<-tibble(x=rep(-Inf,4),y.line=c(3,9,40,NA),y.label=c((3/2),(3+9)/2,(9+40)/2,43),y.label.text=c("0","M","E","HE"))
y.max_CHL_A_COR<-max(45,max(privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="CHL_A_COR")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE),na.rm=TRUE)
(gg.CHL_A_COR.lake<-ggplot(data=privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="CHL_A_COR"),aes(x=year,y=parameterValue_mean))+
    geom_hline(yintercept=labels_CHL_A_COR$y.line,linetype=2,color="grey")+
    geom_text(data=labels_CHL_A_COR,aes(x=x,y=y.label,label=y.label.text),hjust=-0.2,color="grey")+
    geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+
    geom_point(size=2,shape=21,fill="light grey")+
    geom_line(aes(x=year,y=parameterValue_mean_fit),color="blue")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(Chl~italic(a)~(mu*g*'/'*L)))+
    scale_y_continuous(limits=c(0,y.max_CHL_A_COR)))

#SECCHI specific graphs####
labels_SECCHI<-tibble(x=rep(-Inf,4),y.line=c(0.45,1.3,2.6,NA),y.label=c((0.45/2),(0.45+1.3)/2,(1.3+2.6)/2,3),y.label.text=c("HE","E","M","O"))
y.max_SECCHI<-max(3.2,max(privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="SECCHI")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE),na.rm=TRUE)
(gg.SECCHI.lake<-ggplot(data=privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="SECCHI"),aes(x=year,y=parameterValue_mean))+
    geom_hline(yintercept=labels_SECCHI$y.line,linetype=2,color="grey")+ #Lines for indicating trophic state
    geom_text(data=labels_SECCHI,aes(x=x,y=y.label,label=y.label.text),hjust=-0.2,color="grey")+ #labels for trophic state
    geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+
    geom_point(size=2,shape=21,fill="light grey")+
    geom_line(aes(x=year,y=parameterValue_mean_fit),color="blue")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(Secchi~(m)))+
    #scale_y_continuous()+
    scale_y_reverse(limits=c(y.max_SECCHI,-0.2))
)

#TSS specific graphs####
y.max_TSS<-max(privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="TSS")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE)
(gg.TSS.lake<-ggplot(data=privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="TSS"),aes(x=year,y=parameterValue_mean))+
    #geom_hline(yintercept=labels_TN$y.line,linetype=2,color="grey")+
    #geom_text(data=labels_TN,aes(x=x,y=y.label,label=y.label.text),hjust=-0.2,color="grey")+
    geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+
    geom_point(size=2,shape=21,fill="light grey")+
    geom_line(aes(x=year,y=parameterValue_mean_fit),color="blue")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(TSS~(mg*'/'*L)))+
    scale_y_continuous(limits=c(0,y.max_TSS)))

#PIM specific graphs####
y.max_PIM<-max(privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="PIM")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE)
(gg.PIM.lake<-ggplot(data=privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="PIM"),aes(x=year,y=parameterValue_mean))+
    #geom_hline(yintercept=labels_TN$y.line,linetype=2,color="grey")+
    #geom_text(data=labels_TN,aes(x=x,y=y.label,label=y.label.text),hjust=-0.2,color="grey")+
    geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+
    geom_point(size=2,shape=21,fill="light grey")+
    geom_line(aes(x=year,y=parameterValue_mean_fit),color="blue")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(PIM~(mg*'/'*L)))+
    scale_y_continuous(limits=c(0,y.max_PIM)))

#POM specific graphs####
y.max_POM<-max(privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="POM")%>%mutate(upper=parameterValue_mean+parameterValue_sderr)%>%dplyr::select(upper)%>%ungroup()%>%pull(),na.rm=TRUE)
(gg.POM.lake<-ggplot(data=privateLakes_merged_annual_slopes%>%filter(MULakeNumber==lake.id&parameterType=="POM"),aes(x=year,y=parameterValue_mean))+
    #geom_hline(yintercept=labels_TN$y.line,linetype=2,color="grey")+
    #geom_text(data=labels_TN,aes(x=x,y=y.label,label=y.label.text),hjust=-0.2,color="grey")+
    geom_errorbar(aes(ymin = parameterValue_mean-parameterValue_sderr, ymax = parameterValue_mean+parameterValue_sderr))+
    geom_point(size=2,shape=21,fill="light grey")+
    geom_line(aes(x=year,y=parameterValue_mean_fit),color="blue")+
    theme_bw()+
    xlab("Year")+
    ylab(bquote(POM~(mg*'/'*L)))+
    scale_y_continuous(limits=c(0,y.max_POM)))

#Find the absolute max and min for the years####
year.range<-privateLakes_merged_annual%>%filter(MULakeNumber==lake.id)%>%ungroup()%>%summarize(min.year=min(year,na.rm=TRUE)-1,max.year=max(year,na.rm=TRUE)+1)%>%slice(1)%>%as.numeric()

#Merge together in 1 four panel frame with A, B, C, D labels in upper left and no x/y labels where appropriate
IndLakeFigureList<-list(gg.TN.lake+
                          xlab("")+
                          theme(axis.text.x=element_blank(),
                                panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          )+
                          scale_x_continuous(limits=year.range)+
                          annotate("text",x=Inf,y=Inf,label="A",hjust=1.2,vjust=1.2),
                        gg.TP.lake+
                          xlab("")+
                          #ylab("")+
                          theme(axis.text.x=element_blank(),
                                #axis.text.y=element_blank(),
                                #axis.ticks.x=element_blank(),
                                panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          )+
                          scale_x_continuous(limits=year.range)+
                          annotate("text",x=Inf,y=Inf,label="B",hjust=1.2,vjust=1.2),
                        gg.CHL_A_COR.lake+
                          #xlab("")+
                          #ylab("")+
                          theme( #axis.text.x=element_blank(),
                            #axis.text.y=element_blank(),
                            #axis.ticks.x=element_blank(),
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          )+
                          scale_x_continuous(limits=year.range)+
                          annotate("text",x=Inf,y=Inf,label="C",hjust=1.2,vjust=1.2),
                        gg.SECCHI.lake+
                          #xlab("")+
                          #ylab("")+
                          theme(#axis.text.x=element_blank(),
                            #axis.text.y=element_blank(),
                            #axis.ticks.x=element_blank(),
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          )+
                          scale_x_continuous(limits=year.range)+
                          annotate("text",x=Inf,y=-Inf,label="D",hjust=1.2,vjust=1.2)
)

#Put the plots on a 2x2 matrix####
(gg.trophic.Individuallake<-wrap_plots(IndLakeFigureList,ncol=2,nrow=2)&theme(plot.margin = unit(c(3,3,3,3),"pt")))

#Export the plot as a jpg####
ggsave(gg.trophic.Individuallake,file=paste0("09_Figures/",year,"_IndividualLakeReports/","SLAPReport-Figure3-",year,"-MULakeNumber-",lake.id,".jpg"),width=plot.width,height=plot.height,units="in",dpi=300)


#Merge together in 1 four panel frame with A, B, C, D labels in upper left and no x/y labels where appropriate
IndLakeFigureList.TSS<-list(gg.TSS.lake+
                          xlab("")+
                          theme(axis.text.x=element_blank(),
                                panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          )+
                          scale_x_continuous(limits=year.range)+
                          annotate("text",x=Inf,y=Inf,label="A",hjust=1.2,vjust=1.2),
                        gg.PIM.lake+
                          xlab("")+
                          theme(axis.text.x=element_blank(),
                                panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          )+
                          scale_x_continuous(limits=year.range)+
                          annotate("text",x=Inf,y=Inf,label="B",hjust=1.2,vjust=1.2),
                        gg.POM.lake+
                          #xlab("")+
                          theme(#axis.text.x=element_blank(),
                                panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                          )+
                          scale_x_continuous(limits=year.range)+
                          annotate("text",x=Inf,y=Inf,label="C",hjust=1.2,vjust=1.2)
)

#Put the plots on a 3x1 matrix####
(gg.trophic.Individuallake.TSS<-wrap_plots(IndLakeFigureList.TSS,ncol=1,nrow=3)&theme(plot.margin = unit(c(3,3,3,3),"pt")))

#Export the plot as a jpg####
ggsave(gg.trophic.Individuallake.TSS,file=paste0("09_Figures/",year,"_IndividualLakeReports/","SLAPReport-Figure4-",year,"-MULakeNumber-",lake.id,".jpg"),width=plot.width/2,height=plot.height*(3/2),units="in",dpi=300)

} #end of loop

