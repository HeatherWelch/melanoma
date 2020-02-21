### making maps

source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
master=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/master_dataframe.csv") %>% mutate(COUNTY_FIPS=as.character(str_pad(COUNTY_FIPS, 5, pad = "0")))%>% mutate(STATEFP=as.character(STATEFP)) %>% dplyr::select(-c(STATEFP))

studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
studyarea=studyarea %>% mutate(COUNTY_FIPS=as.character(glue("{STATEFP}{COUNTYFP}")))

outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/maps"
dat=full_join(studyarea,master,by="COUNTY_FIPS") 

library(scales)

mappfunction=function(dat,mapVar){
  if(mapVar=="SEER_rate"){
    varName="NHW melanoma incidence"
  } else if (mapVar=="anRange_temperature"){
    varName="Annual range temperature"
  } else if (mapVar=="cancer_gov_UV_exposure"){
    varName="UV exposure"
  } else if (mapVar=="mean_cloud"){
    varName="Mean cloud cover"
  } else if (mapVar=="elevation"){
    varName="Elevation"
  } else if (mapVar=="mean_temperature"){
    varName="Mean temperature"
  } else if (mapVar=="seasonality_cloud"){
    varName="Seasonality of cloud cover"
  } else if (mapVar=="seasonality_temperature"){
    varName="Seasonality of temperature"
  } else if (mapVar=="sun_exposure"){
    varName="Sun exposure"
  } else if (mapVar=="UV_daily_dose"){
    varName="UV daily dose"
  } else if (mapVar=="UV_irradiance"){
    varName="UV irradiance"
  } else if (mapVar=="incm_pc"){
    varName="Income per capita"
  } else if (mapVar=="incm_mh"){
    varName="Median household income"
  } else if (mapVar=="derm_pk"){
    varName="Dermatologists"
  } else if (mapVar=="pcp_pk"){
    varName="Primary care"
  } else if (mapVar=="docs_pk"){
    varName="Doctors"
  } else if (mapVar=="wpovr50"){
    varName="Households >$50,000"
  } else if (mapVar=="wpvr100"){
    varName="Households >$100,000"
  } else if (mapVar=="HI_65"){
    varName="Health insurance"
  }
# dat=dat[1:10,]
  if(varName=="Health insurance"||varName=="Households >$100,000"||varName=="Households >$50,000"||varName=="Doctors"||varName=="Primary care"||varName=="Dermatologists"||varName=="NHW melanoma incidence"||varName=="Median household income"||varName=="Income per capita"){
  datt=dat %>% mutate(new=.data[[mapVar]])
  labels=pretty(datt$new,n=10)
  if(grepl("income",varName)){labels2=dollar(labels)
  } else if(grepl("Income",varName)){labels2=dollar(labels)
  } else if(grepl("Households",varName)){labels2=percent(labels/100)
  } else if(grepl("Health",varName)){labels2=percent(labels/100)
  } else {labels2=labels}
  map=ggplot(data=dat)+geom_sf(aes(fill=ntile(.data[[mapVar]],100)), size = 0.4,color="black")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    scale_fill_gradientn(varName,breaks=rescale(labels,c(0,100)),labels=labels2,colours = pals::parula(100),na.value="black")+
    coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(), 
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())+
    theme(legend.title = element_text(size=8),legend.position=c(.92,.4),legend.key.width = unit(1.5, "cm"),legend.key.height = unit(1.4, "cm"))+theme(legend.text=element_text(size=8),legend.title = element_text(size=9))
  } else {
    map=ggplot(data=dat)+geom_sf(aes(fill=.data[[mapVar]]), size = 0.4,color="black")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
      scale_fill_gradientn(varName,colours = pals::parula(100),na.value="black")+
      coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(), 
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())+
      theme(legend.title = element_text(size=8),legend.position=c(.92,.4),legend.key.width = unit(1.5, "cm"),legend.key.height = unit(1.4, "cm"))+theme(legend.text=element_text(size=8),legend.title = element_text(size=9))
  }
  
  png(glue("{outdir}/{mapVar}.png"),width=36,height=22,type = "cairo",units='cm',res=400)
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  print({map})
  dev.off()
}
mappfunctionEnvt=function(dat,mapVar,varName){
  # dat=dat[1:10,]
  datt=dat %>% mutate(new=.data[[mapVar]])
  labels=pretty(datt$new,n=10)
  map=ggplot(data=dat)+geom_sf(aes(fill=.data[[mapVar]]), size = 0.4,color="black")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    scale_fill_gradientn(varName,colours = pals::parula(100),na.value="black")+
    coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(), 
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())+
    theme(legend.title = element_text(size=8),legend.position=c(.92,.4),legend.key.width = unit(1.5, "cm"),legend.key.height = unit(1.4, "cm"))+theme(legend.text=element_text(size=8),legend.title = element_text(size=9))
  
  map
  
  png(glue("{outdir}/{mapVar}.png"),width=36,height=22,type = "cairo",units='cm',res=400)
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  print({map})
  dev.off()
}



library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(5)

predictions=names(dat)[c(20,22:31,33:40)]
# predictions=names(dat)[c(20)]
system.time(print(
  foreach(i=1:length(predictions),.export = c("mappfunction","outdir","predictions","dat"),.packages = c("ggplot2","glue","maps","tidyverse","scales"),.verbose=T) %dopar% {
    mappfunction(mapVar=predictions[i],dat=dat)
  }
))

# predictions=names(dat)[c(22:31)]
# # predictions=names(dat)[c(20)]
# system.time(print(
#   foreach(i=1:length(predictions),.export = c("mappfunctionEnvt","outdir","predictions","dat"),.packages = c("ggplot2","glue","maps","tidyverse","scales"),.verbose=T) %dopar% {
#     # old junk with lablels ####
#     # if(predictions[i]=="SEER_rate"){
#     #   varName="NHW melanoma incidence"
#     # } else if (predictions[i]=="anRange_temperature"){
#     #   varName="Annual range temperature (°C)"
#     # } else if (predictions[i]=="cancer_gov_UV_exposure"){
#     #   varName="UV exposure (mw/m2)"
#     # } else if (predictions[i]=="mean_cloud"){
#     #   varName="Mean cloud cover (% cloudy days per year)"
#     # } else if (predictions[i]=="elevation"){
#     #   varName="Elevation (m)"
#     # } else if (predictions[i]=="mean_temperature"){
#     #   varName="Mean temperature (°C)"
#     # } else if (predictions[i]=="seasonality_cloud"){
#     #   varName="Seasonality of cloud cover"
#     # } else if (predictions[i]=="seasonality_temperature"){
#     #   varName="Seasonality of temperature"
#     # } else if (predictions[i]=="sun_exposure"){
#     #   varName="Sun exposure (kj/m2)"
#     # } else if (predictions[i]=="UV_daily_dose"){
#     #   varName="UV daily dose (j/m2)"
#     # } else if (predictions[i]=="UV_irradiance"){
#     #   varName="UV irradiance (mw/m2)"
#     # } else if (predictions[i]=="incm_pc"){
#     #   varName="Income per capita (USD)"
#     # } else if (predictions[i]=="incm_mh"){
#     #   varName="Median household income (USD)"
#     # } else if (predictions[i]=="derm_pk"){
#     #   varName="Dermatologists"
#     # } else if (predictions[i]=="pcp_pk"){
#     #   varName="Primary care"
#     # } else if (predictions[i]=="docs_pk"){
#     #   varName="Doctors"
#     # } else if (predictions[i]=="wpovr50"){
#     #   varName="Households >$50,000 (%)"
#     # } else if (predictions[i]=="wpvr100"){
#     #   varName="Households >$100,000 (%)"
#     # } else if (predictions[i]=="HI_65"){
#     #   varName="Health insurance < age 65 (%)"
#     # }
#     # 
#     
#     # old junk without lablels ####
#     if(predictions[i]=="SEER_rate"){
#       varName="NHW melanoma incidence"
#     } else if (predictions[i]=="anRange_temperature"){
#       varName="Annual range temperature"
#     } else if (predictions[i]=="cancer_gov_UV_exposure"){
#       varName="UV exposure"
#     } else if (predictions[i]=="mean_cloud"){
#       varName="Mean cloud cover"
#     } else if (predictions[i]=="elevation"){
#       varName="Elevation"
#     } else if (predictions[i]=="mean_temperature"){
#       varName="Mean temperature"
#     } else if (predictions[i]=="seasonality_cloud"){
#       varName="Seasonality of cloud cover"
#     } else if (predictions[i]=="seasonality_temperature"){
#       varName="Seasonality of temperature"
#     } else if (predictions[i]=="sun_exposure"){
#       varName="Sun exposure"
#     } else if (predictions[i]=="UV_daily_dose"){
#       varName="UV daily dose"
#     } else if (predictions[i]=="UV_irradiance"){
#       varName="UV irradiance"
#     } else if (predictions[i]=="incm_pc"){
#       varName="Income per capita"
#     } else if (predictions[i]=="incm_mh"){
#       varName="Median household income"
#     } else if (predictions[i]=="derm_pk"){
#       varName="Dermatologists"
#     } else if (predictions[i]=="pcp_pk"){
#       varName="Primary care"
#     } else if (predictions[i]=="docs_pk"){
#       varName="Doctors"
#     } else if (predictions[i]=="wpovr50"){
#       varName="Households >$50,000"
#     } else if (predictions[i]=="wpvr100"){
#       varName="Households >$100,000"
#     } else if (predictions[i]=="HI_65"){
#       varName="Health insurance"
#     }
#     
#     mappfunctionEnvt(mapVar=predictions[i],dat=dat,varName = varName)
#   }
# ))
# 
