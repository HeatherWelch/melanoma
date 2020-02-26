source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
# library(gam)
library(mgcv)
library(lme4)
library(MuMIn)

# seer rate all data ####
modDF=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/master_dataframe.csv") %>% dplyr::select(-c(X,Pop,COUNTY_FIPS,STATEFP,NAME))
empty=list()

for(i in 2:ncol(modDF)){
  var=names(modDF)[i]
  print(var)
  # build models
  fm2=lm(as.formula(glue("SEER_rate ~ {var}")),data=modDF)
  # fm4 <- lmer(as.formula(glue("SEER_rate ~ {var}+(1|STATEFP)")), data = modDF)
 
   # get coefficients
  lm_summary=summary(fm2)
  # lmm_summary=summary(fm4)
  
  lm_int=lm_summary$coefficients[1]
  lm_slope=lm_summary$coefficients[2]
  
  # lmm_int=lmm_summary$coefficients[1]
  # lmm_slope=lmm_summary$coefficients[2]
  
  # get rsqured
  lm_rsq_fixed=r.squaredGLMM(fm2)[1]
  # lmm_rsq_fixed=r.squaredGLMM(fm4)[1]
  lm_rsq_random=r.squaredGLMM(fm2)[2]
  # lmm_rsq_random=r.squaredGLMM(fm4)[2]
  
  dat_lm=data.frame(variable=as.character(var),
                 slope=as.numeric(lm_slope),
                 intercept=as.numeric(lm_int),
                 rsq_fixed=as.numeric(lm_rsq_fixed),
                 modtype=as.character("linear"),
                 rsq_fixed_plus_random=as.numeric(lm_rsq_random),
                 stringsAsFactors = F)
  # dat_lmm=data.frame(variable=as.character(var),
  #                   slope=as.numeric(lmm_slope),
  #                   intercept=as.numeric(lmm_int),
  #                   rsq_fixed=as.numeric(lmm_rsq_fixed),
  #                   modtype=as.character("mixed"),
  #                   rsq_fixed_plus_random=as.numeric(lmm_rsq_random),
  #                   stringsAsFactors = F)
  # dat=rbind(dat_lm,dat_lmm)
  
  empty[[length(empty)+1]]<-dat_lm
  
  }


master=do.call(rbind,empty) %>% filter(variable!="STATEFP" &  variable!="NAME")
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/scatterplot"

for(i in 1:length(unique(master$variable))){
  
  mapVar=unique(master$variable)[i]
  
        if(mapVar=="SEER_rate"){
          varName="NHW melanoma incidence"
        } else if (mapVar=="anRange_temperature"){
          varName="Annual range temperature (°C)"
        } else if (mapVar=="cancer_gov_UV_exposure"){
          varName="UV exposure (mw/m2)"
        } else if (mapVar=="mean_cloud"){
          varName="Mean cloud cover (% cloudy days per year)"
        } else if (mapVar=="elevation"){
          varName="Elevation (m)"
        } else if (mapVar=="mean_temperature"){
          varName="Mean temperature (°C)"
        } else if (mapVar=="seasonality_cloud"){
          varName="Seasonality of cloud cover"
        } else if (mapVar=="seasonality_temperature"){
          varName="Seasonality of temperature"
        } else if (mapVar=="sun_exposure"){
          varName="Sun exposure (kj/m2)"
        } else if (mapVar=="UV_daily_dose"){
          varName="UV daily dose (j/m2)"
        } else if (mapVar=="UV_irradiance"){
          varName="UV irradiance (mw/m2)"
        } else if (mapVar=="incm_pc"){
          varName="Income per capita (USD)"
        } else if (mapVar=="incm_mh"){
          varName="Median household income (USD)"
        } else if (mapVar=="derm_pk"){
          varName="Dermatologists"
        } else if (mapVar=="pcp_pk"){
          varName="Primary care"
        } else if (mapVar=="docs_pk"){
          varName="Doctors"
        } else if (mapVar=="wpovr50"){
          varName="Households >$50,000 (%)"
        } else if (mapVar=="wpvr100"){
          varName="Households >$100,000 (%)"
        } else if (mapVar=="HI_65"){
          varName="Health insurance < age 65 (%)"
        }

dat=master %>% filter(variable==mapVar)
plott=ggplot(modDF,aes(x=.data[[mapVar]],y=SEER_rate))+geom_point(size=1)+geom_abline(slope=dat[1,2],intercept = dat[1,3],color="red",size=1)+geom_abline(slope=dat[2,2],intercept = dat[2,3],color="blue")+
  xlab(varName)+ylab("NHW melanoma incidence")+theme_classic()+ylim(c(0,150))+
  annotate("text", Inf, Inf, label = glue("R squared={round(dat$rsq_fixed,3)}"), hjust =1.5, vjust = 2.5)#+
  # annotate("text", Inf, Inf, label = glue("Slope={round(dat$slope,3)}"), hjust =1.5, vjust = 3.8)

if(mapVar=="docs_pk"){
  plott=plott+xlim(c(0,1300))
}
if(mapVar=="derm_pk"){
  plott=plott+xlim(c(0,30))
}
if(mapVar=="pcp_pk"){
  plott=plott+xlim(c(0,200))
}

plott

png(glue("{outdir}/{mapVar}.png"),width=10,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({plott})
dev.off()
}


# ## not run ##
# a=ggplot(data=master,aes(x=modtype,y=intercept))+geom_bar(aes(fill=modtype),stat="identity")+facet_wrap(~variable,scales="free")+
#   scale_fill_manual("model type",values=c("linear"="red","mixed"="blue"))+theme_classic()
# b=ggplot(data=master,aes(x=modtype,y=rsq_fixed))+geom_bar(aes(fill=modtype),stat="identity")+facet_wrap(~variable,scales="free")+
#   scale_fill_manual("model type",values=c("linear"="red","mixed"="blue"))+theme_classic()
# c=ggplot(data=master,aes(x=modtype,y=rsq_fixed_plus_random))+geom_bar(aes(fill=modtype),stat="identity")+facet_wrap(~variable,scales="free")+
#   scale_fill_manual("model type",values=c("linear"="red","mixed"="blue"))+theme_classic()
# 
# png(glue("{outdir}/intercept.png"),width=20,height=20,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# print({a})
# dev.off()
# 
# png(glue("{outdir}/rsq_fixed.png"),width=20,height=20,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# print({b})
# dev.off()
# 
# png(glue("{outdir}/rsq_fullmodel.png"),width=20,height=20,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# print({c})
# dev.off()
# 
# filter(master,modtype=="linear") %>% arrange(rsq_fixed)
# filter(master,modtype=="mixed") %>% arrange(rsq_fixed)


# seer rate all data Pop > 500000 ####
modDF=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/master_dataframe.csv")%>% mutate(Pop=as.numeric(gsub(",","",as.character(Pop)))) %>% filter(Pop>500000) %>% dplyr::select(-c(X,Pop,COUNTY_FIPS,STATEFP,NAME))
empty=list()

for(i in 2:ncol(modDF)){
  var=names(modDF)[i]
  print(var)
  # build models
  fm2=lm(as.formula(glue("SEER_rate ~ {var}")),data=modDF)
  # fm4 <- lmer(as.formula(glue("SEER_rate ~ {var}+(1|STATEFP)")), data = modDF)
  
  # get coefficients
  lm_summary=summary(fm2)
  # lmm_summary=summary(fm4)
  
  lm_int=lm_summary$coefficients[1]
  lm_slope=lm_summary$coefficients[2]
  
  # lmm_int=lmm_summary$coefficients[1]
  # lmm_slope=lmm_summary$coefficients[2]
  
  # get rsqured
  lm_rsq_fixed=r.squaredGLMM(fm2)[1]
  # lmm_rsq_fixed=r.squaredGLMM(fm4)[1]
  lm_rsq_random=r.squaredGLMM(fm2)[2]
  # lmm_rsq_random=r.squaredGLMM(fm4)[2]
  
  dat_lm=data.frame(variable=as.character(var),
                    slope=as.numeric(lm_slope),
                    intercept=as.numeric(lm_int),
                    rsq_fixed=as.numeric(lm_rsq_fixed),
                    modtype=as.character("linear"),
                    rsq_fixed_plus_random=as.numeric(lm_rsq_random),
                    stringsAsFactors = F)
  # dat_lmm=data.frame(variable=as.character(var),
  #                   slope=as.numeric(lmm_slope),
  #                   intercept=as.numeric(lmm_int),
  #                   rsq_fixed=as.numeric(lmm_rsq_fixed),
  #                   modtype=as.character("mixed"),
  #                   rsq_fixed_plus_random=as.numeric(lmm_rsq_random),
  #                   stringsAsFactors = F)
  # dat=rbind(dat_lm,dat_lmm)
  
  empty[[length(empty)+1]]<-dat_lm
  
}


master=do.call(rbind,empty) %>% filter(variable!="STATEFP" &  variable!="NAME")
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/scatterplot_80";dir.create(outdir)

for(i in 1:length(unique(master$variable))){
  
  mapVar=unique(master$variable)[i]
  
  if(mapVar=="SEER_rate"){
    varName="NHW melanoma incidence"
  } else if (mapVar=="anRange_temperature"){
    varName="Annual range temperature (°C)"
  } else if (mapVar=="cancer_gov_UV_exposure"){
    varName="UV exposure (mw/m2)"
  } else if (mapVar=="mean_cloud"){
    varName="Mean cloud cover (% cloudy days per year)"
  } else if (mapVar=="elevation"){
    varName="Elevation (m)"
  } else if (mapVar=="mean_temperature"){
    varName="Mean temperature (°C)"
  } else if (mapVar=="seasonality_cloud"){
    varName="Seasonality of cloud cover"
  } else if (mapVar=="seasonality_temperature"){
    varName="Seasonality of temperature"
  } else if (mapVar=="sun_exposure"){
    varName="Sun exposure (kj/m2)"
  } else if (mapVar=="UV_daily_dose"){
    varName="UV daily dose (j/m2)"
  } else if (mapVar=="UV_irradiance"){
    varName="UV irradiance (mw/m2)"
  } else if (mapVar=="incm_pc"){
    varName="Income per capita (USD)"
  } else if (mapVar=="incm_mh"){
    varName="Median household income (USD)"
  } else if (mapVar=="derm_pk"){
    varName="Dermatologists"
  } else if (mapVar=="pcp_pk"){
    varName="Primary care"
  } else if (mapVar=="docs_pk"){
    varName="Doctors"
  } else if (mapVar=="wpovr50"){
    varName="Households >$50,000 (%)"
  } else if (mapVar=="wpvr100"){
    varName="Households >$100,000 (%)"
  } else if (mapVar=="HI_65"){
    varName="Health insurance < age 65 (%)"
  }
  
  dat=master %>% filter(variable==mapVar)
  plott=ggplot(modDF,aes(x=.data[[mapVar]],y=SEER_rate))+geom_point(size=1)+geom_abline(slope=dat[1,2],intercept = dat[1,3],color="red",size=1)+geom_abline(slope=dat[2,2],intercept = dat[2,3],color="blue")+
    xlab(varName)+ylab("NHW melanoma incidence")+theme_classic()+ylim(c(0,150))+
    annotate("text", Inf, Inf, label = glue("R squared={round(dat$rsq_fixed,3)}"), hjust =1.5, vjust = 2.5)#+
  # annotate("text", Inf, Inf, label = glue("Slope={round(dat$slope,3)}"), hjust =1.5, vjust = 3.8)
  
  if(mapVar=="docs_pk"){
    plott=plott+xlim(c(0,1300))
  }
  if(mapVar=="derm_pk"){
    plott=plott+xlim(c(0,30))
  }
  if(mapVar=="pcp_pk"){
    plott=plott+xlim(c(0,200))
  }
  
  plott
  
  png(glue("{outdir}/{mapVar}.png"),width=10,height=10,units='cm',res=400)
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  print({plott})
  dev.off()
}


# ## not run ##
# a=ggplot(data=master,aes(x=modtype,y=intercept))+geom_bar(aes(fill=modtype),stat="identity")+facet_wrap(~variable,scales="free")+
#   scale_fill_manual("model type",values=c("linear"="red","mixed"="blue"))+theme_classic()
# b=ggplot(data=master,aes(x=modtype,y=rsq_fixed))+geom_bar(aes(fill=modtype),stat="identity")+facet_wrap(~variable,scales="free")+
#   scale_fill_manual("model type",values=c("linear"="red","mixed"="blue"))+theme_classic()
# c=ggplot(data=master,aes(x=modtype,y=rsq_fixed_plus_random))+geom_bar(aes(fill=modtype),stat="identity")+facet_wrap(~variable,scales="free")+
#   scale_fill_manual("model type",values=c("linear"="red","mixed"="blue"))+theme_classic()
# 
# png(glue("{outdir}/intercept.png"),width=20,height=20,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# print({a})
# dev.off()
# 
# png(glue("{outdir}/rsq_fixed.png"),width=20,height=20,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# print({b})
# dev.off()
# 
# png(glue("{outdir}/rsq_fullmodel.png"),width=20,height=20,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# print({c})
# dev.off()
# 
# filter(master,modtype=="linear") %>% arrange(rsq_fixed)
# filter(master,modtype=="mixed") %>% arrange(rsq_fixed)


# seer rate all +ivasive + in situ data ####
modDF=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/master_dataframe.csv") %>% dplyr::select(-c(X,Pop,STATEFP,NAME)) %>% .[complete.cases(.),]
seer=read.csv("/Users/heatherwelch/Dropbox/melenoma/medical/Melanoma_insitu_vs_invasive.csv") %>% mutate(COUNTY_FIPS=as.integer(FIPS))
test=left_join(modDF,seer)
empty=list()

for(i in 3:20){
  var=names(test)[i]
  print(var)
  # build models
  fm2=lm(as.formula(glue("In.situ ~ {var}")),data=test)
  fm2a=lm(as.formula(glue("Invasive ~ {var}")),data=test)
  fm2b=lm(as.formula(glue("SEER_rate ~ {var}")),data=test)
  # fm4 <- lmer(as.formula(glue("SEER_rate ~ {var}+(1|STATEFP)")), data = modDF)
  
  # get coefficients
  lm_summary=summary(fm2)
  lm_summarya=summary(fm2a)
  lm_summaryb=summary(fm2b)
  
  lm_int=lm_summary$coefficients[1]
  lm_slope=lm_summary$coefficients[2]
  
  lm_inta=lm_summarya$coefficients[1]
  lm_slopea=lm_summarya$coefficients[2]
  
  lm_intb=lm_summaryb$coefficients[1]
  lm_slopeb=lm_summaryb$coefficients[2]
  
  # get rsqured
  lm_rsq_fixed=r.squaredGLMM(fm2)[1]
  lm_rsq_random=r.squaredGLMM(fm2)[2]
  
  lm_rsq_fixeda=r.squaredGLMM(fm2a)[1]
  lm_rsq_randoma=r.squaredGLMM(fm2a)[2]
  
  lm_rsq_fixedb=r.squaredGLMM(fm2b)[1]
  lm_rsq_randomb=r.squaredGLMM(fm2b)[2]
  
  dat_lm=data.frame(variable=as.character(var),
                    slope=as.numeric(lm_slope),
                    intercept=as.numeric(lm_int),
                    rsq_fixed=as.numeric(lm_rsq_fixed),
                    modtype=as.character("In.situ"),
                    rsq_fixed_plus_random=as.numeric(lm_rsq_random),
                    stringsAsFactors = F)
  
  dat_lma=data.frame(variable=as.character(var),
                    slope=as.numeric(lm_slopea),
                    intercept=as.numeric(lm_inta),
                    rsq_fixed=as.numeric(lm_rsq_fixeda),
                    modtype=as.character("Invasive"),
                    rsq_fixed_plus_random=as.numeric(lm_rsq_randoma),
                    stringsAsFactors = F)
  
  dat_lmb=data.frame(variable=as.character(var),
                     slope=as.numeric(lm_slopeb),
                     intercept=as.numeric(lm_intb),
                     rsq_fixed=as.numeric(lm_rsq_fixedb),
                     modtype=as.character("All"),
                     rsq_fixed_plus_random=as.numeric(lm_rsq_randomb),
                     stringsAsFactors = F)
  
  dat=rbind(dat_lm,dat_lma,dat_lmb)
  
  empty[[length(empty)+1]]<-dat
  
}

master=do.call(rbind,empty) %>% filter(variable!="STATEFP" &  variable!="NAME")
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/scatterplot/insitu_invasive_all"#;dir.create(outdir)

for(i in 1:length(unique(master$variable))){
  
  mapVar=unique(master$variable)[i]
  
  if(mapVar=="SEER_rate"){
    varName="NHW melanoma incidence"
  } else if (mapVar=="anRange_temperature"){
    varName="Annual range temperature (°C)"
  } else if (mapVar=="cancer_gov_UV_exposure"){
    varName="UV exposure (mw/m2)"
  } else if (mapVar=="mean_cloud"){
    varName="Mean cloud cover (% cloudy days per year)"
  } else if (mapVar=="elevation"){
    varName="Elevation (m)"
  } else if (mapVar=="mean_temperature"){
    varName="Mean temperature (°C)"
  } else if (mapVar=="seasonality_cloud"){
    varName="Seasonality of cloud cover"
  } else if (mapVar=="seasonality_temperature"){
    varName="Seasonality of temperature"
  } else if (mapVar=="sun_exposure"){
    varName="Sun exposure (kj/m2)"
  } else if (mapVar=="UV_daily_dose"){
    varName="UV daily dose (j/m2)"
  } else if (mapVar=="UV_irradiance"){
    varName="UV irradiance (mw/m2)"
  } else if (mapVar=="incm_pc"){
    varName="Income per capita (USD)"
  } else if (mapVar=="incm_mh"){
    varName="Median household income (USD)"
  } else if (mapVar=="derm_pk"){
    varName="Dermatologists"
  } else if (mapVar=="pcp_pk"){
    varName="Primary care"
  } else if (mapVar=="docs_pk"){
    varName="Doctors"
  } else if (mapVar=="wpovr50"){
    varName="Households >$50,000 (%)"
  } else if (mapVar=="wpvr100"){
    varName="Households >$100,000 (%)"
  } else if (mapVar=="HI_65"){
    varName="Health insurance < age 65 (%)"
  }
  
  dat=master %>% filter(variable==mapVar)
  plott=ggplot(test,aes(x=.data[[mapVar]],y=SEER_rate))+geom_point(size=1)+geom_abline(slope=dat[3,2],intercept = dat[3,3],color="red",size=1)+
    xlab(varName)+ylab("NHW all melanoma incidence")+theme_classic()+ylim(c(0,150))+
    annotate("text", Inf, Inf, label = glue("R squared={round(dat[3,4],3)}"), hjust =1.5, vjust = 2.5)
  
  plotta=ggplot(test,aes(x=.data[[mapVar]],y=In.situ))+geom_point(size=1)+geom_abline(slope=dat[1,2],intercept = dat[1,3],color="red",size=1)+
    xlab(varName)+ylab("NHW in situ melanoma incidence")+theme_classic()+ylim(c(0,75))+
    annotate("text", Inf, Inf, label = glue("R squared={round(dat[1,4],3)}"), hjust =1.5, vjust = 2.5)
  
  plottb=ggplot(test,aes(x=.data[[mapVar]],y=Invasive))+geom_point(size=1)+geom_abline(slope=dat[2,2],intercept = dat[2,3],color="red",size=1)+
    xlab(varName)+ylab("NHW invasive melanoma incidence")+theme_classic()+ylim(c(0,100))+
    annotate("text", Inf, Inf, label = glue("R squared={round(dat[2,4],3)}"), hjust =1.5, vjust = 2.5)
  
  if(mapVar=="docs_pk"){
    plott=plott+xlim(c(0,1300))
  }
  if(mapVar=="derm_pk"){
    plott=plott+xlim(c(0,30))
  }
  if(mapVar=="pcp_pk"){
    plott=plott+xlim(c(0,200))
  }
  
  plott
  plotta
  plottb
  
  png(glue("{outdir}/{mapVar}.png"),width=30,height=10,units='cm',res=400)
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  print({grid.arrange(plott,plotta,plottb,ncol=3,nrow=1)})
  dev.off()
}

new=master
new$mapVar=new$variable
new$varName=NA

new =new %>% mutate(varName = case_when(mapVar %in% c("anRange_temperature") ~ "Annual range temperature",
                                        mapVar %in% c("cancer_gov_UV_exposure") ~ "UV exposure",
                                        mapVar %in% c("mean_cloud") ~ "Mean cloud cover",
                                        mapVar %in% c("elevation") ~ "Elevation",
                                        mapVar %in% c("mean_temperature") ~ "Mean temperature",
                                        mapVar %in% c("seasonality_cloud") ~ "Seasonality of cloud cover",
                                        mapVar %in% c("seasonality_temperature") ~ "Seasonality of temperature",
                                        mapVar %in% c("sun_exposure") ~ "Sun exposure",
                                        mapVar %in% c("UV_daily_dose") ~ "UV daily dose",
                                        mapVar %in% c("UV_irradiance") ~ "UV irradiance",
                                        mapVar %in% c("incm_pc") ~ "Income per capita",
                                        mapVar %in% c("incm_mh") ~ "Median household income",
                                        mapVar %in% c("derm_pk") ~ "Dermatologists",
                                        mapVar %in% c("pcp_pk") ~ "Primary care",
                                        mapVar %in% c("docs_pk") ~ "Doctors",
                                        mapVar %in% c("wpovr50") ~ "Households >$50,000",
                                        mapVar %in% c("wpvr100") ~ "Households >$100,000",
                                        mapVar %in% c("HI_65") ~ "Health insurance < age 65",
))
                                  

# ggplot(dat=master,aes(x=reorder(rsq_fixed),y=intercept))+geom_point(aes(color=variable,shape=modtype))
label_data2 <- new %>% group_by(varName) %>%
  summarise(sum=sum(rsq_fixed)) %>%
  arrange(desc(sum))
label_data2$id <- c(1:nrow(label_data2))


# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data2)
angle <-  90 - 360 * ( label_data2$id-.5 )  /number_of_bar    

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data2$hjust<-ifelse( angle < -90, 1, 0)

# flip angle to make them readable
label_data2$angle<-ifelse(angle < -90, angle+180, angle)

# find order of countries to use for scale_x, turn into factor
order <- label_data2$varName
order <- c(order)
order <- factor(order, levels=order)

pal <- c("#e76a6a","grey","#6dac4f")

map=ggplot(dat=new,aes(x=varName,rsq_fixed))+geom_bar(aes(group=modtype,fill=modtype),stat="identity")+
  scale_x_discrete(limits=order)+
  scale_fill_manual("Melanoma type",values=pal)+
  theme_void() +
  #cale_fill_manual("Melanoma type",values=c("All"="red","Invasive"="blue","In.situ"="grey"))+
  coord_polar(direction = 1,
              clip = "off") +ylim(-.05,.5)+
  geom_text(data=label_data2, aes(x=id, y=sum+.03, label=varName, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=2, 
            angle= label_data2$angle, inherit.aes = FALSE ) +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))+
  theme(legend.title = element_text(size=2),legend.position=c(.3,.8),legend.key.width = unit(.3, "cm"),legend.key.height = unit(.2, "cm"))+theme(legend.text=element_text(size=4),legend.title = element_text(size=5))

map=map+  annotate(geom="text",
                   x="Households >$50,000",y=.45,
                   label="0.45-",
                   hjust=3.0,
                   size=1.5,
                   color="black",
                   alpha=.7) +
  annotate(geom="text",
              x="Households >$50,000",y=.35,
              label="0.35-",
              hjust=2.6,
              size=1.5,
              color="black",
              alpha=.7)+
  annotate(geom="text",
              x="Households >$50,000",y=.25,
              label="0.25-",
              hjust=2.2,
              size=1.5,
              color="black",
              alpha=.7)
map
  
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20"
png(glue("{outdir}/rsq_rose.png"),width=12,height=14,units='cm',res=400)
par(ps=10)
par(mar=c(1,1,1,1))
par(cex=1)
print({map})
dev.off()


 #####






a=ggplot(data=master,aes(x=modtype,y=intercept))+geom_bar(aes(fill=modtype),stat="identity")+facet_wrap(~variable,scales="free")+
  scale_fill_manual("model type",values=c("linear"="red","mixed"="blue"))+theme_classic()
b=ggplot(data=master,aes(x=modtype,y=rsq_fixed))+geom_bar(aes(fill=modtype),stat="identity")+facet_wrap(~variable,scales="free")+
  scale_fill_manual("model type",values=c("linear"="red","mixed"="blue"))+theme_classic()
c=ggplot(data=master,aes(x=modtype,y=rsq_fixed_plus_random))+geom_bar(aes(fill=modtype),stat="identity")+facet_wrap(~variable,scales="free")+
  scale_fill_manual("model type",values=c("linear"="red","mixed"="blue"))+theme_classic()

png(glue("{outdir}/intercept.png"),width=20,height=20,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({a})
dev.off()

png(glue("{outdir}/rsq_fixed.png"),width=20,height=20,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({b})
dev.off()

png(glue("{outdir}/rsq_fullmodel.png"),width=20,height=20,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({c})
dev.off()

filter(master,modtype=="linear") %>% arrange(rsq_fixed)
filter(master,modtype=="mixed") %>% arrange(rsq_fixed)

## full models #####
lmm_medical_best=r.squaredGLMM(lmer(SEER_rate~rescale(incm_mh,to=c(0,100))+rescale(wpovr50,to=c(0,100))+(1|STATEFP),data=modDF))[1]
lm_medical_best=r.squaredGLMM(lm(SEER_rate~rescale(incm_mh,to=c(0,100))+rescale(wpovr50,to=c(0,100)),data=modDF))[1]

lmm_medical_all=r.squaredGLMM(lmer(SEER_rate~rescale(incm_mh,to=c(0,100))+rescale(wpovr50,to=c(0,100))+rescale(wpvr100,to=c(0,100))+rescale(docs_pk,to=c(0,100))+rescale(pcp_pk,to=c(0,100))+rescale(derm_pk,to=c(0,100))+(1|STATEFP),data=modDF))[1]
lm_medical_all=r.squaredGLMM(lm(SEER_rate~rescale(incm_mh,to=c(0,100))+rescale(wpovr50,to=c(0,100))+rescale(wpvr100,to=c(0,100))+rescale(docs_pk,to=c(0,100))+rescale(pcp_pk,to=c(0,100))+rescale(derm_pk,to=c(0,100)),data=modDF))[1]

lmm_envt_best=r.squaredGLMM(lmer(SEER_rate~rescale(UV_irradiance,to=c(0,100))+rescale(seasonality_temperature,to=c(0,100))+(1|STATEFP),data=modDF))[1]
lm_envt_best=r.squaredGLMM(lm(SEER_rate~rescale(UV_irradiance,to=c(0,100))+rescale(seasonality_temperature,to=c(0,100)),data=modDF))[1]

lmm_envt_all=r.squaredGLMM(lmer(SEER_rate~rescale(UV_irradiance,to=c(0,100))+rescale(seasonality_temperature,to=c(0,100))+rescale(sun_exposure,to=c(0,100))+rescale(seasonality_cloud,to=c(0,100))+rescale(elevation,to=c(0,100))+rescale(mean_cloud,to=c(0,100))+(1|STATEFP),data=modDF))[1]
lm_envt_all=r.squaredGLMM(lm(SEER_rate~rescale(UV_irradiance,to=c(0,100))+rescale(seasonality_temperature,to=c(0,100))+rescale(sun_exposure,to=c(0,100))+rescale(seasonality_cloud,to=c(0,100))+rescale(elevation,to=c(0,100))+rescale(mean_cloud,to=c(0,100)),data=modDF))[1]

models=c("mixed","linear","mixed","linear","mixed","linear","mixed","linear")
data=c(rep("Medial",4),rep("Environment",4))
type=c("Best_two","Best_two","All","All","Best_two","Best_two","All","All")
rsq=list(lmm_medical_best,lm_medical_best,lmm_medical_all,lm_medical_all,lmm_envt_best,lm_envt_best,lmm_envt_all,lm_envt_all) %>% unlist()

dat=data.frame(models=as.character(models),
               data=as.character(data),
               type=as.character(type),
               rsq=as.numeric(rsq),
               stringsAsFactors = F)

c=ggplot(data=dat,aes(x=models,y=rsq))+geom_bar(aes(group=data,fill=data),stat="identity",, position = "dodge")+facet_wrap(~type)+
                        scale_fill_manual("model type",values=c("Medial"="red","Environment"="blue"))+theme_classic()


png(glue("{outdir}/twoVSallVariables.png"),width=20,height=20,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({c})
dev.off()

## full models again ####
vars=colnames(modDF)[c(5:14,17:23)]
vars_envt=vars[1:10]
vars_medial=vars[11:17]

test=modDF[c(2,5:14,17:23)]
g=cor(test)
f=corrplot(g,order="hclust")
f
