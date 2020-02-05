### predicting 4 wigglinesses of GAMs predicted in model_fit_02_03_20.Rmd

master=read.csv("/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/model_data/model_predict_data_02_04_20.csv")

source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')

studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
studyarea=studyarea %>% mutate(COUNTY_FIPS=as.integer(glue("{STATEFP}{COUNTYFP}")))

outdir="/Users/heatherwelch/Dropbox/melenoma/plots_02_04_20"
# dat=left_join(master,studyarea) %>% st_sf()
dat=left_join(studyarea,master) 

mappfunction=function(dat,mapVar){
  map=ggplot()+geom_sf(data=dat,aes(fill = ntile(.data[[mapVar]],100)),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    ggtitle(mapVar)+
    scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))
  # ,limits=c(10,136.90)
  # dat2=as.data.frame(dat)
  # map=ggplot(dat2,aes(x=SEER_rate,y=.data[[mapVar]]))+geom_point()
  
  png(glue("{outdir}/{mapVar}.png"),width=36,height=22,type = "cairo",units='cm',res=400)
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  print({map})
  dev.off()
}

 # mappfunction(dat=dat,mapVar = "model4M")

library(foreach)
library(doParallel, quietly = TRUE)
registerDoParallel(5)

predictions=c("model1E","model2E","model3E","model4E","model1M","model2M","model3M","model4M","SEER_rate")
system.time(print(
  foreach(i=1:length(predictions),.export = c("mappfunction","outdir","predictions","dat"),.packages = c("ggplot2","glue"),.verbose=T) %dopar% {
    mappfunction(predictions[i],dat=dat)
  }
))

dat=left_join(studyarea,master) 
new=dat %>% mutate(model1Er=SEER_rate-model1E)%>% mutate(model2Er=SEER_rate-model2E)%>% mutate(model3Er=SEER_rate-model3E)%>% mutate(model4Er=SEER_rate-model4E) %>% 
  mutate(model1Mr=SEER_rate-model1M)%>% mutate(model2Mr=SEER_rate-model2M)%>% mutate(model3Mr=SEER_rate-model3M)%>% mutate(model4Mr=SEER_rate-model4M) 
predictions=c("model1Er","model2Er","model3Er","model4Er","model1Mr","model2Mr","model3Mr","model4Mr")

mappfunction=function(dat,mapVar){
  map=ggplot()+geom_sf(data=dat,aes(fill = .data[[mapVar]]),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
    ggtitle(mapVar)+
    scale_fill_gradient2(low = "red", mid = "white",
                         high = "blue", midpoint = 0,na.value="black",limits=c(-57,82.5))+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))
  # ,limits=c(10,136.90)
  # dat2=as.data.frame(dat)
  # map=ggplot(dat2,aes(x=SEER_rate,y=.data[[mapVar]]))+geom_point()
  
  png(glue("{outdir}/{mapVar}.png"),width=36,height=22,type = "cairo",units='cm',res=400)
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  print({map})
  dev.off()
}

system.time(print(
  foreach(i=1:length(predictions),.export = c("mappfunction","outdir","predictions","dat"),.packages = c("ggplot2","glue"),.verbose=T) %dopar% {
    mappfunction(predictions[i],dat=new)
  }
))

# useful fixed effects links
#https://www.jmp.com/support/help/14/mixed-models-and-random-effect-models.shtml
#https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html
#https://rpubs.com/rslbliss/fixed_effects
#https://dss.princeton.edu/training/Panel101.pdf

dat=left_join(master,studyarea)
a4=mgcv::gam(SEER_rate~+incm_pc+wpovr50+incm_mh+derm_pk+pcp_pk+docs_pk+wpvr100+STATEFP-1,data=dat,family=gaussian) # fixe
a4=mgcv::gam(SEER_rate~+incm_pc+wpovr50+incm_mh+derm_pk+pcp_pk+docs_pk+wpvr100+(1|STATEFP),data=dat,family=gaussian)

M <- a4$fitted
library(car)
scatterplot(yhat~dat$UV_daily_dose|dat$STATEFP, boxplots=FALSE, xlab="UV_daily_dose", ylab="Incidence",smooth=FALSE)
abline(lm(dat$SEER_rate~dat$UV_daily_dose),lwd=3, col="red")

dat=left_join(master,studyarea) %>% filter(STATEFP=="06")
a4=mgcv::gam(SEER_rate~+incm_pc+wpovr50+incm_mh+derm_pk+pcp_pk+docs_pk+wpvr100,data=dat,family=gaussian)
a4=mgcv::gam(SEER_rate~+anRange_temperature+cancer_gov_UV_exposure+mean_cloud+elevation+mean_temperature+seasonality_cloud+seasonality_temperature+sun_exposure+UV_daily_dose+UV_irradiance,data=dat,family=gaussian)
