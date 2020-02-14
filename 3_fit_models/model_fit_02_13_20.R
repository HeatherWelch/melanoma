source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
# library(gam)
library(mgcv)
library(lme4)
library(MuMIn)
modDF=read.csv("/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/model_data/data_01_30_20_withpop_FIPS_STATEFP.csv")

empty=list()

for(i in 5:ncol(modDF)){
  var=names(modDF)[i]
  print(var)
  # build models
  fm2=lm(as.formula(glue("SEER_rate ~ {var}")),data=modDF)
  fm4 <- lmer(as.formula(glue("SEER_rate ~ {var}+(1|STATEFP)")), data = modDF)
 
   # get coefficients
  lm_summary=summary(fm2)
  lmm_summary=summary(fm4)
  
  lm_int=lm_summary$coefficients[1]
  lm_slope=lm_summary$coefficients[2]
  
  lmm_int=lmm_summary$coefficients[1]
  lmm_slope=lmm_summary$coefficients[2]
  
  # get rsqured
  lm_rsq_fixed=r.squaredGLMM(fm2)[1]
  lmm_rsq_fixed=r.squaredGLMM(fm4)[1]
  lm_rsq_random=r.squaredGLMM(fm2)[2]
  lmm_rsq_random=r.squaredGLMM(fm4)[2]
  
  dat_lm=data.frame(variable=as.character(var),
                 slope=as.numeric(lm_slope),
                 intercept=as.numeric(lm_int),
                 rsq_fixed=as.numeric(lm_rsq_fixed),
                 modtype=as.character("linear"),
                 rsq_fixed_plus_random=as.numeric(lm_rsq_random),
                 stringsAsFactors = F)
  dat_lmm=data.frame(variable=as.character(var),
                    slope=as.numeric(lmm_slope),
                    intercept=as.numeric(lmm_int),
                    rsq_fixed=as.numeric(lmm_rsq_fixed),
                    modtype=as.character("mixed"),
                    rsq_fixed_plus_random=as.numeric(lmm_rsq_random),
                    stringsAsFactors = F)
  dat=rbind(dat_lm,dat_lmm)
  
  empty[[length(empty)+1]]<-dat
  
  }


master=do.call(rbind,empty) %>% filter(variable!="STATEFP" &  variable!="NAME")
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_02_13_20"

for(i in 1:length(unique(master$variable))){

map.var=unique(master$v)[i]
dat=master %>% filter(variable==map.var)
plott=ggplot(modDF,aes(x=.data[[map.var]],y=SEER_rate))+geom_point()+geom_abline(slope=dat[1,2],intercept = dat[1,3],color="red")+geom_abline(slope=dat[2,2],intercept = dat[2,3],color="blue")+
  xlab(map.var)

png(glue("{outdir}/{map.var}.png"),width=20,height=20,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({plott})
dev.off()
}

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
