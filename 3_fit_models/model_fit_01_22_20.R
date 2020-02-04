### putting it all together
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"

modDF=read.csv("/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/model_data/data_01_22_20.csv")

## quick model
master=master %>% mutate(random=sample(1:nrow(master)))
master=master[complete.cases(master),]
master=master%>% mutate(SEER_rate=as.integer(round(SEER_rate)))
gbm.x=c("docs_pk","wpovr50","wpvr100","anRange_temperature","cancer_gov_UV_exposure","mean_cloud","elevation","mean_temperature","seasonality_cloud","seasonality_temperature","sun_exposure","UV_daily_dose","UV_irradiance","incm_pc","incm_mh","derm_pk","pcp_pk","random")
family="poisson"
lr=0.001
tc=3
bf=0.6
tolerance = 0.1
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="SEER_rate",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf) #4900 trees
# name=glue("/Volumes/SeaGate/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_12_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}.rds")
# write_rds(gap_brt_poiss_step,name)
summary(gap_brt_poiss_step)
gbm.plot(gap_brt_poiss_step)
dev_eval2(gap_brt_poiss_step)

gbm.x=c("docs_pk","wpovr50","wpvr100","incm_pc","incm_mh","derm_pk","pcp_pk","random")
family="poisson"
lr=0.001
tc=3
bf=0.6
tolerance = 0.1
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="SEER_rate",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf) #4900 trees
# name=glue("/Volumes/SeaGate/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_12_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}.rds")
# write_rds(gap_brt_poiss_step,name)
summary(gap_brt_poiss_step)
gbm.plot(gap_brt_poiss_step)
dev_eval2(gap_brt_poiss_step)


gbm.x=c("anRange_temperature","cancer_gov_UV_exposure","elevation","mean_temperature","seasonality_cloud","seasonality_temperature","UV_daily_dose","random")
family="poisson"
lr=0.001
tc=3
bf=0.6
tolerance = 0.1
gap_brt_poiss_step = gbm.step(master,gbm.x=gbm.x,gbm.y="SEER_rate",family=family,learning.rate = lr, tree.complexity =tc, bag.fraction = bf) #4900 trees
# name=glue("/Volumes/SeaGate/IUU_GRW/SDMs/brt/gaps_classA_sat_fracD0.2_100nm_12hr_12_12_19_step_lr{lr}_tc{tc}_bf{bf}_tol{tolerance}_{family}.rds")
# write_rds(gap_brt_poiss_step,name)
summary(gap_brt_poiss_step)
gbm.plot(gap_brt_poiss_step)
dev_eval2(gap_brt_poiss_step)


g=master[complete.cases(master),] %>% dplyr::select(-c(Pop,COUNTY_FIPS,NAME)) %>% dplyr::select(seasonality_temperature,UV_daily_dose,elevation,incm_mh,derm_pk)

head(g)

M <- cor(g)
corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9)

gam.mod1 <- mgcv::gam(SEER_rate~s(seasonality_temperature, bs="ts")+s(UV_daily_dose, bs="ts")+s(elevation, bs="ts")+s(incm_mh, bs="ts")+ s(derm_pk, bs="ts"),family=poisson, data=master, method = "REML", select = T)
gam.mod1a <- mgcv::gam(SEER_rate~s(seasonality_temperature, bs="ts")+s(UV_daily_dose, bs="ts")+s(elevation, bs="ts")+ s(derm_pk, bs="ts"),family=poisson, data=master, method = "REML", select = T)



gam.mod2 <- mgcv::gamm(SEER_rate~s(seasonality_temperature, bs="ts")+s(UV_daily_dose, bs="ts")+s(elevation, bs="ts")+s(incm_mh, bs="ts")+ s(derm_pk, bs="ts"),family=poisson, data=master, method = "REML", select = T)
gam.mod2 <- mgcv::gamm(SEER_rate~s(UV_daily_dose, bs="ts")+s(elevation, bs="ts")+s(incm_mh, bs="ts")+ s(derm_pk, bs="ts"),family=poisson, data=master, method = "REML", select = T)

