source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
master=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/master_dataframe.csv") %>% dplyr::select(c(SEER_rate,cancer_gov_UV_exposure,elevation,seasonality_cloud,seasonality_temperature,UV_irradiance,incm_mh,incm_pc,wpovr50,pcp_pk,derm_pk))
colnames(master) <- make.unique(names(master))
new=master[complete.cases(master),] %>% rename("Seasonality of temperature"=seasonality_temperature) %>% 
  rename("Dermatologists"=derm_pk) %>% 
  rename("NHW melanoma incidence"=SEER_rate) %>% 
  rename("UV exposure"=cancer_gov_UV_exposure) %>% 
  rename("Elevation"=elevation) %>% 
  rename("Primary care"=pcp_pk) %>% 
  rename("Seasonality of cloud"=seasonality_cloud) %>% 
  rename("UV irradiance"=UV_irradiance) %>% 
  rename("Median household income"=incm_mh) %>% 
  rename("Primary care"=incm_pc) 
colnames(new)[9]='Households >$50,000'


M <- cor(new)
map=corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9)
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20"

png(glue("{outdir}/corplot.png"),width=15,height=15,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9)
dev.off()

