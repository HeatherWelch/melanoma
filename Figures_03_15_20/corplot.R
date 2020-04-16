source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
master=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/master_dataframe_breast_cancer.csv") %>% dplyr::select(c(SEER_rate,cancer_gov_UV_exposure,elevation,seasonality_cloud,seasonality_temperature,UV_irradiance,incm_mh,incm_pc,wpovr50,pcp_pk,derm_pk,Pop))
colnames(master) <- make.unique(names(master))
new=master[complete.cases(master),] %>% rename("Temperature Seasonality"=seasonality_temperature) %>% 
  rename("Dermatologists"=derm_pk) %>% 
  rename("NHW melanoma incidence"=SEER_rate) %>% 
  rename("UV Daily Dose"=cancer_gov_UV_exposure) %>% 
  rename("Elevation"=elevation) %>% 
  rename("Primary care"=pcp_pk) %>% 
  rename("Cloud Seasonality"=seasonality_cloud) %>% 
  rename("UV Solar Noon"=UV_irradiance) %>% 
  rename("Median Household"=incm_mh) %>% 
  rename("Income per capita"=incm_pc) %>% 
  rename("Households >$50,000"=wpovr50)

pop=new$Pop %>% gsub(",","",.) %>% as.numeric()
new=new %>% dplyr::select(-Pop)

# unweighted ####
M <- cor(new)
map=corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9)
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_15_20";dir.create(outdir)

png(glue("{outdir}/corplot_unweighted.png"),width=15,height=15,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9)
dev.off()

write.csv(M,glue("{outdir}/corMatrix_unweighted.csv"))

# weighted wtd.cors ####
library(weights)
test=wtd.cors(new,weight = pop)
test2=test[c(2,6,3,4,5,1,8,7,9,10,11),c(2,6,3,4,5,1,8,7,9,10,11)]
map=corrplot(test2,type="upper",order="original",outline = T,tl.col="black",tl.cex = .9)

png(glue("{outdir}/corplot_weighted_wtd.cors.png"),width=15,height=15,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(test2,type="upper",order="original",outline = T,tl.col="black",tl.cex = .9)
dev.off()

write.csv(test,glue("{outdir}/corMatrix_weighted_wtd.cors.csv"))

# weighted wtd.cor ####
library(weights)
test=wtd.cor(x=new, weight=pop)
test2=test$correlation[c(2,6,3,4,5,1,8,7,9,10,11),c(2,6,3,4,5,1,8,7,9,10,11)]
map=corrplot(test2,type="upper",order="original",outline = T,tl.col="black",tl.cex = .9)

png(glue("{outdir}/corplot_weighted_wtd.cor.png"),width=15,height=15,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(test2,type="upper",order="original",outline = T,tl.col="black",tl.cex = .9)
dev.off()

write.csv(test$correlation,glue("{outdir}/corMatrix_weighted_wtd.cor.csv"))

## trying matrix math
a=M %>% round(.,2)
b=test$correlation%>% round(.,2)
math=a-b
write.csv(math,glue("{outdir}/corMatrix_weighted_minus_unweighted.csv"))

a=M %>% round(.,2) %>% abs()
b=test$correlation%>% round(.,2)%>% abs()
math=a-b
write.csv(math,glue("{outdir}/corMatrix_weighted_minus_unweighted_abs.csv"))


