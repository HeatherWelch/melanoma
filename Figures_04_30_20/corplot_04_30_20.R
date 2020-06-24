source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20"
master=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20/master_dataframe_04_30_20.csv") %>% dplyr::select(c(Melanoma_incidence,cancer_gov_UV_exposure,elevation,seasonality_cloud,seasonality_temperature,UV_irradiance,incm_mh,incm_pc,wpovr50,pcp_pk,derm_pk,Melanoma_incidence_pop))
colnames(master) <- make.unique(names(master))
new=master[complete.cases(master),] %>% rename("Temperature Variability"=seasonality_temperature) %>% 
  rename("Dermatologists"=derm_pk) %>% 
  rename("NHW melanoma incidence"=Melanoma_incidence) %>% 
  rename("UV Daily Dose"=cancer_gov_UV_exposure) %>% 
  rename("Elevation"=elevation) %>% 
  rename("Primary care"=pcp_pk) %>% 
  rename("Cloud Variability"=seasonality_cloud) %>% 
  rename("UV Solar Noon"=UV_irradiance) %>% 
  rename("Median Household"=incm_mh) %>% 
  rename("Income per capita"=incm_pc) %>% 
  rename("Households >$50,000"=wpovr50)

pop=new$Melanoma_incidence_pop %>% gsub(",","",.) %>% as.numeric()
new=new %>% dplyr::select(-Melanoma_incidence_pop)

# unweighted ####
library(RColorBrewer)
scalebluered <- colorRampPalette(brewer.pal(8, "RdBu"))(8)
M <- cor(new)
corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9,cl.pos="n")
# corrplot(M ,order="hclust",outline = T,tl.cex = .9,tl.col="black", 
#           type="upper", cl.pos="n",mar = c(5, 5, 5, 5))
# colorlegend(scalebluered, xlim=c(13,15), ylim=c(11,7),c(seq(-1,1,.5)),align="l", vertical=TRUE, addlabels=TRUE,
#             cex = 0.5)

png(glue("{outdir}/corplot_unweighted.png"),width=15,height=15,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9,cl.pos="n")
dev.off()

write.csv(M,glue("{outdir}/corMatrix_unweighted.csv"))

# weighted wtd.cors ####
library(weights)
test=wtd.cors(new,weight = pop)
x=new$wpovr50
test=wtd.cors(new,weight = x)
corrplot(test,type="upper",order="original",outline = T,tl.col="black",tl.cex = .9)
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



# full correlation matrix for sups

master=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20/master_dataframe_04_30_20.csv") 
colnames(master) <- make.unique(names(master))
new=master %>% rename("*Temperature Variability*"=seasonality_temperature) %>% 
  rename("*Dermatologists*"=derm_pk) %>% 
  rename("NHW MELANOMA INCIDENCE"=Melanoma_incidence) %>% 
  rename("*UV Daily Dose*"=cancer_gov_UV_exposure) %>% 
  rename("*Elevation*"=elevation) %>% 
  rename("*Primary care*"=pcp_pk) %>% 
  rename("*Cloud Variability*"=seasonality_cloud) %>% 
  rename("*UV Solar Noon*"=UV_irradiance) %>% 
  rename("*Median Household*"=incm_mh) %>% 
  rename("*Income per capita*"=incm_pc) %>% 
  rename("*Households >$50,000*"=wpovr50) %>% 
  rename("Population"=Melanoma_incidence_pop)%>% 
  rename("Temperature range"=anRange_temperature)%>% 
  rename("Cloud"=mean_cloud)%>% 
  rename("Temperature"=mean_temperature)%>% 
  rename("Sun exposure"=sun_exposure)%>% 
  rename("UV daily dose CDC"=UV_daily_dose)%>% 
  rename("Doctors"=docs_pk)%>% 
  rename("Households >$100,000"=wpvr100)%>% 
  rename("Health insurance < 65"="HI_65")

new=new %>% dplyr::select(-c(X,COUNTY_FIPS,STATEFP,NAME,COUNTY_,Melanoma_mortality_county_name,Melanoma_mortality_count,Melanoma_mortality_pop,CTYNAME,hisp_metric_2010_all_ages,hisp_metric_2010_over40,county,Breast_cancer_NHWinsitu,
                             Breast_cancer_NHWinvasive,Breast_cancer_NHWfemalePop,County,Melanoma_incidence_In_situ,Melanoma_incidence_Invasive,State.x,Lung_cancer_incidence,Lung_cancer_incidence_count,Lung_cancer_incidence_pop,State.y,Lung_cancer_mortality,
                             County,Lung_cancer_mortality_count,Lung_cancer_mortality_pop,Melanoma_mortality)) %>% 
  .[complete.cases(.),]

pop=new$Population %>% gsub(",","",.) %>% as.numeric()
new2=new %>% dplyr::select(-Population)

M <- cor(new2)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",col=col(200),
         shade.col=NA,addCoef.col="black",tl.cex = .9,cl.pos="n",method="shade")

png(glue("{outdir}/corplot_unweighted_supps.png"),width=20,height=20,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",col=col(200),
         shade.col=NA,addCoef.col="black",tl.cex = .9,cl.pos="n",method="shade",number.cex=.8)

dev.off()

## supps weighted ####

library(weights)
test=wtd.cors(new2,weight = pop)
corrplot(test,type="upper",order="original",outline = T,tl.col="black",tl.cex = .9)
test2=test[c(6,10,11,3,9,4,19,2,8,5,7,1,12,13,17,18,15,14,16),c(6,10,11,3,9,4,19,2,8,5,7,1,12,13,17,18,15,14,16)]
corrplot(test2,type="upper",order="hclust",outline = T,tl.col="black",col=col(200),
         shade.col=NA,addCoef.col="black",tl.cex = .9,cl.pos="n",method="shade",number.cex=.8)

labels=c("black","black","blue","blue",rep("black",4),rep("blue",7),"black",rep("blue",2),"black")
png(glue("{outdir}/corplot_weighted_supps.png"),width=20,height=20,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(test2,type="upper",order="original",outline = T,tl.col=labels,col=col(200),
         shade.col=NA,addCoef.col="black",tl.cex = .9,cl.pos="n",method="shade",number.cex=.8)

dev.off()

diff=M-test2
corrplot(diff,type="upper",order="hclust",outline = T,tl.col="black",col=col(200),
         shade.col=NA,addCoef.col="black",tl.cex = .9,cl.pos="n",method="shade",number.cex=.8)


## testing some other stuff related to weighting ####
studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
studyarea=studyarea %>% mutate(COUNTY_FIPS=glue("{STATEFP}{COUNTYFP}")) %>% mutate(COUNTY_FIPS=as.integer(COUNTY_FIPS))
# centroid=st_centroid(studyarea)

income=new$`*Households >$50,000*` %>% as.numeric()
pop=new$Population %>% gsub(",","",.) %>% as.numeric()

test=wtd.cor(new,weight = pop,collapse=FALSE)
corrplot(test,type="upper",order="original",outline = T,tl.col="black",tl.cex = .9)

png(glue("{outdir}/corplot_weighted_supps_pop.png"),width=20,height=20,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(test,type="upper",order="original",outline = T,tl.col=labels,col=col(200),
         shade.col=NA,addCoef.col="black",tl.cex = .9,cl.pos="n",method="shade",number.cex=.8)
dev.off()

test=wtd.cors(new,weight = income)
corrplot(test,type="upper",order="original",outline = T,tl.col="black",tl.cex = .9)

png(glue("{outdir}/corplot_weighted_supps_income.png"),width=20,height=20,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(test,type="upper",order="original",outline = T,tl.col=labels,col=col(200),
         shade.col=NA,addCoef.col="black",tl.cex = .9,cl.pos="n",method="shade",number.cex=.8)
dev.off()

M <- cor(new)
png(glue("{outdir}/corplot_weighted_supps_noweight.png"),width=20,height=20,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(M,type="upper",order="original",outline = T,tl.col=labels,col=col(200),
         shade.col=NA,addCoef.col="black",tl.cex = .9,cl.pos="n",method="shade",number.cex=.8)
dev.off()

library(ggExtra)
master=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20/master_dataframe_04_30_20.csv") 
colnames(master) <- make.unique(names(master))
new=master %>% rename("*Temperature Variability*"=seasonality_temperature) %>% 
  rename("*Dermatologists*"=derm_pk) %>% 
  rename("NHW MELANOMA INCIDENCE"=Melanoma_incidence) %>% 
  rename("*UV Daily Dose*"=cancer_gov_UV_exposure) %>% 
  rename("*Elevation*"=elevation) %>% 
  rename("*Primary care*"=pcp_pk) %>% 
  rename("*Cloud Variability*"=seasonality_cloud) %>% 
  rename("*UV Solar Noon*"=UV_irradiance) %>% 
  rename("*Median Household*"=incm_mh) %>% 
  rename("*Income per capita*"=incm_pc) %>% 
  rename("*Households >$50,000*"=wpovr50) %>% 
  rename("Population"=Melanoma_incidence_pop)%>% 
  rename("Temperature range"=anRange_temperature)%>% 
  rename("Cloud"=mean_cloud)%>% 
  rename("Temperature"=mean_temperature)%>% 
  rename("Sun exposure"=sun_exposure)%>% 
  rename("UV daily dose CDC"=UV_daily_dose)%>% 
  rename("Doctors"=docs_pk)%>% 
  rename("Households >$100,000"=wpvr100)%>% 
  rename("Health insurance < 65"="HI_65")

new=new %>% dplyr::select(-c(X,NAME,COUNTY_,Melanoma_mortality_county_name,Melanoma_mortality_count,Melanoma_mortality_pop,CTYNAME,hisp_metric_2010_all_ages,hisp_metric_2010_over40,county,Breast_cancer_NHWinsitu,
                             Breast_cancer_NHWinvasive,Breast_cancer_NHWfemalePop,County,Melanoma_incidence_In_situ,Melanoma_incidence_Invasive,State.x,Lung_cancer_incidence,Lung_cancer_incidence_count,Lung_cancer_incidence_pop,Lung_cancer_mortality,
                             County,Lung_cancer_mortality_count,Lung_cancer_mortality_pop,Melanoma_mortality)) %>% 
  .[complete.cases(.),]

colors=c("#4f3071",
          "#71b245",
          "#6f4acb",
          "#c1953b",
          "#ca4abf",
          "#59b298",
          "#c74d2e",
          "#688db6",
          "#c4426d",
          "#485b33",
          "#b683c2",
          "#a9a381",
          "#4e2d33",
          "#be7c74",
         "black")

new=new %>% mutate(state=strtrim(State.y,4)) %>% mutate(location=case_when(STATEFP=="35" ~ "New Mexico",
                                                                           STATEFP=="36" ~ "Eastern seaboard",
                                                                           STATEFP=="25" ~ "Eastern seaboard",
                                                                           STATEFP=="9" ~ "Eastern seaboard",
                                                                           STATEFP=="34" ~ "Eastern seaboard",
                                                                           # STATEFP=="22" ~ "Eastern seaboard",
                                                                           STATEFP=="13" ~ "Georgia",
                                                                           STATEFP=="6" ~ "California",
                                                                           STATEFP=="49" ~ "Utah",
                                                                           STATEFP=="22" ~ "Lousiana",
                                                                           STATEFP=="16" ~ "Idaho",
                                                                           STATEFP=="IA" ~ "Indiana",
                                                                           STATEFP=="26" ~ "Michigan",
                                                                           STATEFP=="53" ~ "Washington",
                                                                           STATEFP=="21" ~ "Kentucky",
                                                                           STATEFP=="19" ~ "Iowa",
                                                                           
                                                                           TRUE ~ "Other"))

fm2=lm(as.formula(glue("`NHW MELANOMA INCIDENCE` ~ `*Households >$50,000*`")),data=new)
# fm4 <- lmer(as.formula(glue("`NHW MELANOMA INCIDENCE` ~ `*Households >$50,000*`+(1|Population)")), data = new)
fm4 <- rlm(new$`NHW MELANOMA INCIDENCE` ~ new$`*Households >$50,000*`,data = new,weights=Population)

# get coefficients
lm_summary=summary(fm2)
lmm_summary=summary(fm4)

lm_int=lm_summary$coefficients[1]
lm_slope=lm_summary$coefficients[2]

lmm_int=lmm_summary$coefficients[1]
lmm_slope=lmm_summary$coefficients[2]

a=ggplot()+geom_point(data=new,aes(x=`*Households >$50,000*`,y=`NHW MELANOMA INCIDENCE`,size=as.numeric(Population),alpha=as.numeric(Population),color=as.factor(location)))+
  scale_color_manual("State",values=c("California"="blue",
                                      "Eastern seaboard"="red",
                                      "Kentucky"="yellow",
                                      "Utah"="lightgrey",
                                      "Lousiana"="darkgrey",
                                      "Idaho"="lightblue",
                                      "Indiana"="darkgreen",
                                      "Michigan"="orange",
                                      "Washington"="mediumaquamarine",
                                      "New Mexico"="maroon",
                                      "Iowa"="black",
                                      "Georgia"="lightcyan2"
                                      ))+
  geom_abline(slope=lm_slope,intercept = lm_int,color="red",size=1)+geom_abline(slope=lmm_slope,intercept = lmm_int,color="blue")+
  scale_size(range = c(1,10),breaks=pretty(new$Population,5),labels = pretty(new$Population,5))
a

new1=new %>% arrange(Population) %>% mutate(weight=Population/(sum(Population))) %>% filter(Population>52000)

#%>% filter(COUNTY_FIPS!=6037)
fm2a=lm(as.formula(glue("`NHW MELANOMA INCIDENCE` ~ `*UV Daily Dose*`")),data=new1)
# fm4a <- lmer(as.formula(glue("`NHW MELANOMA INCIDENCE` ~ `*UV Daily Dose*`+(1|Population)")), data = new)
fm4a <- rlm(new1$`NHW MELANOMA INCIDENCE` ~ new1$`*UV Daily Dose*`,data = new1,weights=Population)

# get coefficients
lm_summarya=summary(fm2a)
lmm_summarya=summary(fm4a)

lm_inta=lm_summarya$coefficients[1]
lm_slopea=lm_summarya$coefficients[2]

lmm_inta=lmm_summarya$coefficients[1]
lmm_slopea=lmm_summarya$coefficients[2]

new1=new1 %>% mutate(Population=as.numeric(Population)) 
new1=new %>% arrange(desc(Population))%>% filter(COUNTY_FIPS!=6037)  %>% filter(location=="Eastern seaboard"||location=="California")
dd=new1 %>% dplyr::select(-c(COUNTY_FIPS,STATEFP,State.y,state,location))
weight=dd$Population
wtd.cors(dd,weight=weight)

b=ggplot()+geom_point(data=new1,aes(x=`*UV Daily Dose*`,y=`NHW MELANOMA INCIDENCE`,size=as.numeric(Population),alpha=as.numeric(Population),color=as.factor(location)))+
  scale_color_manual("State",values=c("California"="blue",
                                      "Eastern seaboard"="red",
                                      "Kentucky"="yellow",
                                      "Utah"="lightgrey",
                                      "Lousiana"="darkgrey",
                                      "Idaho"="lightblue",
                                      "Indiana"="darkgreen",
                                      "Michigan"="orange",
                                      "Washington"="mediumaquamarine",
                                      "New Mexico"="maroon",
                                      "Iowa"="black",
                                      "Georgia"="lightcyan2"))+
  geom_abline(slope=lm_slopea,intercept = lm_inta,color="red",size=1)+geom_abline(slope=lmm_slopea,intercept = lmm_inta,color="blue")+
   scale_size(range = c(1,10),breaks=pretty(new$Population,5),labels = pretty(new$Population,5))+
  geom_count()
# # ggMarginal(b,type = "histogram")
#   
# xhist <- 
#   axis_canvas(b, axis = "x") + 
#   geom_histogram(data = new, aes(x = `*UV Daily Dose*`), color = 'lightgray')
# yhist <-
#   axis_canvas(p, axis = "y", coord_flip = TRUE) + 
#   geom_histogram(data = new, aes(x = `NHW MELANOMA INCIDENCE`), color = 'lightgray',adjust=.5) +
#   coord_flip()
# b%>%
#   #insert_xaxis_grob(xhist, grid::unit(1, "in"), position = "top") %>%
#    insert_yaxis_grob(yhist, grid::unit(3, "in"), position = "right") %>%
#   ggdraw()

trial=left_join(new,studyarea,by="COUNTY_FIPS") %>% mutate(lon=as.numeric(substring(INTPTLON, 2)))
ggplot(trial)+geom_density(aes(x=lon,fill=Population,stat(count)))

ggplot(new)+geom_boxplot(aes(x=location,y=`NHW MELANOMA INCIDENCE`))
ggplot(new)+geom_boxplot(aes(x=location,y=`*UV Daily Dose*`))
ggplot(new)+geom_boxplot(aes(x=location,y=`Population`))
ggplot(new)+geom_boxplot(aes(x=location,y=`*Households >$50,000*`))

png(glue("{outdir}/weighting.png"),width=40,height=15,units='cm',res=400)
par(ps=10)
#par(mar=c(1,1,1,1))
par(cex=1)
plot_grid(a,b, ncol=2, align="h",axis = "bt")
dev.off()

