## preparing county medical datasource('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
library(sf)
studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
studyarea=studyarea %>% mutate(COUNTY_FIPS=glue("{STATEFP}{COUNTYFP}"))
us_ext=c(-125.0011, -66.9326, 24.9493,49.5904)
sa=studyarea %>% mutate(COUNTY_FIPS=as.integer(COUNTY_FIPS))

spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"

# indidence data ####
# rate=read.csv("/Users/heatherwelch/Dropbox/melenoma/medical/Melanoma_2012-16.csv")
# 
# variable="SEER_rate"
# x.var <- rlang::sym(variable)
# sa=studyarea %>% mutate(COUNTY_FIPS=as.integer(COUNTY_FIPS))
# b=left_join(sa,rate)
# b=b %>% mutate(Rate=as.numeric(Rate))
# 
# # b=crop(a,us_ext)
# 
# ## generic
# # c=studyarea %>% mutate(!!variable := raster::extract(b,.,fun=mean,na.rm=T))
# # d=as.data.frame(c) %>% dplyr::select(!!variable,COUNTY_FIPS)
# d=as.data.frame(b) %>% dplyr::select(Rate,Pop,COUNTY_FIPS) %>% rename(SEER_rate=Rate)
# 
# 
# # st_write(c, glue("{spatial_dir}/{variable}.shp"))
# write.csv(d,glue("{spatial_dir}/{variable}.csv"))
# # writeRaster(b,glue("{spatial_dir}/{variable}.grd"),overwrite=T)
# 
# map=ggplot()+geom_sf(data=b,aes(fill = Rate),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
#   ggtitle(variable)+
#   scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))
# 
# 
# png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# print({map})
# dev.off()

# health resource ####
#http://asdfree.com/area-health-resource-file-ahrf.html
#https://www.marsja.se/how-to-import-data-reading-sas-files-in-r/
library(haven)
a=read_sas("/Users/heatherwelch/Dropbox/melenoma/medical/AHRF_2018-2019/AHRF_2018-2019_SAS/ahrf2019.sas7bdat")
# dermo=a %>% dplyr::select(f00002,f00012,f1108717,f1108715,f1108710)
# pcp=a %>% dplyr::select(f00002,f00012,f1467516,f1467515,f1467514,f1467513,f1467512)
# docs=a %>% dplyr::select(f00002,f00012,f1468116,f1468115,f1468114,f1468113,f1468112)
# pop=a %>% dplyr::select(f00002,f00012,f1198416,f1198415,f1198414,f1198413,f1198412)
# income_pc=a %>% dplyr::select(f00002,f00012,f0978116,f0978115,f0978114,f0978113,f0978112)
# income_mh=a %>% dplyr::select(f00002,f00012,f1434513 ,f1434511)

# dermo=a %>% dplyr::select(f1108717,f1108715,f1108710) %>% rowMeans()
dermo=a %>% dplyr::select(f1108715) %>% rowMeans() ## just doing 2015
pcp=a %>% dplyr::select(f1467516,f1467515,f1467514,f1467513,f1467512)%>% rowMeans()
docs=a %>% dplyr::select(f1468116,f1468115,f1468114,f1468113,f1468112)%>% rowMeans()
pop=a %>% dplyr::select(f1198416,f1198415,f1198414,f1198413,f1198412)%>% rowMeans()
pop2015=a %>% dplyr::select(f1198415)%>% rowMeans()
income_pc=a %>% dplyr::select(f0978116,f0978115,f0978114,f0978113,f0978112)%>% rowMeans()
income_mh=a %>% dplyr::select(f1434513 ,f1434511)%>% rowMeans()
# income_NHW_13_17=a%>%dplyr::select(f1439513,f1439613,f1439713,f1439813,f1439913,f1440013)%>%rowMeans()
# income_NHW_11_15=a%>%dplyr::select(f1439511,f1439611,f1439711,f1439811,f1439911,f1440011)%>%rowMeans()
# income_NHW=as.data.frame(income_NHW_11_15) %>% mutate(income_NHW_13_17=income_NHW_13_17) %>% mutate(summ=income_NHW_13_17+income_NHW_11_15)

incomeNHW0=a %>% dplyr::select(f1439513,f1439511)%>% rowMeans() #Wh non/Hisp HHld Inc < $10,000
incomeNHW1=a %>% dplyr::select(f1439613,f1439611)%>% rowMeans() #Wh n/His HHd Inc$10,000-14,999
incomeNHW2=a %>% dplyr::select(f1439713,f1439711)%>% rowMeans() #Wh n/His HHd Inc$15,000-24,999
incomeNHW3=a %>% dplyr::select(f1439813,f1439811)%>% rowMeans() #Wh n/His HHd Inc$25,000-49,999
incomeNHW4=a %>% dplyr::select(f1439913,f1439911)%>% rowMeans() #Wh n/His HHd Inc$50,000-99,999
incomeNHW5=a %>% dplyr::select(f1440013,f1440011)%>% rowMeans() #Wh non/Hisp HHld Inc $100,000+
income=as.data.frame(incomeNHW0) 
income=income %>% mutate(incomeNHW1=incomeNHW1) %>% mutate(incomeNHW2=incomeNHW2) %>% mutate(incomeNHW3=incomeNHW3) %>% mutate(incomeNHW4=incomeNHW4) %>% mutate(incomeNHW5=incomeNHW5) %>% mutate(total=rowSums(.))
income=income %>% mutate(over50=incomeNHW4+incomeNHW5) %>% mutate(wpover50=over50/total*100) %>% mutate(wpover100=incomeNHW5/total*100)
HI_65=a %>% dplyr::select(f1547416,f1547415,f1547414,f1547413,f1475112)%>% rowMeans() 
HI_65=100-HI_65

master=a %>% dplyr::select(f00002,f00012,f00011,f00010) %>% mutate(dermo=dermo) %>% mutate(pcp=pcp) %>% mutate(docs=docs) %>% mutate(pop=pop) %>% mutate(income_pc=income_pc) %>% mutate(income_mh=income_mh) %>% 
  mutate(wpover50=income$wpover50) %>% mutate(wpover100=income$wpover100) %>% mutate(HI_65=HI_65)
master=master %>% mutate(dermo_pk=dermo*100000/pop2015)
master=master %>% mutate(pcp_pk=pcp*100000/pop)
master=master %>% mutate(docs_pk=docs*100000/pop)
master=master %>% mutate(COUNTY_FIPS=as.integer(f00002))
b=left_join(sa,master)
# st_write(b, glue("{spatial_dir}/AHRF.shp"))
# st_write(b, glue("{spatial_dir}/AHRF_new.shp"))
# st_write(b, glue("{spatial_dir}/AHRF_new_01_30_20.shp"))
st_write(b, glue("{spatial_dir}/AHRF_new_12_02_21.shp"))

# health resource income per capita ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"
b=st_read(glue("{spatial_dir}/AHRF.shp"))
variable="incm_pc"
x.var <- rlang::sym(variable)

map=ggplot()+geom_sf(data=b,aes(fill = ntile(!!x.var,100)),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()

# health resource median household income ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"
b=st_read(glue("{spatial_dir}/AHRF.shp"))
variable="incm_mh"
x.var <- rlang::sym(variable)

map=ggplot()+geom_sf(data=b,aes(fill = ntile(!!x.var,100)),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}_2.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()

# health resource derms per 100,000 people ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"
b=st_read(glue("{spatial_dir}/AHRF.shp"))
variable="derm_pk"
x.var <- rlang::sym(variable)

map=ggplot()+geom_sf(data=b,aes(fill = ntile(!!x.var,100)),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()

# health resource pcps per 100,000 people ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"
b=st_read(glue("{spatial_dir}/AHRF.shp"))
variable="pcp_pk"
x.var <- rlang::sym(variable)

map=ggplot()+geom_sf(data=b,aes(fill = ntile(!!x.var,100)),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()

# NHW households with income >50 ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"
b=st_read(glue("{spatial_dir}/AHRF_new.shp"))
variable="wpovr50"
x.var <- rlang::sym(variable)

map=ggplot()+geom_sf(data=b,aes(fill = ntile(!!x.var,100)),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()

# NHW households with income >100 ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"
b=st_read(glue("{spatial_dir}/AHRF_new.shp"))
variable="wpvr100"
x.var <- rlang::sym(variable)

map=ggplot()+geom_sf(data=b,aes(fill = ntile(!!x.var,100)),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()

# health resource docs per 100,000 people ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"
b=st_read(glue("{spatial_dir}/AHRF.shp"))
variable="docs_pk"
x.var <- rlang::sym(variable)

map=ggplot()+geom_sf(data=b,aes(fill = ntile(!!x.var,100)),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()

            
            
            

# % with health insurance (<age65) ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"
b=st_read(glue("{spatial_dir}/AHRF_new_01_30_20.shp"))
variable="HI_65"
x.var <- rlang::sym(variable)

map=ggplot()+geom_sf(data=b,aes(fill = ntile(!!x.var,100)),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()







