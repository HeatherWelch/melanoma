## ANNUAL AVERAGE SUNLIGHT EXPOSURE MEASURED BY SOLAR IRRADIANCE (KJ/M2)  (sun_exposure) ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')

studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
studyarea=studyarea %>% mutate(COUNTY_FIPS=as.character(glue("{STATEFP}{COUNTYFP}")))%>% dplyr::select(-FUNCSTAT) %>% group_by(COUNTY_FIPS)
us_ext=c(-125.0011, -66.9326, 24.9493,49.5904)

spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"

variable="sun_exposure"
x.var <- rlang::sym(variable)
a=read.csv("/Users/heatherwelch/Dropbox/melenoma/environment/raw/sunlight_exposure_solar_irradiance_kjm2/data_132939.csv")
a=a%>% mutate(countyFIPS=str_pad(countyFIPS,5,side="left",pad=0)) %>% mutate(COUNTY_FIPS=as.character(countyFIPS))%>%
  filter(Year<2012) %>% group_by(COUNTY_FIPS) %>% mutate(Value=gsub(",","",Value)) %>% summarize(Value=mean(as.numeric(Value)))
b=left_join(studyarea,a) %>% mutate(Value=as.integer(Value))

d=as.data.frame(b) %>% dplyr::select(Value,COUNTY_FIPS) %>% rename(!!variable :=Value)

st_write(b, glue("{spatial_dir}/{variable}_2012.shp"))
write.csv(d,glue("{spatial_dir}/{variable}_2012.csv"))

# map=ggplot()+geom_sf(data=b,aes(fill = Value),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
#   ggtitle(variable)+
#   scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


# png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# print({map})
# dev.off()

## Population-Weighted Ultraviolet Irradiance (erythemally weighted daily dose)  (UV_daily_dose) ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')

studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
studyarea=studyarea %>% mutate(COUNTY_FIPS=as.character(glue("{STATEFP}{COUNTYFP}")))%>% dplyr::select(-FUNCSTAT) %>% group_by(COUNTY_FIPS) 
us_ext=c(-125.0011, -66.9326, 24.9493,49.5904)

spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"

variable="UV_daily_dose"
x.var <- rlang::sym(variable)
a=read.csv("/Users/heatherwelch/Dropbox/melenoma/environment/raw/Daily_dose_UV_irradiance_jm2/data_132536.csv")
a=a%>% mutate(countyFIPS=str_pad(countyFIPS,5,side="left",pad=0)) %>% mutate(COUNTY_FIPS=as.character(countyFIPS))%>%
  filter(Year<2012) %>% group_by(COUNTY_FIPS) %>% mutate(Value=gsub(",","",Value)) %>% summarize(Value=mean(as.numeric(Value)))
b=left_join(studyarea,a) %>% mutate(Value=as.integer(Value))

d=as.data.frame(b) %>% dplyr::select(Value,COUNTY_FIPS) %>% rename(!!variable :=Value)


st_write(b, glue("{spatial_dir}/{variable}_2012.shp"))
write.csv(d,glue("{spatial_dir}/{variable}_2012.csv"))

# map=ggplot()+geom_sf(data=b,aes(fill = Value),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
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

## Population-Weighted Ultraviolet Irradiance (erythemally weighted irradiance at local solar noon time) (UV_irradiance) ####
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')

studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
studyarea=studyarea %>% mutate(COUNTY_FIPS=as.character(glue("{STATEFP}{COUNTYFP}")))%>% dplyr::select(-FUNCSTAT) %>% group_by(COUNTY_FIPS) 
us_ext=c(-125.0011, -66.9326, 24.9493,49.5904)

spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"

variable="UV_irradiance"
x.var <- rlang::sym(variable)
a=read.csv("/Users/heatherwelch/Dropbox/melenoma/environment/raw/UV_irradiance_wmm2_2/data_163016.csv")
a=a%>% mutate(countyFIPS=str_pad(countyFIPS,5,side="left",pad=0)) %>% mutate(COUNTY_FIPS=as.character(countyFIPS))%>%
  filter(Year<2012) %>% group_by(COUNTY_FIPS) %>% mutate(Value=gsub(",","",Value)) %>% summarize(Value=mean(as.numeric(Value)))
b=left_join(studyarea,a) %>% mutate(Value=as.integer(Value))

d=as.data.frame(b) %>% dplyr::select(Value,COUNTY_FIPS) %>% rename(!!variable :=Value)


st_write(b, glue("{spatial_dir}/{variable}_2012.shp"))
write.csv(d,glue("{spatial_dir}/{variable}_2012.csv"))

# map=ggplot()+geom_sf(data=b,aes(fill = Value),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
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


