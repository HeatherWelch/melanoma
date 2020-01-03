## preparing county rasters

source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')

studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
studyarea=studyarea %>% mutate(COUNTY_FIPS=glue("{STATEFP}{COUNTYFP}"))
us_ext=c(-125.0011, -66.9326, 24.9493,49.5904)

spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"

#1. Mean annual cloud cover ####
# Source: https://www.earthenv.org/cloud
# savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/MODCF_meanannual.tif
# other:  Valid values range from 0-10,000 and need to be multiplied by 0.01 to result in % cloudy days. Values greater than 10,000 are used for fill.
variable="mean_cloud"
x.var <- rlang::sym(variable)
a=raster("/Users/heatherwelch/Dropbox/melenoma/environment/raw/MODCF_meanannual.tif")

b=crop(a,us_ext)
b[values(b)>10000]=NA
b=b*0.01

## generic
c=studyarea %>% mutate(!!variable := raster::extract(b,.,fun=mean,na.rm=T))
d=as.data.frame(c) %>% dplyr::select(!!variable,COUNTY_FIPS)

st_write(c, glue("{spatial_dir}/{variable}.shp"))
write.csv(d,glue("{spatial_dir}/{variable}.csv"))
writeRaster(b,glue("{spatial_dir}/{variable}.grd"),overwrite=T)

map=ggplot()+geom_sf(data=c,aes(fill = !!x.var),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()


#2. Within year seasonality of cloud cover ####
#Source: https://www.earthenv.org/cloud
# savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/MODCF_intraannualSD.tif
# other:   Values need to be multiplied by 0.01 to recover SD.
variable="seasonality_cloud"
x.var <- rlang::sym(variable)
a=raster("/Users/heatherwelch/Dropbox/melenoma/environment/raw/MODCF_intraannualSD.tif")

b=crop(a,us_ext)
b=b*0.01

## generic
c=studyarea %>% mutate(!!variable := raster::extract(b,.,fun=mean,na.rm=T))
d=as.data.frame(c) %>% dplyr::select(!!variable,COUNTY_FIPS)

st_write(c, glue("{spatial_dir}/{variable}.shp"))
write.csv(d,glue("{spatial_dir}/{variable}.csv"))
writeRaster(b,glue("{spatial_dir}/{variable}.grd"),overwrite=T)

map=ggplot()+geom_sf(data=c,aes(fill = !!x.var),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()

#3. Mean temperature ####
#Source: http://chelsa-climate.org/downloads/
# savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/CHELSA_temp10_01_1979-2013_V1.2_land.tif (jan only, n=12)
# other:   units are degrees C/10
variable="mean_temperature"
x.var <- rlang::sym(variable)
a=list.files("/Users/heatherwelch/Dropbox/melenoma/environment/raw/",pattern = "CHELSA_temp10_",full.names = T) %>% stack()

b=crop(a,us_ext)
b=mean(b,na.rm=T)

## generic
c=studyarea %>% mutate(!!variable := raster::extract(b,.,fun=mean,na.rm=T))
d=as.data.frame(c) %>% dplyr::select(!!variable,COUNTY_FIPS)

st_write(c, glue("{spatial_dir}/{variable}.shp"))
write.csv(d,glue("{spatial_dir}/{variable}.csv"))
writeRaster(b,glue("{spatial_dir}/{variable}.grd"),overwrite=T)

map=ggplot()+geom_sf(data=c,aes(fill = !!x.var),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()

#3. Temperature seasonality ####
#Source: http://chelsa-climate.org/downloads/
# savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/CHELSA_bio10_04.tif 
# other:  
variable="seasonality_temperature"
x.var <- rlang::sym(variable)
a=raster("/Users/heatherwelch/Dropbox/melenoma/environment/raw/CHELSA_bio10_04.tif")

b=crop(a,us_ext)

## generic
c=studyarea %>% mutate(!!variable := raster::extract(b,.,fun=mean,na.rm=T))
d=as.data.frame(c) %>% dplyr::select(!!variable,COUNTY_FIPS)

st_write(c, glue("{spatial_dir}/{variable}.shp"))
write.csv(d,glue("{spatial_dir}/{variable}.csv"))
writeRaster(b,glue("{spatial_dir}/{variable}.grd"),overwrite=T)

map=ggplot()+geom_sf(data=c,aes(fill = !!x.var),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()
#4. Temperature annual range ####
#Source: http://chelsa-climate.org/downloads/
# savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/CHELSA_bio10_07.tif
# other:  
variable="anRange_temperature"
x.var <- rlang::sym(variable)
a=raster("/Users/heatherwelch/Dropbox/melenoma/environment/raw/CHELSA_bio10_07.tif")

b=crop(a,us_ext)

## generic
c=studyarea %>% mutate(!!variable := raster::extract(b,.,fun=mean,na.rm=T))
d=as.data.frame(c) %>% dplyr::select(!!variable,COUNTY_FIPS)

st_write(c, glue("{spatial_dir}/{variable}.shp"))
write.csv(d,glue("{spatial_dir}/{variable}.csv"))
writeRaster(b,glue("{spatial_dir}/{variable}.grd"),overwrite=T)

map=ggplot()+geom_sf(data=c,aes(fill = !!x.var),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()
#6. Elevation - https://www.ngdc.noaa.gov/mgg/global/ ####
#Source: ETEPO
# savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/ETOPO1_Bed_g_geotiff.tif
# other:  
variable="elevation"
x.var <- rlang::sym(variable)
a=raster("/Users/heatherwelch/Dropbox/melenoma/environment/raw/ETOPO1_Bed_g_geotiff.tif")

b=crop(a,us_ext)

## generic
c=studyarea %>% mutate(!!variable := raster::extract(b,.,fun=mean,na.rm=T))
d=as.data.frame(c) %>% dplyr::select(!!variable,COUNTY_FIPS)

st_write(c, glue("{spatial_dir}/{variable}.shp"))
write.csv(d,glue("{spatial_dir}/{variable}.csv"))
writeRaster(b,glue("{spatial_dir}/{variable}.grd"),overwrite=T)

map=ggplot()+geom_sf(data=c,aes(fill = !!x.var),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()
#7. County level UV Exposure data ####
#Source: https://gis.cancer.gov/tools/uv-exposure/
# savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/uv-county.xlsx
# other:  
variable="cancer_gov_UV_exposure"
x.var <- rlang::sym(variable)
a=read.csv("/Users/heatherwelch/Dropbox/melenoma/environment/raw/uv-county.csv")
sa=studyarea %>% mutate(COUNTY_FIPS=as.integer(COUNTY_FIPS))
b=left_join(sa,a)

# b=crop(a,us_ext)

## generic
# c=studyarea %>% mutate(!!variable := raster::extract(b,.,fun=mean,na.rm=T))
# d=as.data.frame(c) %>% dplyr::select(!!variable,COUNTY_FIPS)
d=as.data.frame(b) %>% dplyr::select(UV_.Wh.m. ,COUNTY_FIPS) %>% rename(cancer_gov_UV_exposure=UV_.Wh.m.)


# st_write(c, glue("{spatial_dir}/{variable}.shp"))
write.csv(d,glue("{spatial_dir}/{variable}.csv"))
# writeRaster(b,glue("{spatial_dir}/{variable}.grd"),overwrite=T)

map=ggplot()+geom_sf(data=b,aes(fill = !!x.var),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()
#9. Climatological erythemal UV dose of Whole year ####
# Source: http://www.temis.nl/uvradiation/UVarchive/v1_GOME/uvclim.php?fb=uvdei
# savefile: /Users/heatherwelch/Dropbox/melenoma/environment/raw/uvdeiclimyear.hdf
# other:  

library(rgdal)
library(gdalUtils)
sds <- get_subdatasets('/Users/heatherwelch/Dropbox/melenoma/environment/raw/uvdeiclimyear.hdf')

library(h5)

variable="elevation"
x.var <- rlang::sym(variable)
a=raster("/Users/heatherwelch/Dropbox/melenoma/environment/raw/ETOPO1_Bed_g_geotiff.tif")

sds <- get_subdatasets('/Users/heatherwelch/Dropbox/melenoma/environment/raw/uvdeiclimyear.hdf')

b=crop(a,us_ext)

## generic
c=studyarea %>% mutate(!!variable := raster::extract(b,.,fun=mean,na.rm=T))
d=as.data.frame(c) %>% dplyr::select(!!variable,COUNTY_FIPS)

st_write(c, glue("{spatial_dir}/{variable}.shp"))
write.csv(d,glue("{spatial_dir}/{variable}.csv"))
writeRaster(b,glue("{spatial_dir}/{variable}.grd"),overwrite=T)

map=ggplot()+geom_sf(data=c,aes(fill = !!x.var),color = "black", size = 0.4)+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+
  ggtitle(variable)+
  scale_fill_gradientn(colours = pals::parula(100),na.value="black")+ coord_sf(xlim = c(-125.0011, -66.9326),ylim = c(24.9493,49.5904))


png(glue("{outdir}/{variable}.png"),width=36,height=22,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
print({map})
dev.off()