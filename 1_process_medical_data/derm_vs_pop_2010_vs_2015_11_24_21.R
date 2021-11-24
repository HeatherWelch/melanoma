## dermatologists for dad

source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
studyarea=studyarea %>% mutate(COUNTY_FIPS=glue("{STATEFP}{COUNTYFP}"))
us_ext=c(-125.0011, -66.9326, 24.9493,49.5904)
sa=studyarea %>% mutate(COUNTY_FIPS=as.integer(COUNTY_FIPS))

spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_11_24_21";dir.create(outdir)

## data
library(haven)
library(glue)
a=read_sas("/Users/heatherwelch/Dropbox/melenoma/medical/AHRF_2018-2019/AHRF_2018-2019_SAS/ahrf2019.sas7bdat")
b=read.csv("/Users/heatherwelch/Dropbox/melenoma/medical/co-est2019-alldata.csv")

## derm
derm_count2015=a %>% dplyr::select(f1108715) %>% as.data.frame() %>% pull(f1108715)
derm_count2010=a %>% dplyr::select(f1108710) %>% as.data.frame() %>% pull(f1108710)

## pop
pop_2015_HRF=a %>% dplyr::select(f1198415) %>% as.data.frame() %>% pull(f1198415)
pop_2015_census=b %>% dplyr::select(POPESTIMATE2015)%>% as.data.frame() %>% pull(POPESTIMATE2015)
pop_2010_census=b %>% dplyr::select(POPESTIMATE2010)%>% as.data.frame() %>% pull(POPESTIMATE2010)

## county
HRF=a%>% dplyr::select(f00002,f00010) %>% mutate(COUNTY_FIPS=f00002) %>% 
  mutate(derm_count2010=derm_count2010,derm_count2015=derm_count2015,pop_2015_HRF=pop_2015_HRF)

census=b%>% mutate(county=str_pad(COUNTY,3,"left",0)) %>% 
  mutate(state=str_pad(STATE,2,"left",0)) %>% mutate(COUNTY_FIPS=as.character(glue("{state}{county}"))) %>% 
  dplyr::select(COUNTY_FIPS,CTYNAME) %>% mutate(pop_2015_census=pop_2015_census,pop_2010_census=pop_2010_census)

master=full_join(HRF,census) %>% dplyr::select(-f00002) %>% rename(CTYNAME_HRF=f00010,CTYNAME_census=CTYNAME) %>% 
  .[,c(2,1,6,3,4,8,7,5)] %>% mutate(pop_2015_census_minus_HRF=pop_2015_census-pop_2015_HRF) %>% 
  mutate(derm_rate2010_census=derm_count2010*100000/pop_2010_census) %>% 
  mutate(derm_rate2015_census=derm_count2015*100000/pop_2015_census) %>% 
  mutate(derm_rate2015_HRF=derm_count2015*100000/pop_2015_HRF)

write.csv(master,glue("{outdir}/derm_pop_11-24-21.csv"))

# # sa2=as.data.frame(sa) %>% dplyr::select(STATEFP,COUNTYFP,NAMELSAD,COUNTY_FIPS)
# master=a %>% dplyr::select(f00002,f00012,f00011,f00010)%>% mutate(COUNTY_FIPS=as.integer(f00002)) 
# 
# b2=b %>% .[,1:10] %>% mutate(county=str_pad(COUNTY,3,"left",0)) %>% 
#   mutate(state=str_pad(STATE,2,"left",0)) %>% mutate(COUNTY_FIPS=glue("{state}{county}"))
# test=master %>% filter(f00010=="Cuming")
# test2=b2 %>% filter(CTYNAME=="Cuming County") %>% .[,1:13]
# master2=left_join(sa2,master)
# 
# test=a %>% dplyr::select(f1198410)
# pop=a %>% dplyr::select(f1198411) 
#   F1198410