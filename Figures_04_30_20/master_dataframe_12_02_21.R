### putting it all together
source("/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R")
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_12_02_21";dir.create(outdir)

# add medical data medical ####
# b created here: /Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/1_process_medical_data/prepare_medical_data.R
b=st_read(glue("{spatial_dir}/AHRF_new_12_02_21.shp")) %>% as.data.frame
ahrf=b %>% dplyr::select(c(STATEFP,NAME,COUNTY_, incm_pc,incm_mh,derm_pk,pcp_pk,docs_pk,wpovr50,wpvr100,HI_65)) %>% mutate(COUNTY_FIPS=str_pad(COUNTY_,5,side="left",pad="0"))

# add seer indicence data ####
# not used
# seer=read.csv("/Users/heatherwelch/Dropbox/melenoma/spatial_files/SEER_rate.csv") %>% 
#   rename("Melanoma_incidence"="SEER_rate","Melanoma_incidence_pop"="Pop") %>% mutate(Melanoma_incidence_pop=gsub(",","",Melanoma_incidence_pop))

# add environmental data ####
csv_list=list.files("/Users/heatherwelch/Dropbox/melenoma/spatial_files",pattern=".csv",full.names = T) #%>% 
  #grep("dose.csv|irradiance.csv|exposure.csv",.,value=T,invert=T)
for(csv in csv_list){
  name=gsub("/Users/heatherwelch/Dropbox/melenoma/spatial_files/","",csv) 
  name2= gsub(".csv","",name)
  print(name2)
  a=read.csv(csv) %>% dplyr::select(-X) %>% 
    mutate(COUNTY_FIPS=str_pad(as.character(COUNTY_FIPS),5,side="left",pad="0"))
  print(nrow(a))
  assign(name2,a)
}

master=left_join(SEER_rate,anRange_temperature) %>%  rename("Melanoma_incidence"="SEER_rate","Melanoma_incidence_pop"="Pop") %>% mutate(Melanoma_incidence_pop=gsub(",","",Melanoma_incidence_pop))
master=left_join(master,cancer_gov_UV_exposure)
master=left_join(master,mean_cloud)
master=left_join(master,elevation)
master=left_join(master,mean_temperature)
master=left_join(master,seasonality_cloud)
master=left_join(master,seasonality_temperature)
master=left_join(master,sun_exposure_2012)
master=left_join(master,UV_daily_dose_2012)
master=left_join(master,UV_irradiance_2012)
master=left_join(master,ahrf)

# clean up all units
#CHELSA data
#cloud data was already addressed in preparing_county_raster.R
mast=master %>% mutate(anRange_temperature=anRange_temperature/10,mean_temperature=mean_temperature/10,seasonality_temperature=seasonality_temperature/100)

# add seer mortality ####
mortality=read.csv("/Users/heatherwelch/Dropbox/melenoma/mortality/mortality.csv") # this is a cleaned version of the .xlsx you sent
test=mortality %>% mutate(COUNTY_FIPS=str_extract(mortality$County_name, '(?<=\\()[0-9-]+(?=\\))')) # extract COUNTY_FIPS
colnames(test)=c("Melanoma_mortality_county_name","Melanoma_mortality","Melanoma_mortality_count","Melanoma_mortality_pop","COUNTY_FIPS") # ignore

master=left_join(mast,test,by="COUNTY_FIPS") # joining old csv with mortality

hisp=read.csv("/Users/heatherwelch/Dropbox/melenoma/mortality/countypop-2010_2018-alldata.csv") # your new hispanic csv
a=hisp %>% dplyr::select(c(STATE,COUNTY,CTYNAME,YEAR,AGEGRP,WA_MALE,WA_FEMALE,HWA_MALE,HWA_FEMALE)) # selecting relevant columns
b=a %>% mutate(st=str_pad(STATE,2,side="left",pad="0"))%>% # extracting COUNTY_FIPS (ignore)
  mutate(cnt=str_pad(COUNTY,3,side="left",pad="0")) %>% # extracting COUNTY_FIPS (ignore)
  mutate(COUNTY_FIPS=as.character(glue("{st}{cnt}"))) # extracting COUNTY_FIPS (ignore)

### okay this is the stuff to pay attention to!

# metric 1 (all years)
c=b %>% filter(AGEGRP==0) %>% filter(YEAR==1) %>% # filter data to AGEGRP = 0, YEAR = 1
  mutate(numerator=HWA_MALE + HWA_FEMALE)%>% mutate(denominator=WA_MALE + WA_FEMALE) %>% # create the numerator and denominator
  mutate(hisp_metric_2010_all_ages=numerator/denominator) # divide for final metric

c=c %>% dplyr::select(c(CTYNAME,COUNTY_FIPS,hisp_metric_2010_all_ages))
# metric 2 (above 40)
d=b %>% filter(AGEGRP>=9) %>% filter(YEAR==1) %>% # filter data to AGEGRP = 9-18, YEAR = 1
  group_by(COUNTY_FIPS) %>% # group by county, so 10 rows for each county (1 for each year)
  summarise(WA_MALE=sum(WA_MALE),HWA_MALE=sum(HWA_MALE),WA_FEMALE=sum(WA_FEMALE),HWA_FEMALE=sum(HWA_FEMALE))%>% #sum these columns
  # across the grouped counties (so this will add up the values across years)
  mutate(numerator=HWA_MALE + HWA_FEMALE) %>% mutate(denominator=WA_MALE + WA_FEMALE) %>% # create the numerator and denominator
  mutate(hisp_metric_2010_over40=numerator/denominator) # divide for final metric

d=d %>% dplyr::select(c(COUNTY_FIPS,hisp_metric_2010_over40))
master2=left_join(master,c,by="COUNTY_FIPS") %>% left_join(.,d,by="COUNTY_FIPS")

# add breast cancer ####
BD=read.csv("/Users/heatherwelch/Dropbox/melenoma/medical/Breast_cancer_727_counties.csv") %>% 
  mutate(COUNTY_FIPS=str_pad(as.character(CountyFips),5,side="left",pad="0")) %>% dplyr::select(-CountyFips)
colnames(BD)=c("county","Breast_cancer_NHWinsitu","Breast_cancer_NHWinvasive","Breast_cancer_NHWfemalePop","COUNTY_FIPS")
new=left_join(master2,BD)

# add invasive vs insitu melanoma incidence ####
invasiveInsitu=read.csv("/Users/heatherwelch/Dropbox/melenoma/medical/Melanoma_insitu_vs_invasive.csv") %>% 
  mutate(FIPS=str_pad(FIPS,5,side="left",pad="0")) 
colnames(invasiveInsitu)=c("County","Melanoma_incidence_In_situ","Melanoma_incidence_Invasive","COUNTY_FIPS")
new=left_join(new,invasiveInsitu)

# add lung cancer incidence and mortality ####
lc_incidence=read.csv("/Users/heatherwelch/Dropbox/melenoma/lung_cancer/lung_cancer_incidence.csv") %>% 
  mutate(Rate=as.character(Rate)) %>% 
  mutate(COUNTY_FIPS=str_extract(lc_incidence$State, '(?<=\\()[0-9-]+(?=\\))')) %>% 
  mutate(COUNTY_FIPS=str_pad(COUNTY_FIPS,5,side="left",pad="0")) %>% 
  mutate(Lung_cancer_incidence=as.numeric(Rate)) %>% 
  mutate(Lung_cancer_incidence_count=gsub(",","",Count)) %>% 
  mutate(Lung_cancer_incidence_pop=gsub(",","",Pop)) %>% 
  dplyr::select(-c(Rate,Count,Pop))

new=left_join(new,lc_incidence)
  
lc_mortality=read.csv("/Users/heatherwelch/Dropbox/melenoma/lung_cancer/lung_cancer_mortality.csv") %>% 
  mutate(Rate=as.character(Rate)) 

lc_county=str_extract(lc_mortality$State, '(?<=\\()[0-9-]+(?=\\))')
lc_mortality=lc_mortality %>% 
  # mutate(COUNTY_FIPS=str_extract(lc_mortality$State, '(?<=\\()[0-9-]+(?=\\))')) %>% 
  mutate(COUNTY_FIPS=lc_county) %>% 
  mutate(COUNTY_FIPS=str_pad(COUNTY_FIPS,5,side="left",pad="0")) %>% 
  mutate(Lung_cancer_mortality=as.numeric(Rate)) %>% 
  mutate(Lung_cancer_mortality_count=gsub(",","",Count)) %>% 
  mutate(Lung_cancer_mortality_pop=gsub(",","",Pop))%>% 
  dplyr::select(-c(Rate,Count,Pop))

new=left_join(new,lc_mortality,by="COUNTY_FIPS")

new_final=new
new_final[new_final=="^"]<-NA

write.csv(new_final, glue("{outdir}/master_dataframe_12_02_21.csv"))

## checking county counts for dad ####
total=nrow(new_final)
seer_incidence_w_data=nrow(new_final[complete.cases(new_final$Melanoma_incidence),])
seer_and_variables_w_data=new_final %>% 
  dplyr::select(c(Melanoma_incidence,anRange_temperature,cancer_gov_UV_exposure,mean_cloud,
                  elevation,mean_temperature,seasonality_cloud,seasonality_temperature,sun_exposure,
                  UV_daily_dose,UV_irradiance)) %>% .[complete.cases(.),] %>% nrow()
seer_mortality_w_data=nrow(new_final[complete.cases(new_final$Melanoma_mortality),])
seer_mortality_incidence_w_data=new_final %>% 
  dplyr::select(c(Melanoma_incidence,Melanoma_mortality)) %>% .[complete.cases(.),] %>% nrow()
LC_incidence_w_data=nrow(new_final[complete.cases(new_final$Lung_cancer_incidence),])
LC_mortality_w_data=nrow(new_final[complete.cases(new_final$Lung_cancer_mortality),])
LC_mortality_incidence_w_data=new_final %>% 
  dplyr::select(c(Lung_cancer_incidence,Lung_cancer_mortality)) %>% .[complete.cases(.),] %>% nrow()
all_incidence_and_mortality=new_final %>% 
  dplyr::select(c(Lung_cancer_incidence,Lung_cancer_mortality,Melanoma_incidence,Melanoma_mortality)) %>% .[complete.cases(.),] %>% nrow()

## finding counties with lowest and highest melanoma incidence and UV ####
a=new_final %>% 
  dplyr::select(c(Melanoma_incidence,anRange_temperature,cancer_gov_UV_exposure,mean_cloud,
                  elevation,mean_temperature,seasonality_cloud,seasonality_temperature,sun_exposure,
                  UV_daily_dose,UV_irradiance,COUNTY_FIPS,CTYNAME,State.y)) %>% .[complete.cases(.),] %>% arrange(desc(Melanoma_incidence))




