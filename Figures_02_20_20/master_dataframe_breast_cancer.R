### putting it all together
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"

# medical
b=st_read(glue("{spatial_dir}/AHRF_new_01_30_20.shp")) %>% as.data.frame
ahrf=b %>% dplyr::select(c(STATEFP,NAME,COUNTY_, incm_pc,incm_mh,derm_pk,pcp_pk,docs_pk,wpovr50,wpvr100,HI_65)) %>% rename(COUNTY_FIPS=COUNTY_)

seer=read.csv("/Users/heatherwelch/Dropbox/melenoma/spatial_files/SEER_rate.csv")

# environmental
csv_list=list.files("/Users/heatherwelch/Dropbox/melenoma/spatial_files",pattern=".csv",full.names = T)
for(csv in csv_list){
  name=gsub("/Users/heatherwelch/Dropbox/melenoma/spatial_files/","",csv) 
  name2= gsub(".csv","",name)
  print(name2)
  a=read.csv(csv) %>% dplyr::select(-X)
  print(nrow(a))
  assign(name2,a)
}

master=left_join(SEER_rate,anRange_temperature)
master=left_join(master,cancer_gov_UV_exposure)
master=left_join(master,mean_cloud)
master=left_join(master,elevation)
master=left_join(master,mean_temperature)
master=left_join(master,seasonality_cloud)
master=left_join(master,seasonality_temperature)
master=left_join(master,sun_exposure)
master=left_join(master,UV_daily_dose)
master=left_join(master,UV_irradiance)
master=left_join(master,ahrf)

BD=read.csv("/Users/heatherwelch/Dropbox/melenoma/medical/Breast_cancer_727_counties.csv")
new=left_join(master,BD,by=c("COUNTY_FIPS"="CountyFips"))

mast=new %>% mutate(anRange_temperature=anRange_temperature/10,mean_temperature=mean_temperature/10,seasonality_temperature=seasonality_temperature/10)

write.csv(mast, "/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/master_dataframe_breast_cancer.csv")

