### putting it all together
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"

# medical
b=st_read(glue("{spatial_dir}/AHRF.shp")) %>% as.data.frame
ahrf=b %>% dplyr::select(c(NAME,COUNTY_, incm_pc,incm_mh,derm_pk,pcp_pk,docs_pk)) %>% rename(COUNTY_FIPS=COUNTY_)

seer=read.csv("/Users/heatherwelch/Dropbox/melenoma/spatial_files/SEER_rate.csv")

# environmental
csv_list=list.files("/Users/heatherwelch/Dropbox/melenoma/spatial_files",pattern=".csv",full.names = T)
for(csv in csv_list){
  name=gsub("/Users/heatherwelch/Dropbox/melenoma/spatial_files/","",csv) 
  name2= gsub(".csv","",name)
  print(name2)
  a=read.csv(csv) %>% dplyr::select(-X)
  assign(name2,a)
}

master=left_join(SEER_rate,anRange_temperature)
master=left_join(master,cancer_gov_UV_exposure)
master=left_join(master,mean_cloud)
master=left_join(master,elevation)
master=left_join(master,mean_temperature)
master=left_join(master,seasonality_cloud)
master=left_join(master,seasonality_temperature)
master=left_join(master,ahrf)

g=master[complete.cases(master),] %>% dplyr::select(-c(Pop,COUNTY_FIPS,NAME))

head(g)

M <- cor(g)
corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9)


