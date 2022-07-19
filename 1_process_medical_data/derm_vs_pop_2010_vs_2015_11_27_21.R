## dermatologists for dad

source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')

## melanoma incidence data
incidence=read.csv("/Users/heatherwelch/Dropbox/melenoma/plots_11_24_21/Melanoma_incidence.csv")
county=incidence$County %>% str_sub(.,-6,-2)
in2=incidence %>% mutate(COUNTY_FIPS=county) %>% dplyr::select(-County) %>% 
  mutate(NHWpop10_11=as.numeric(gsub(",","",NHWpop10_11))) %>% 
  mutate(NHWpop15_16=as.numeric(gsub(",","",NHWpop15_16)))

# studyarea=st_read(glue("/Users/heatherwelch/Dropbox/melenoma/us_shapefiles/tl_2017_us_county/tl_2017_us_county.shp"))
# studyarea=studyarea %>% mutate(COUNTY_FIPS=glue("{STATEFP}{COUNTYFP}"))
# us_ext=c(-125.0011, -66.9326, 24.9493,49.5904)
# sa=studyarea %>% mutate(COUNTY_FIPS=as.integer(COUNTY_FIPS))

# spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_11_24_21";dir.create(outdir)

## derm data data
library(haven)
library(glue)
a=read_sas("/Users/heatherwelch/Dropbox/melenoma/medical/AHRF_2018-2019/AHRF_2018-2019_SAS/ahrf2019.sas7bdat")
b=read.csv("/Users/heatherwelch/Dropbox/melenoma/medical/co-est2019-alldata.csv")

## derm
derm_count2015=a %>% dplyr::select(f1108715) %>% as.data.frame() %>% pull(f1108715)
derm_count2010=a %>% dplyr::select(f1108710) %>% as.data.frame() %>% pull(f1108710)

# ## pop
# pop_2015_HRF=a %>% dplyr::select(f1198415) %>% as.data.frame() %>% pull(f1198415)
# pop_2015_census=b %>% dplyr::select(POPESTIMATE2015)%>% as.data.frame() %>% pull(POPESTIMATE2015)
# pop_2010_census=b %>% dplyr::select(POPESTIMATE2010)%>% as.data.frame() %>% pull(POPESTIMATE2010)

## county
HRF=a%>% dplyr::select(f00002,f00010) %>% mutate(COUNTY_FIPS=f00002) %>% 
  mutate(derm_count2010=derm_count2010,derm_count2015=derm_count2015)

master=left_join(in2,HRF) %>% dplyr::select(-f00002) ## send this to dad
master2=master %>% mutate(restrict=case_when(derm_count2015==0&derm_count2010==0~"restrict",
                                             TRUE ~ "no")) %>% 
  filter(restrict=="no")

# quantile(master3$change_supply)
# library(mltools)
master3=master2 %>% mutate(change_supply=derm_count2015/derm_count2010) %>% 
  mutate(category=case_when(#is.infinite(change_supply)~"undefined",
    change_supply<=.75~"decrease_25%",
                            change_supply>.75&change_supply<1.25~"little_change",
                            change_supply>=1.25&change_supply<1.5~"increase_25%",
                            change_supply>=1.5~"increase_50%",
                            TRUE~as.character(change_supply))) #%>% 
  # mutate(category1=case_when(change_supply<=0.9424342~"quantile_0-.25",
  #   change_supply>0.9424342&change_supply<=1.0000000~"quantile_.25-.50",
  #   change_supply>1.0000000&change_supply<=1.1773158~"quantile_.50-.75",
  #   change_supply>1.1773158~"quantile_.75-1",
  #   TRUE~as.character(change_supply)))

master3.1=master3 %>% dplyr::select(-restrict)
# write.csv(master3.1,glue("{outdir}/change_supply_11-29-21.csv"))

master3 %>% group_by(category) %>% summarise(n=n())

master4=master3 %>% dplyr::select(c(All.melanoma10_11,All.melanoma15_16,derm_count2010,derm_count2015,category)) %>% 
  gather(year,number,-c(derm_count2010,derm_count2015,category)) %>% 
  group_by(year,category) %>% summarise(mean_incidence=mean(number)) %>% 
  spread(year,mean_incidence) %>% mutate(diff=All.melanoma15_16-All.melanoma10_11) %>% arrange(diff)

# write.csv(master4,glue("{outdir}/summary_11-29-21.csv"))

### dads new idea ####
master3=master2 %>% mutate(change_supply=derm_count2015/derm_count2010) %>% 
  mutate(category=case_when(#is.infinite(change_supply)~"undefined",
    change_supply<1~"Lost_derms",
    change_supply==1~"No_change",
    change_supply>1~"Gained_derms",
    TRUE~as.character(change_supply)))

master3 %>% group_by(category) %>% summarise(n=n())
master4=master3 %>% dplyr::select(c(All.melanoma10_11,All.melanoma15_16,derm_count2010,derm_count2015,category)) %>% 
  gather(year,number,-c(derm_count2010,derm_count2015,category)) %>% 
  group_by(year,category) %>% summarise(mean_incidence=mean(number)) %>% 
  spread(year,mean_incidence) %>% mutate(diff=All.melanoma15_16-All.melanoma10_11) %>% arrange(diff)

### new idea with UV ####
master3=master2 %>% mutate(change_supply=derm_count2015/derm_count2010) %>% 
  mutate(category=case_when(#is.infinite(change_supply)~"undefined",
    change_supply<1~"Lost_derms",
    change_supply==1~"No_change",
    change_supply>1~"Gained_derms",
    TRUE~as.character(change_supply))) %>% 
  mutate(COUNTY_FIPS=as.integer(COUNTY_FIPS))

big_df=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_12_02_21/master_dataframe_12_02_21.csv") %>%
  dplyr::select(c(cancer_gov_UV_exposure,COUNTY_FIPS)) %>% 
  .[complete.cases(.),]

master4=left_join(master3,big_df)%>% dplyr::select(c(cancer_gov_UV_exposure,All.melanoma10_11,All.melanoma15_16,derm_count2010,derm_count2015,category)) %>% 
  gather(year,number,-c(derm_count2010,derm_count2015,category,cancer_gov_UV_exposure)) %>% 
  group_by(year,category) %>% summarise(mean_incidence=mean(number),mean_uv=mean(cancer_gov_UV_exposure)) %>% 
  spread(year,mean_incidence) %>% mutate(diff=All.melanoma15_16-All.melanoma10_11) %>% arrange(diff)

### new idea 12.19.21 insitu vs invasive
master_in_v_in=master3 %>% dplyr::select(c(In.situ10_11,Invasive10_11,In.situ15_16,Invasive15_16,category)) %>% 
  mutate(Invasive=Invasive15_16-Invasive10_11, In.situ=In.situ15_16-In.situ10_11) %>% 
  dplyr::select(c(category,Invasive,In.situ)) %>% 
  group_by(category) %>% summarise(Invasive=mean(Invasive),In.situ=mean(In.situ)) %>% 
  mutate(sum=Invasive+In.situ) %>% dplyr::select(-sum) %>% 
  gather(melanoma,rate,-category) %>% 
  mutate(category=factor(category,levels=c("Lost_derms","No_change","Gained_derms")))

ggplot(master_in_v_in,aes(x=category,y=rate,fill=melanoma))+
  geom_bar(stat="identity")

  
  gather(year,number,-c(derm_count2010,derm_count2015,category)) %>% 
  group_by(year,category) %>% summarise(mean_incidence=mean(number))


master_10.11=master3 %>% dplyr::select(In.situ10_11,Invasive10_11,category) %>% 
  mutate(diff=All.melanoma15_16-All.melanoma10_11)

# %>% 
#   gather(incidence,value,-c(change_supply,category)) %>% mutate(year=)
master_15.16=master3 %>% dplyr::select(In.situ15_16,Invasive15_16,change_supply,category) %>% 
  gather(incidence,value,-c(change_supply,category))

# master4=master3 %>% dplyr::select(c(Invasive10_11,Invasive15_16,derm_count2010,derm_count2015,category)) %>% 
#   gather(year,number,-c(derm_count2010,derm_count2015,category)) %>% 
#   group_by(year,category) %>% summarise(mean_incidence=mean(number)) %>% 
#   spread(year,mean_incidence) %>% mutate(diff=Invasive15_16,Invasive10_11) %>% arrange(diff)
# 
# test=master3 %>% mutate(test=bin_data(master3$change_supply, bins=4, binType = "quantile"))
# a=test %>% group_by(test) %>% summarise(n=n())
# summary(as.factor(master3$category1))
# a=master3 %>% dplyr::select(category1,change_supply) %>% arrange(category1)
# a=master3 %>% group_by(category1) %>% summarise(n=n())
# 
# master4=master3 %>% dplyr::select(c(All.melanoma10_11,All.melanoma15_16,derm_count2010,derm_count2015,category1)) %>% 
#   gather(year,number,-c(derm_count2010,derm_count2015,category1)) %>% 
#   group_by(year,category1) %>% summarise(mean_incidence=mean(number)) %>% 
#   spread(year,mean_incidence) %>% mutate(diff=All.melanoma15_16-All.melanoma10_11) %>% arrange(diff)
# 
# ggplot(master4,aes(x=category,y=number,group=year,fill=year))+geom_bar(stat="identity",position = "dodge")
# 
# master4=test %>% dplyr::select(c(All.melanoma10_11,All.melanoma15_16,derm_count2010,derm_count2015,test)) %>% 
#   gather(year,number,-c(derm_count2010,derm_count2015,test)) %>% 
#   group_by(year,test) %>% summarise(mean_incidence=mean(number)) %>% 
#   spread(year,mean_incidence) %>% mutate(diff=All.melanoma15_16-All.melanoma10_11) %>% arrange(diff)
# 
# census=b%>% mutate(county=str_pad(COUNTY,3,"left",0)) %>% 
#   mutate(state=str_pad(STATE,2,"left",0)) %>% mutate(COUNTY_FIPS=as.character(glue("{state}{county}"))) %>% 
#   dplyr::select(COUNTY_FIPS,CTYNAME) %>% mutate(pop_2015_census=pop_2015_census,pop_2010_census=pop_2010_census)
# 
# master=full_join(HRF,census) %>% dplyr::select(-f00002) %>% rename(CTYNAME_HRF=f00010,CTYNAME_census=CTYNAME) %>% 
#   .[,c(2,1,6,3,4,8,7,5)] %>% mutate(pop_2015_census_minus_HRF=pop_2015_census-pop_2015_HRF) %>% 
#   mutate(derm_rate2010_census=derm_count2010*100000/pop_2010_census) %>% 
#   mutate(derm_rate2015_census=derm_count2015*100000/pop_2015_census) %>% 
#   mutate(derm_rate2015_HRF=derm_count2015*100000/pop_2015_HRF)
# 
# write.csv(master,glue("{outdir}/derm_pop_11-24-21.csv"))
# 
# # # sa2=as.data.frame(sa) %>% dplyr::select(STATEFP,COUNTYFP,NAMELSAD,COUNTY_FIPS)
# # master=a %>% dplyr::select(f00002,f00012,f00011,f00010)%>% mutate(COUNTY_FIPS=as.integer(f00002)) 
# # 
# # b2=b %>% .[,1:10] %>% mutate(county=str_pad(COUNTY,3,"left",0)) %>% 
# #   mutate(state=str_pad(STATE,2,"left",0)) %>% mutate(COUNTY_FIPS=glue("{state}{county}"))
# # test=master %>% filter(f00010=="Cuming")
# # test2=b2 %>% filter(CTYNAME=="Cuming County") %>% .[,1:13]
# # master2=left_join(sa2,master)
# # 
# # test=a %>% dplyr::select(f1198410)
# # pop=a %>% dplyr::select(f1198411) 
# #   F1198410