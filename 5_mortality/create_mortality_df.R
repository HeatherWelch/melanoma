mortality=read.csv("/Users/heatherwelch/Dropbox/melenoma/mortality/mortality.csv") # this is a cleaned version of the .xlsx you send
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R') # ignore

test=mortality %>% mutate(COUNTY_FIPS=as.numeric(str_extract(mortality$County_name, '(?<=\\()[0-9-]+(?=\\))'))) # extract COUNTY_FIPS
colnames(test)=c("mortality_county_name","mortality_rate","mortality_count","mortality_pop","COUNTY_FIPS") # ignore

old=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_20_20/master_dataframe_breast_cancer.csv") # old csv with seer_rate and exposure
master=left_join(old,test,by="COUNTY_FIPS") # joining old csv with mortality

hisp=read.csv("/Users/heatherwelch/Dropbox/melenoma/mortality/countypop-2010_2018-alldata.csv") # your new hispanic csv
a=hisp %>% dplyr::select(c(STATE,COUNTY,CTYNAME,YEAR,AGEGRP,WA_MALE,WA_FEMALE,HWA_MALE,HWA_FEMALE)) # selecting relevant columns
b=a %>% mutate(st=str_pad(STATE,2,side="left",pad="0"))%>% # extracting COUNTY_FIPS (ignore)
  mutate(cnt=str_pad(COUNTY,3,side="left",pad="0")) %>% # extracting COUNTY_FIPS (ignore)
  mutate(COUNTY_FIPS=as.numeric(glue("{st}{cnt}"))) # extracting COUNTY_FIPS (ignore)

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

# combine everything together (you can stop paying attention)
master2=left_join(master,c,by="COUNTY_FIPS") %>% left_join(.,d,by="COUNTY_FIPS") %>% #linking old csv, mortality, two new hisp metrics
  dplyr::select(c(SEER_rate,COUNTY_FIPS,Pop,cancer_gov_UV_exposure,seasonality_cloud,seasonality_temperature,mortality_rate,mortality_count,mortality_pop,hisp_metric_2010_all_ages,hisp_metric_2010_over40))
  
master3=master2[complete.cases(master2$mortality_rate),]
master4=master3 %>% dplyr::select(c(SEER_rate,cancer_gov_UV_exposure,mortality_rate)) %>% gather(type,rate,-cancer_gov_UV_exposure)
write.csv(master3,"/Users/heatherwelch/Dropbox/melenoma/mortality/incidence_mortality_exposure_hip.csv")
ggplot(data=master4)+geom_point(aes(y=cancer_gov_UV_exposure,x=rate))+geom_smooth(aes(y=cancer_gov_UV_exposure,x=rate),method = "lm")+facet_wrap(~type,scales="free")

# new stuff dad for dad 04-07-20 larger data frame
master2=left_join(master,c,by="COUNTY_FIPS") %>% left_join(.,d,by="COUNTY_FIPS") %>% dplyr::select(-X)
master3=master2[complete.cases(master2$mortality_rate),]
write.csv(master3,"/Users/heatherwelch/Dropbox/melenoma/mortality/incidence_mortality_exposure_hip_fullDF.csv")


### mortality scatter
new=master2 %>% dplyr::select(SEER_rate,mortality_rate,cancer_gov_UV_exposure)

ggplot(new,aes(x=SEER_rate,y=mortality_rate,size=cancer_gov_UV_exposure))+geom_point(alpha=.3)+
  scale_size_binned()(range = c(0, 10))
  
  
  