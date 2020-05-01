## physician supply figure check

new_final=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20/master_dataframe_04_30_20.csv") %>% 
  dplyr::select(c(Melanoma_incidence,anRange_temperature,cancer_gov_UV_exposure,mean_cloud,
                  elevation,mean_temperature,seasonality_cloud,seasonality_temperature,sun_exposure,
                  UV_daily_dose,UV_irradiance,incm_pc,incm_mh,derm_pk,HI_65,pcp_pk,docs_pk,wpovr50,wpvr100)) %>% .[complete.cases(.),] %>% 
    mutate(Derms=derm_pk,PrimCar=pcp_pk) %>% 
                  mutate(Derms = replace(Derms, derm_pk == 0, "No_derm")) %>% 
                  mutate(Derms = replace(Derms, derm_pk > 0, "Yes_derm"))%>% 
  mutate(PrimCar = replace(PrimCar, pcp_pk < 30, "No_pcp")) %>% 
  mutate(PrimCar = replace(PrimCar, pcp_pk > 30, "Yes_pcp"))
  
# ratios derms
derms=new_final %>% group_by(Derms) %>% summarise(meanMel=mean(Melanoma_incidence),meanUV=mean(cancer_gov_UV_exposure),meanIncom=mean(wpovr50),
                                                  ncounties=n()) %>% 
  rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 

colnames(derms)=derms[1,]
  
derms=derms %>% dplyr::slice(.,2:n()) %>% mutate(No_derm=as.numeric(No_derm),Yes_derm=as.numeric(Yes_derm)) %>% as.data.frame %>% 
  mutate(ratio=Yes_derm/No_derm)

# ratios pcp
pcp=new_final %>% group_by(PrimCar) %>% summarise(meanMel=mean(Melanoma_incidence),meanUV=mean(cancer_gov_UV_exposure),meanIncom=mean(wpovr50),
                                                  ncounties=n()) %>% 
  rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 

colnames(pcp)=pcp[5,]

pcp=pcp %>% dplyr::slice(.,1:4) %>% mutate(No_pcp=as.numeric(No_pcp),Yes_pcp=as.numeric(Yes_pcp)) %>% as.data.frame %>% 
  mutate(ratio=Yes_pcp/No_pcp)
  
 
