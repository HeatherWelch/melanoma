## incidence vs mortality plot
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20"

empty=list()
for(i in 1:length(list.files("/Users/heatherwelch/Dropbox/melenoma/congruent_LC_mel_incidence_mortality",pattern=".csv",full.names = T))){
  print(list.files("/Users/heatherwelch/Dropbox/melenoma/congruent_LC_mel_incidence_mortality",pattern=".csv",full.names = T)[i])
  name=gsub("/Users/heatherwelch/Dropbox/melenoma/congruent_LC_mel_incidence_mortality/","",
            list.files("/Users/heatherwelch/Dropbox/melenoma/congruent_LC_mel_incidence_mortality",pattern=".csv",full.names = T)[i]) %>% gsub(".csv","",.)
  a=read.csv(list.files("/Users/heatherwelch/Dropbox/melenoma/congruent_LC_mel_incidence_mortality",pattern=".csv",full.names = T)[i])%>% mutate(COUNTY_FIPS=str_extract(State, '(?<=\\()[0-9-]+(?=\\))')) %>% 
    filter(grepl(':', State)) %>% filter(!grepl('AK|HI', State)) %>% .[complete.cases(.),] %>% mutate(Dataset=name) %>% mutate(State=as.character(State)) %>% 
    mutate(Rate=as.numeric(as.character(Rate))) %>% dplyr::select(-c(State,Count,Pop)) 
  empty[[length(empty)+1]]<-a
  str(a)
}
# # master=do.call("rbind",empty)
# # a1=left_join(empty[[1]],empty[[2]],by="COUNTY_FIPS")
# a1=join_all(empty,by="COUNTY_FIPS",type="full")%>% spread(Dataset,Rate)

master=do.call("rbind",empty) %>% spread(Dataset,Rate)

a=ggplot(data=master,aes(x=`Mel_incidence_01-16`,y=`Mel_mortality_02-17`))+geom_point()+theme_classic()+
  geom_smooth(method="lm",se=F,color="red")+ylab("Melanoma mortality")+xlab("Melanoma incidence")#+
  annotate("text", Inf, Inf, label = glue("R squared={lm.mel}"), hjust =1.5, vjust = 2.5)

b=ggplot(data=master,aes(x=`LC_incidence_01-16`,y=`LC_mortality_02-17`))+geom_point()+theme_classic()+
  geom_smooth(method="lm",se=F,color="red")+ylab("Lung cancer mortality")+xlab("Lung cancer incidence")#+
  annotate("text", Inf, Inf, label = glue("R squared={lm.lc}"), hjust =1.5, vjust = 2.5)
  
# test=master %>% dplyr::select(c('LC_incidence_01-16','LC_mortality_02-17')) %>% .[complete.cases(.),] %>% nrow()
test=master %>% dplyr::select(c('Mel_incidence_01-16','Mel_mortality_02-17')) %>% .[complete.cases(.),] %>% nrow()

mel=master%>% dplyr::select(-c('LC_incidence_01-16','LC_mortality_02-17')) %>% .[complete.cases(.),]
mel_counties=mel$COUNTY_FIPS

all=master %>% .[complete.cases(.),]
all_counties=all$COUNTY_FIPS

setdiff(mel_counties,all_counties)
test=master %>% .[complete.cases(.),]

write.csv(test, "/Users/heatherwelch/Dropbox/melenoma/congruent_LC_mel_incidence_mortality/cancers_combined.csv")

  
# lc_m=read.csv("/Users/heatherwelch/Dropbox/melenoma/congruent_LC_mel_incidence_mortality/LC_mortality_02-17.csv") %>% 
#   dplyr::rename(LC_m_rate=Rate)%>% mutate(COUNTY_FIPS=str_extract(State, '(?<=\\()[0-9-]+(?=\\))')) %>% 
#   filter(grepl(':', State)) %>% filter(!grepl('AK|HI', State)) %>% dplyr::select(-c(Count,Pop))
# lc_i=read.csv("/Users/heatherwelch/Dropbox/melenoma/congruent_LC_mel_incidence_mortality/LC_incidence_01-16.csv") %>% 
#   dplyr::mutate(LC_i_rate=as.numeric(as.character(Rate)))%>% mutate(COUNTY_FIPS=str_extract(State, '(?<=\\()[0-9-]+(?=\\))')) %>% 
#   filter(grepl(':', State)) %>% filter(!grepl('AK:|HI:', State))%>% dplyr::select(-c(Count,Pop,State))
# 
# test=left_join(lc_i,lc_m)
# test2=dplyr::select(test,c(LC_i_rate,LC_m_rate)) %>% .[complete.cases(.),]
# cor(test2)
# 
# b=ggplot(data=test,aes(x=LC_i_rate,y=LC_m_rate))+geom_point()+theme_classic()+
#   geom_smooth(method="lm",se=F,color="red")+ylab("Lung cancer mortality")+xlab("Lung cancer incidence")
# 
# 
# b
# 
# hist(test$LC_i_rate)
# 
# png(glue("{outdir}/Mel_LC.png"),width=20,height=10,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# plot_grid(a,b,ncol=2)
# dev.off()
# 
# 
# 
# lm.mel <- round(summary(lm(Melanoma_mortality ~ Melanoma_incidence,data=new_final))$r.squared,2)
# lm.lc <- round(summary(lm(Lung_cancer_mortality ~ Lung_cancer_incidence,data=new_final))$r.squared,2)
# 
# mel=new_final %>% dplyr::select(Melanoma_mortality,Melanoma_incidence,Lung_cancer_mortality,Lung_cancer_incidence) %>% 
#   .[complete.cases(.),]
# 
# cor(mel)
# 
# a=ggplot(data=new_final,aes(x=Melanoma_incidence,y=Melanoma_mortality))+geom_point()+theme_classic()+
#   geom_smooth(method="lm",se=F,color="red")+ylab("Melanoma mortality")+xlab("Melanoma incidence")+
#   annotate("text", Inf, Inf, label = glue("R squared={lm.mel}"), hjust =1.5, vjust = 2.5)
# 
# b=ggplot(data=new_final,aes(x=Lung_cancer_incidence,y=Lung_cancer_mortality))+geom_point()+theme_classic()+
#   geom_smooth(method="lm",se=F,color="red")+ylab("Lung cancer mortality")+xlab("Lung cancer incidence")+
#   annotate("text", Inf, Inf, label = glue("R squared={lm.lc}"), hjust =1.5, vjust = 2.5)
# 
# png(glue("{outdir}/Mel_LC.png"),width=20,height=10,units='cm',res=400)
# par(ps=10)
# par(mar=c(4,4,1,1))
# par(cex=1)
# plot_grid(a,b,ncol=2)
# dev.off()
