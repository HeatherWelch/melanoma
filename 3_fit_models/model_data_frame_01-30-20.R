### putting it all together
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"

# medical
b=st_read(glue("{spatial_dir}/AHRF_new_01_30_20.shp")) %>% as.data.frame
ahrf=b %>% dplyr::select(c(NAME,COUNTY_, incm_pc,incm_mh,derm_pk,pcp_pk,docs_pk,wpovr50,wpvr100,HI_65)) %>% rename(COUNTY_FIPS=COUNTY_)

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
master=left_join(master,sun_exposure)
master=left_join(master,UV_daily_dose)
master=left_join(master,UV_irradiance)
master=left_join(master,ahrf)

g=master[complete.cases(master),] 
write.csv(g,"/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/model_data/data_01_30_20_withpop_FIPS.csv")
g=master[complete.cases(master),] %>% dplyr::select(-c(Pop,COUNTY_FIPS,NAME))

head(g)

M <- cor(g)
corrplot(M,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9)

write.csv(g,"/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/model_data/data_01_30_20.csv")


## quick model
master=g %>% gather(variable, value,-SEER_rate)

C3=ggplot(master,aes(x=SEER_rate,y=value),color="black")+geom_point(size=1)+stat_smooth(se=F,method = "gam",formula = y~s(x, bs="cr"))+
  theme(text = element_text(size=8),axis.text = element_text(size=8),plot.title = element_text(hjust=0,size=8),legend.position=c(.15,.3),legend.justification = c(.9,.9),legend.key.size = unit(.5,'lines'))+
  theme(legend.background = element_blank(),legend.box.background = element_rect(colour = NA),legend.margin=unit(0.3, "lines"))+theme_classic()+
  #ggtitle("Effect of equal opposing swordfish/leatherback weightings on the relationship between EcoCast values and leatherback habitat suitability values")+
  ylab("Predictor")+xlab("Melanoma rate")+facet_wrap(~variable,scales="free")

C3

C3=ggplot(master,aes(x=SEER_rate,y=value),color="black")+geom_point(size=1)+stat_smooth(se=F,method = "lm",formula = y~x)+
  theme(text = element_text(size=8),axis.text = element_text(size=8),plot.title = element_text(hjust=0,size=8),legend.position=c(.15,.3),legend.justification = c(.9,.9),legend.key.size = unit(.5,'lines'))+
  theme(legend.background = element_blank(),legend.box.background = element_rect(colour = NA),legend.margin=unit(0.3, "lines"))+theme_classic()+
  #ggtitle("Effect of equal opposing swordfish/leatherback weightings on the relationship between EcoCast values and leatherback habitat suitability values")+
  ylab("Predictor")+xlab("Melanoma rate")+facet_wrap(~variable,scales="free")

C3

data=seq(0,1,by=.001) %>% as.data.frame()
df <- data.frame(var=character(),
                 rsq=character(), 
                 dev=character(), 
                 stringsAsFactors=FALSE) 

h=apply(g[,2:19],2,function(x)gam(g$SEER_rate~s(x,bs="cr"))) #%>% lapply(.,function(x)summary[[x]])
for(i in 1:length(h)){
  a=summary(h[[i]])$r.sq
  print(a)
  b=summary(h[[i]])$dev.expl
  print(b)
  var=names(h)[i]
  # df$var[i]=var
  # df$rsq[i]=a
  # df$dev[i]=b
  c=data.frame(var=var,rsq=a,dev=b)
  df=rbind(df,c)
}

df %>% arrange(dev)


df <- data.frame(var=character(),
                 rsq=character(), 
                 stringsAsFactors=FALSE) 

h=apply(g[,2:19],2,function(x)lm(g$SEER_rate~x,g)) #%>% lapply(.,function(x)summary[[x]])
for(i in 1:length(h)){
  a=summary(h[[i]])$r.sq
  print(a)
  # b=summary(h[[i]])$dev.expl
  # print(b)
  var=names(h)[i]
  # df$var[i]=var
  # df$rsq[i]=a
  # df$dev[i]=b
  c=data.frame(var=var,rsq=a)
  df=rbind(df,c)
}

df %>% arrange(rsq)
