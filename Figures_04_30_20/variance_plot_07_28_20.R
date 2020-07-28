source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20"
master=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20/master_dataframe_04_30_20.csv") %>% dplyr::select(c(Melanoma_incidence,cancer_gov_UV_exposure,elevation,seasonality_cloud,seasonality_temperature,UV_irradiance,incm_mh,incm_pc,wpovr50,pcp_pk,derm_pk,Melanoma_incidence_pop))
colnames(master) <- make.unique(names(master))
new=master[complete.cases(master),] %>% rename("Temperature Variability"=seasonality_temperature) %>% 
  rename("Dermatologists"=derm_pk) %>% 
  rename("NHW melanoma incidence"=Melanoma_incidence) %>% 
  rename("UV Daily Dose"=cancer_gov_UV_exposure) %>% 
  rename("Elevation"=elevation) %>% 
  rename("Primary care"=pcp_pk) %>% 
  rename("Cloud Variability"=seasonality_cloud) %>% 
  rename("UV Solar Noon"=UV_irradiance) %>% 
  rename("Median Household"=incm_mh) %>% 
  rename("Income per capita"=incm_pc) %>% 
  rename("Households >$50,000"=wpovr50)

new=new %>% mutate(Melanoma_incidence_pop=gsub(",","",Melanoma_incidence_pop) %>% as.numeric()) %>% mutate(Melanoma_incidence_pop=Melanoma_incidence_pop/5)
# new=new %>% dplyr::select(-Melanoma_incidence_pop)

breaks=seq(5000,500000,by=5000)
dat=data.frame(breakk=as.numeric(),
               NumOver=as.numeric(),
               NumUnder=as.numeric(),
               
               sdover_mel=as.numeric(),
               sdunder_mel=as.numeric(),
               cvover_mel=as.numeric(),
               cvunder_mel=as.numeric(),
               
               sdover_uv=as.numeric(),
               sdunder_uv=as.numeric(),
               cvover_uv=as.numeric(),
               cvunder_uv=as.numeric(),
               
               sdover_wpvr50=as.numeric(),
               sdunder_wpvr50=as.numeric(),
               cvover_wpvr50=as.numeric(),
               cvunder_wpvr50=as.numeric(),
               
               stringsAsFactors=FALSE)

for(i in 1:length(breaks)) {
  over=new %>% filter(Melanoma_incidence_pop>breaks[i])
  under=new %>% filter(Melanoma_incidence_pop<breaks[i])
  
  Nover=nrow(over)
  Nunder=nrow(under)
  
  # melanoma
  sdover_mel=sd(over$`NHW melanoma incidence`)
  sdunder_mel=sd(under$`NHW melanoma incidence`)
  cvover_mel=sdover_mel/mean(over$`NHW melanoma incidence`)
  cvunder_mel=sdover_mel/mean(under$`NHW melanoma incidence`)
  
  # uv daily dose
  sdover_uv=sd(over$`UV Daily Dose`)
  sdunder_uv=sd(under$`UV Daily Dose`)
  cvover_uv=sdover_uv/mean(over$`UV Daily Dose`)
  cvunder_uv=sdover_uv/mean(under$`UV Daily Dose`)
  
  # wpvr50
  sdover_wpvr50=sd(over$`Households >$50,000`)
  sdunder_wpvr50=sd(under$`Households >$50,000`)
  cvover_wpvr50=sdover_wpvr50/mean(over$`Households >$50,000`)
  cvunder_wpvr50=sdover_wpvr50/mean(under$`Households >$50,000`)
  
  dat[i,1]=breaks[i]
  dat$NumOver[i]=Nover
  dat$NumUnder[i]=Nunder
  
  dat$sdover_mel[i]=sdover_mel
  dat$sdunder_mel[i]=sdunder_mel
  dat$cvover_mel[i]=cvover_mel
  dat$cvunder_mel[i]=cvunder_mel
  
  dat$sdover_uv[i]=sdover_uv
  dat$sdunder_uv[i]=sdunder_uv
  dat$cvover_uv[i]=cvover_uv
  dat$cvunder_uv[i]=cvunder_uv
  
  dat$sdover_wpvr50[i]=sdover_wpvr50
  dat$sdunder_wpvr50[i]=sdunder_wpvr50
  dat$cvover_wpvr50[i]=cvover_wpvr50
  dat$cvunder_wpvr50[i]=cvunder_wpvr50
  
}

plot(dat$cvunder_mel,dat$cvunder_uv)

ggplot(dat,aes(x=cvover_mel,y=cvunder_mel))+geom_point(aes(color=breakk))+geom_abline(intercept = 0,slope=1)#+
  #xlim(.3,.8)+ylim(.3,.5)

ggplot(dat,aes(x=sdover_mel,y=sdunder_mel))+geom_point(aes(color=breakk))+geom_abline(intercept = 0,slope=1)#+
  #xlim(.3,.8)+ylim(.3,.8)

ggplot(dat,aes(x=cvover_uv,y=cvunder_uv))+geom_point()+geom_abline(intercept = 0,slope=1)

ggplot(dat,aes(x=breakk,y=sdunder_uv))+geom_point()+
  scale_x_continuous(label=comma)

ggplot(dat,aes(x=breakk,y=cvunder_mel))+geom_point()+
  scale_x_continuous(label=comma)

ggplot(dat,aes(x=breakk,y=cvover_mel))+geom_point()+
  scale_x_continuous(label=comma)

ggplot(dat,aes(x=cvover_uv,y=cvover_mel))+geom_point()

over=ggplot(dat,aes(x=cvover_mel,y=NumOver))+geom_point(aes(color=breakk))+
  scale_color_continuous(breaks=c(5000,50000,100000,300000,500000),labels=c("5000","50000","100000","300000","500000"))+
  ylab("Number of counties over")+xlab("Coefficient of variation in melanoma")+xlim(0.25,.45)+ggtitle("Large counties")+
  ylim(0,700)

under=ggplot(dat,aes(x=cvunder_mel,y=NumUnder))+geom_point(aes(color=breakk))+
  scale_color_continuous(breaks=c(5000,50000,100000,300000,500000),labels=c("5000","50000","100000","300000","500000"))+
  ylab("Number of counties under")+xlab("Coefficient of variation in melanoma")+xlim(0.25,.45)+ggtitle("Small counties")+
  ylim(0,700)

png(glue("{outdir}/cv.png"),width=40,height=15,units='cm',res=400)
par(ps=10)
#par(mar=c(1,1,1,1))
par(cex=1)
plot_grid(over,under, ncol=2, align="h",axis = "bt")
dev.off()

over=ggplot(dat,aes(x=cvover_wpvr50,y=NumOver))+geom_point(aes(color=breakk))+
  scale_color_continuous(breaks=c(5000,50000,100000,300000,500000),labels=c("5000","50000","100000","300000","500000"))+
  ylab("Number of counties over")+xlab("Coefficient of variation in wpover50")+xlim(0.09,.3)+ggtitle("Large counties")

under=ggplot(dat,aes(x=cvunder_wpvr50,y=NumUnder))+geom_point(aes(color=breakk))+
  scale_color_continuous(breaks=c(5000,50000,100000,300000,500000),labels=c("5000","50000","100000","300000","500000"))+
  ylab("Number of counties under")+xlab("Coefficient of variation in wpover50")+xlim(0.09,.3)+ggtitle("Small counties")

png(glue("{outdir}/cvunder_wpvr50.png"),width=40,height=15,units='cm',res=400)
par(ps=10)
#par(mar=c(1,1,1,1))
par(cex=1)
plot_grid(over,under, ncol=2, align="h",axis = "bt")
dev.off()


over=ggplot(dat,aes(x=cvover_uv,y=NumOver))+geom_point(aes(color=breakk))+
  scale_color_continuous(breaks=c(5000,50000,100000,300000,500000),labels=c("5000","50000","100000","300000","500000"))+
  ylab("Number of counties over")+xlab("Coefficient of variation in UV daily dose")+xlim(0.09,.15)+ggtitle("Large counties")

under=ggplot(dat,aes(x=cvunder_uv,y=NumUnder))+geom_point(aes(color=breakk))+
  scale_color_continuous(breaks=c(5000,50000,100000,300000,500000),labels=c("5000","50000","100000","300000","500000"))+
  ylab("Number of counties under")+xlab("Coefficient of variation in UV daily dose")+xlim(0.09,.15)+ggtitle("Small counties")

png(glue("{outdir}/cv_uvdailydoes.png"),width=40,height=15,units='cm',res=400)
par(ps=10)
#par(mar=c(1,1,1,1))
par(cex=1)
plot_grid(over,under, ncol=2, align="h",axis = "bt")
dev.off()





a=arrange(new,Melanoma_incidence_pop)
low=a[1:364,] 
high=a[364:727,]

low=a[1:21,] 
high=a[707:727,]

sdlow=sd(low$`NHW melanoma incidence`)
sdhigh=sd(high$`NHW melanoma incidence`)
cvlow=sdlow/mean(low$`NHW melanoma incidence`)
cvhigh=sdhigh/mean(high$`NHW melanoma incidence`)

sdlowUV=sd(low$`UV Daily Dose`)
sdhighUV=sd(high$`UV Daily Dose`)
cvlowUV=sdlowUV/mean(low$`UV Daily Dose`)
cvhighUV=sdhighUV/mean(high$`UV Daily Dose`)

sdlowMED=sd(low$`Households >$50,000`)
sdhighMED=sd(high$`Households >$50,000`)
cvlowMED=sdlowMED/mean(low$`Households >$50,000`)
cvhighMED=sdhighMED/mean(high$`Households >$50,000`)

## new idea
# values=c(72,144,216) # 10, 20, 30%
values=c(72,182) # 10, 25%, decile quartile
trial=new %>% dplyr::select(`UV Daily Dose`,`NHW melanoma incidence`,Melanoma_incidence_pop,`Households >$50,000`) %>% 
  mutate(`UV Daily Dose`=rescale01(`UV Daily Dose`)) %>%  mutate(`NHW melanoma incidence`=rescale01(`NHW melanoma incidence`)) %>%  mutate(Melanoma_incidence_pop=rescale01(Melanoma_incidence_pop)) %>% 
  mutate(`Households >$50,000`=rescale01(`Households >$50,000`))
trial=trial %>% arrange(Melanoma_incidence_pop) %>% mutate("Smallest vs Largest Decile (n=73, 73)"=c(rep("Small",73),rep(NA,581),rep("Large",73))) %>% 
  mutate("Smallest vs Largest Quantile (n=182, 182)"=c(rep("Small",182),rep(NA,363),rep("Large",182))) #%>% 
  #mutate(ThirtyPercent=c(rep("Low",216),rep(NA,295),rep("High",216)))

# test=trial %>% gather(metric,value,-c(TenPercent,TwentyPercent,ThirtyPercent,Melanoma_incidence_pop))
# ggplot(test,aes(x=metric,y=value,color=TenPercent))+geom_boxplot()
# ggplot(test,aes(x=metric,y=value,color=TwentyPercent))+geom_boxplot()

newtest=trial %>% gather(percent,class,-c(`UV Daily Dose`,`NHW melanoma incidence`,Melanoma_incidence_pop,`Households >$50,000`)) %>%
  group_by(class,percent) %>%
  summarise(`UV Daily Dose`=sd(`UV Daily Dose`),`Melanoma incidence`=sd(`NHW melanoma incidence`),`Households >$50,000`=sd(`Households >$50,000`)) %>%
  gather(metric,value,-c(class,percent)) %>% filter(metric!="Households >$50,000") %>% .[complete.cases(.),] %>%ungroup() %>%  mutate(class=as.factor(class)) %>% 
  mutate(class=factor(class,levels=rev(levels(class))))

ggplot(newtest,aes(x=metric,y=value,fill=class))+geom_bar(stat="identity",position = "dodge")+facet_wrap(~percent)+xlab(NULL)+ylab("Normalized standard deviation")+
  scale_fill_manual("County population size",values=c("Small"="blue","Large"="red"))

# newtest2=trial %>% gather(percent,class,-c(`UV Daily Dose`,`NHW melanoma incidence`,Melanoma_incidence_pop,`Households >$50,000`)) %>%
#   group_by(class,percent) %>%
#   summarise(CVUV=sd(`UV Daily Dose`)/mean(`UV Daily Dose`),CVMel=sd(`NHW melanoma incidence`)/mean(`NHW melanoma incidence`),CVwpvr50=sd(`Households >$50,000`)/mean(`Households >$50,000`)) %>%
#   gather(metric,value,-c(class,percent))  %>% .[complete.cases(.),]
# 
# ggplot(newtest2 %>% filter(percent!="TwentyPercent"),aes(x=metric,y=value,fill=class))+geom_bar(stat="identity",position = "dodge")+facet_wrap(~percent)
#        