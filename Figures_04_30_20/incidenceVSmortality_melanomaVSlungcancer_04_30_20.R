## incidence vs mortality plot
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
outdir="/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20"
new_final=read.csv("/Users/heatherwelch/Dropbox/melenoma/Figures_04_30_20/master_dataframe_04_30_20.csv") 

lm.mel <- round(summary(lm(Melanoma_mortality ~ Melanoma_incidence,data=new_final))$r.squared,2)
lm.lc <- round(summary(lm(Lung_cancer_mortality ~ Lung_cancer_incidence,data=new_final))$r.squared,2)

mel=new_final %>% dplyr::select(Melanoma_mortality,Melanoma_incidence,Lung_cancer_mortality,Lung_cancer_incidence) %>% 
  .[complete.cases(.),]

cor(mel)

a=ggplot(data=new_final,aes(x=Melanoma_incidence,y=Melanoma_mortality))+geom_point()+theme_classic()+
  geom_smooth(method="lm",se=F,color="red")+ylab("Melanoma mortality")+xlab("Melanoma incidence")+
  annotate("text", Inf, Inf, label = glue("R squared={lm.mel}"), hjust =1.5, vjust = 2.5)

b=ggplot(data=new_final,aes(x=Lung_cancer_incidence,y=Lung_cancer_mortality))+geom_point()+theme_classic()+
  geom_smooth(method="lm",se=F,color="red")+ylab("Lung cancer mortality")+xlab("Lung cancer incidence")+
  annotate("text", Inf, Inf, label = glue("R squared={lm.lc}"), hjust =1.5, vjust = 2.5)

png(glue("{outdir}/Mel_LC.png"),width=20,height=10,units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
plot_grid(a,b,ncol=2)
dev.off()
