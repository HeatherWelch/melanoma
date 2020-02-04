### putting it all together
source('/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/utilities/load_libraries.R')
library(mgcv)
spatial_dir="/Users/heatherwelch/Dropbox/melenoma/spatial_files"
outdir="/Users/heatherwelch/Dropbox/melenoma/plots_01_02_20"

modDF=read.csv("/Users/heatherwelch/Dropbox/melenoma/melanoma_GitHub/model_data/data_01_30_20.csv")
#notes on r2: https://stats.stackexchange.com/questions/46345/how-to-calculate-goodness-of-fit-in-glm-r

a=glm(SEER_rate~incm_pc+wpovr50+incm_mh+derm_pk+pcp_pk+docs_pk+wpvr100,data=modDF)
summary(a,correlation=T)
c=summary(a,correlation=T)
d=1-(c$deviance/c$null.deviance) #0.2152378

a=glm(SEER_rate~anRange_temperature+cancer_gov_UV_exposure+mean_cloud+elevation+mean_temperature+seasonality_cloud+seasonality_temperature+sun_exposure+UV_daily_dose+UV_irradiance,data=modDF,family = Gamma)
summary(a,correlation=T)
c=summary(a,correlation=T)
d=1-(c$deviance/c$null.deviance) #0.1107697

library(effects)
plot(predictorEffects(a, xlevels=4))
f=predictorEffects(a) ## this is how to make the plots in ggplot
as.data.frame(f)

g=predict(a,modDF,type="response")
plot(modDF$SEER_rate,g)
# b=lm(SEER_rate~incm_pc+wpovr50,data=modDF)
# https://stackoverflow.com/questions/50413870/non-positive-values-not-allowed-for-the-gamma-family
install.packages("fitdistrplus")
library(fitdistrplus)
descdist(modDF$SEER_rate)

library(caret)
varImp(a)

##autocorrelation
acf(a$residuals)

#overdispersion #aim for <1
e <- resid(a,type="pearson")
N <- nrow(modDF)
p <- length(coef(a))
sum(e^2)/(N-p)

a=gam(SEER_rate~s(incm_pc,bs="ts")+s(wpovr50,bs="ts")+s(incm_mh,bs="ts")+s(derm_pk,bs="ts")+s(pcp_pk,bs="ts")+s(docs_pk,bs="ts")+s(wpvr100,bs="ts"),data=modDF)
summary(a)
AIC(a)
c=summary(a)
d=1-(c$deviance/c$null.deviance)

a=glm(SEER_rate~I(incm_pc^2)+I(wpovr50^2)+I(incm_mh^2)+I(derm_pk^2)+I(pcp_pk^2)+I(docs_pk^2)+I(wpvr100^2),data=modDF)

