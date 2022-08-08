## step 1: load necessary libraries
## if you are loading these for the first time, you will need to install them first by:
# install.packages("corrplot")
# install.packages("tidyverse")
# install.packages("glue")
library(corrplot)
library(tidyverse)
library(glue)

## step 2: read in dataset
path_to_data="/Users/heatherwelch/Dropbox/melenoma/Ades_new_paper" ## change this to where you keep your data
dat=read.csv(glue("{path_to_data}/cleaned_all_cancers_combined.csv")) ## this is a csv of what you sent me, just the first tab, and with the correlation matrix deleted
head(dat) ## see what you read in

dat2=dat %>% dplyr::select(-c(county,fips)) %>% ## remove the county and fips columns, not part of the correlation
  .[complete.cases(.),] ## remove rows with missing data (correlation matrix will fail if there are NAs)

## step 3: run correlations
dat2_cor=cor(dat2)
dat2_cor ## look at your correlation matrix

## step 4: make your corplot look how you want it
## here is the documentation: https://www.rdocumentation.org/packages/corrplot/versions/0.92/topics/corrplot
corrplot(dat2_cor) # default aesthetics
corrplot(dat2_cor,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9) ## aesthetics from our paper

## step 5: once it looks good, write it out
png(glue("{path_to_data}/corplot.png"),width=15,height=15,type = "cairo",units='cm',res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
corrplot(dat2_cor,type="upper",order="hclust",outline = T,tl.col="black",tl.cex = .9) ## aesthetics from our paper
dev.off()
