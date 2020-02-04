library(tidyverse)
library(glue)
library(foreach)
library(doParallel, quietly = TRUE)
outdir="/Users/heatherwelch/Dropbox/plots_02_04_20"

# individual map function with glue and inline variables ###
# full disclosure IDK what type="cairo" means but mclapply and foreach would not work without it: https://stackoverflow.com/questions/57682250/r-the-foreach-does-not-work-with-exporting-figure-out-such-as-png-or-ggsave

# write ggplotting function with inline variables
mappfunction=function(mapVar){
  map=ggplot(data=mtcars,aes(x=mpg,y=.data[[mapVar]]))+geom_point()
  
  png(glue("{outdir}/{mapVar}.png"),width=36,height=22,type = "cairo",units='cm',res=400)
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  print({map})
  dev.off()
}

# test function
mappfunction(mapVar = "wt")
lapply(yVars,FUN=mappfunction) ## that works

# parallel ###
yVars=c("cyl","disp","drat")

# mclapply with two cores
mclapply(yVars,FUN=mappfunction,mc.cores = 2)

# foreach with two cores
registerDoParallel(2)
system.time(print(
  foreach(i=1:length(yVars),.export = c("mappfunction","mtcars","outdir","yVars"),.packages = c("ggplot2","glue"),.verbose=T) %dopar% {
    mappfunction(yVars[i])
  }
))
