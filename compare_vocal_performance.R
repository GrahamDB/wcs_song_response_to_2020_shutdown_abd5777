library(foreach)
library(dplyr)
library(nlme)
source("song_traits_data.R")
source("common.R")

print.table(apply(with(song_obs,table(bird=bird,site=location,year=year)) >0,c(2:3),sum), zero.print=".")
print.table(apply(with(song_obs,table(bird=bird,region=region,year=year)) >0,c(2:3),sum), zero.print="XXX")

cat("\n======   \n\nRun models with just 2016 and 2020 data:\n")
song_reduced_obs <- song_obs %>% filter(year %in% c("Y2020","Y2016"))

me_vp_model_set_re_dialect <- function(vp.data=song_obs){
  me_A <- list()
  me_A$object      <- lme(vp~1, data = vp.data, random = ~1|dialect/bird, method="ML")
  me_A$region      <- lme(vp~region, data = vp.data, random = ~1|dialect/bird, method="ML") 
  me_A$year             <- lme(vp~year, data = vp.data, random = ~1|dialect/bird, method="ML")
  me_A$region_year      <- lme(vp~region+year, data = vp.data, random = ~1|dialect/bird, method="ML")
  me_A$regionXyear      <- lme(vp~region*year, data = vp.data, random = ~1|dialect/bird, method="ML")
  me_A
}

me_vp_dialect_re<-me_vp_model_set_re_dialect(song_reduced_obs)
if(require(AICcmodavg)) { print(aictab(me_vp_dialect_re))

write_aictable_form(me_vp_dialect_re, file = "vp_aictab.csv")
} else {
  print(read.csv("vp_aictab.csv"))
}

print(summary(me_vp_dialect_re$regionXyear))
try({vp_Rural <- lme(vp~region*year, data = song_reduced_obs %>% 
                            mutate(region=factor(region, levels=c("Rural","Urban"))), 
                          random =  ~1|dialect/bird, method="ML");
print(summary(vp_Rural))})
