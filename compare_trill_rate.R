library(foreach)
library(dplyr)
library(nlme)
source("song_traits_data.R")
source("common.R")

print.table(apply(with(song_obs,table(bird=bird,site=location,year=year)) >0,c(2:3),sum), zero.print=".")
print.table(apply(with(song_obs,table(bird=bird,region=region,year=year)) >0,c(2:3),sum), zero.print="XXX")

cat("\n======   \n\nRun models with just 2016 and 2020 data:\n")
song_reduced_obs <- song_obs %>% filter(year %in% c("Y2020","Y2016"))

me_trill.rate_model_set_re_dialect <- function(trill.rate.data=song_obs){
  me_A <- list()
  me_A$object      <- lme(trill.rate~1, data = trill.rate.data, random = ~1|dialect/bird, method="ML")
  me_A$region      <- lme(trill.rate~region, data = trill.rate.data, random = ~1|dialect/bird, method="ML") 
  me_A$year             <- lme(trill.rate~year, data = trill.rate.data, random = ~1|dialect/bird, method="ML")
  me_A$region_year      <- lme(trill.rate~region+year, data = trill.rate.data, random = ~1|dialect/bird, method="ML")
  me_A$regionXyear      <- lme(trill.rate~region*year, data = trill.rate.data, random = ~1|dialect/bird, method="ML")
  me_A
}
if(require(AICcmodavg)) { 
  print(aictab(me_trill.rate_dialect_re<-me_trill.rate_model_set_re_dialect(song_reduced_obs)))
  write_aictable_form(me_trill.rate_dialect_re, file = "trill_rate_aictab.csv")
} else print(read.csv("trill_rate_aictab.csv"))
  
