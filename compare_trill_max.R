library(foreach)
library(dplyr)
library(nlme)
source("common.R")
source("song_traits_data.R")

print.table(apply(with(song_obs,table(bird=bird,site=location,year=year)) >0,c(2:3),sum), zero.print=".")
print.table(apply(with(song_obs,table(bird=bird,region=region,year=year)) >0,c(2:3),sum), zero.print="XXX")


cat("\n======   \n\nRun models with just 2016 and 2020 data:\n")
song_reduced_obs <- song_obs %>% filter(year %in% c("Y2020","Y2016"))

me_freq_hi_model_set_re_dialect <- function(freq_hi.data=song_obs){
  me_A <- list()
  # No year change
  me_A$object      <- lme(freq_hi~1, data = freq_hi.data, random = ~1|dialect/bird, method="ML")
  me_A$region      <- lme(freq_hi~region, data = freq_hi.data, random = ~1|dialect/bird, method="ML") 
  me_A$year             <- lme(freq_hi~year, data = freq_hi.data, random = ~1|dialect/bird, method="ML")
  me_A$region_year      <- lme(freq_hi~region+year, data = freq_hi.data, random = ~1|dialect/bird, method="ML")
  me_A$regionXyear      <- lme(freq_hi~region*year, data = freq_hi.data, random = ~1|dialect/bird, method="ML")
  me_A
}
me_freq_hi_dialect_re<-me_freq_hi_model_set_re_dialect(song_reduced_obs)
if(suppressWarnings(require("AICcmodavg",quietly = T))){
  write_aictable_form(me_freq_hi_dialect_re, file = "trill_max_freq_aictab.csv")
  
}
print(read.csv("trill_max_freq_aictab.csv"))
print(summary(me_freq_hi_dialect_re$regionXyear))

try({freq_hi_Y2020 <- lme(freq_hi~region*year, data = song_reduced_obs %>% 
                                        mutate(year=factor(year, levels=c("Y2020","Y2016"))), 
                                      random =  ~1|dialect/bird, method="ML");
  print(summary(freq_hi_Y2020))})
