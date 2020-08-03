library(foreach)
library(dplyr)
library(nlme)
source("common.R")
source("song_traits_data.R")

print.table(apply(with(song_obs,table(bird=bird,site=location,year=year)) >0,c(2:3),sum), zero.print=".")
print.table(apply(with(song_obs,table(bird=bird,region=region,year=year)) >0,c(2:3),sum), zero.print="XXX")


cat("\n======   \n\nRun models with just 2016 and 2020 data:\n")
song_reduced_obs <- song_obs %>% filter(year %in% c("Y2020","Y2016"))

me_trill.bw_model_set_re_dialect <- function(trill.bw.data=song_obs){
  me_A <- list()
  # No year change
  me_A$object      <- lme(trill.bw~1, data = trill.bw.data, random = ~1|dialect/bird, method="ML")
  me_A$region      <- lme(trill.bw~region, data = trill.bw.data, random = ~1|dialect/bird, method="ML") 
  me_A$year             <- lme(trill.bw~year, data = trill.bw.data, random = ~1|dialect/bird, method="ML")
  me_A$region_year      <- lme(trill.bw~region+year, data = trill.bw.data, random = ~1|dialect/bird, method="ML")
  me_A$regionXyear      <- lme(trill.bw~region*year, data = trill.bw.data, random = ~1|dialect/bird, method="ML")
  me_A
}
me_trill.bw_dialect_re<-me_trill.bw_model_set_re_dialect(song_reduced_obs)
if(suppressWarnings(require("AICcmodavg",quietly = T))){
  
  write_aictable_form(me_trill.bw_dialect_re, file = "trill_freq_bw_aictab.csv")
}
print(read.csv("trill_freq_bw_aictab.csv"))
print(summary(me_trill.bw_dialect_re$regionXyear))


try({trill.bw_Y2020 <- lme(trill.bw~region*year, data = song_reduced_obs %>%
                                        mutate(year=factor(year, levels=c("Y2020","Y2016"))),
                                      random =  ~1|dialect/bird, method="ML");
  print(summary(trill.bw_Y2020))})
try({trill.bw_Rural <- lme(trill.bw~region*year, data = song_reduced_obs %>% 
                       mutate(region=factor(region, levels=c("Rural","Urban"))), 
                     random =  ~1|dialect/bird, method="ML");
print(summary(trill.bw_Rural))})
