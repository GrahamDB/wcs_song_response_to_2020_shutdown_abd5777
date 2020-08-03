library(foreach)
library(dplyr)
library(nlme)
source("common.R")
source("song_traits_data.R")

print.table(apply(with(song_obs,table(bird=bird,site=location,year=year)) >0,c(2:3),sum), zero.print=".")
print.table(apply(with(song_obs,table(bird=bird,region=region,year=year)) >0,c(2:3),sum), zero.print="XXX")


cat("\n======   \n\nRun models with just 2016 and 2020 data:\n")
song_reduced_obs <- song_obs %>% filter(year %in% c("Y2020","Y2016"))

me_freq_lo_model_set_re_dialect <- function(freq_lo.data=song_obs){
  me_A <- list()
  # No year change
  me_A$object      <- lme(freq_lo~1, data = freq_lo.data, random = ~1|dialect/bird, method="ML")
  me_A$region      <- lme(freq_lo~region, data = freq_lo.data, random = ~1|dialect/bird, method="ML") 
  me_A$year             <- lme(freq_lo~year, data = freq_lo.data, random = ~1|dialect/bird, method="ML")
  me_A$region_year      <- lme(freq_lo~region+year, data = freq_lo.data, random = ~1|dialect/bird, method="ML")
  me_A$regionXyear      <- lme(freq_lo~region*year, data = freq_lo.data, random = ~1|dialect/bird, method="ML")
  me_A
}
me_freq_lo_dialect_re<-me_freq_lo_model_set_re_dialect(song_reduced_obs)
if(suppressWarnings(require("AICcmodavg",quietly = T))){
  write_aictable_form(me_freq_lo_dialect_re, file = "trill_min_freq_aictab.csv")
  
}
print(read.csv("trill_min_freq_aictab.csv"))
print(summary(me_freq_lo_dialect_re$regionXyear))


try({freq_lo_Y2020 <- lme(freq_lo~region*year, data = song_reduced_obs %>% 
                                        mutate(year=factor(year, levels=c("Y2020","Y2016"))), 
                                      random =  ~1|dialect/bird, method="ML");
  print(summary(freq_lo_Y2020))})
try({freq_lo_Rural <- lme(freq_lo~region*year, data = song_reduced_obs %>% 
                            mutate(region=factor(region, levels=c("Rural","Urban"))), 
                          random =  ~1|dialect/bird, method="ML");
print(summary(freq_lo_Rural))})

try({freq_lo_RuralY2020 <- lme(freq_lo~region*year, data = song_reduced_obs %>% 
                            mutate(year=factor(year, levels=c("Y2020","Y2016")),
                                   region=factor(region, levels=c("Rural","Urban"))), 
                          random =  ~1|dialect/bird, method="ML");
print(summary(freq_lo_RuralY2020))})
