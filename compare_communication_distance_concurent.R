# basic communication distance comparisons in concurrent noise levels

song_audibility <- read.csv(file="internal_data/song_audibility.csv")
library(nlme)
library(dplyr)
comm_data <- song_audibility %>% 
  mutate(discr_log=20*log10(max_range), 
         detect_log=discr_log+3) %>% 
  inner_join(read.csv("internal_data/song_amplitude_pub.csv") %>% 
               select(song.id, ambient.LAeq ) ) %>%
  filter(!is.na(ambient.LAeq))
{
  did_mset <- list()
  did_mset$object <- lme(discr_log ~ 1, data=comm_data, random = ~1|bird.id, method="ML")
  did_mset$year <- lme(discr_log ~ year, data=comm_data, random = ~1|bird.id, method="ML")
  did_mset$amb <- lme(discr_log ~ ambient.LAeq, data=comm_data, random = ~1|bird.id, method="ML")
  did_mset$dist <- lme(discr_log ~ distance, data=comm_data, random = ~1|bird.id, method="ML")
  did_mset$year_amb <- lme(discr_log ~ ambient.LAeq+year, data=comm_data, random = ~1|bird.id, method="ML")
  did_mset$dist_amb <- lme(discr_log ~ distance+ ambient.LAeq, data=comm_data, random = ~1|bird.id, method="ML")
  did_mset$dist_year <- lme(discr_log ~ distance+year, data=comm_data, random = ~1|bird.id, method="ML")
  did_mset$dist_year_amb <- lme(discr_log ~ distance+ ambient.LAeq+year, data=comm_data, random = ~1|bird.id, method="ML")
  }
print(did_aic <-sapply(did_mset,AIC))
if(require(AICcmodavg)) {
  source("common.R")
  write_aictable_form(did_mset, file = "comm_conc_aictab.csv") 
  read.csv(file = "comm_conc_aictab.csv")%>% print()
  mapply(FUN = importance,cand.set=list(did_mset)[c(1,1,1)],
         parm=c("distance" , "ambient.LAeq", "yearY2020")) %>% print()
  mapply(FUN = modavg,
         parm=c("distance" , "ambient.LAeq", "yearY2020"),
         MoreArgs = list(cand.set=did_mset)) %>% print()
  summary(did_mset$dist_year_amb) %>% print()
}

{
  ded_mset <- list()
  ded_mset$object <- lme(detect_log ~ 1, data=comm_data, random = ~1|bird.id, method="ML")
  ded_mset$year <- lme(detect_log ~ year, data=comm_data, random = ~1|bird.id, method="ML")
  ded_mset$amb <- lme(detect_log ~ ambient.LAeq, data=comm_data, random = ~1|bird.id, method="ML")
  ded_mset$dist <- lme(detect_log ~ distance, data=comm_data, random = ~1|bird.id, method="ML")
  ded_mset$year_amb <- lme(detect_log ~ ambient.LAeq+year, data=comm_data, random = ~1|bird.id, method="ML")
  ded_mset$dist_amb <- lme(detect_log ~ distance+ ambient.LAeq, data=comm_data, random = ~1|bird.id, method="ML")
  ded_mset$dist_year <- lme(detect_log ~ distance+year, data=comm_data, random = ~1|bird.id, method="ML")
  ded_mset$dist_year_amb <- lme(detect_log ~ distance+ ambient.LAeq+year, data=comm_data, random = ~1|bird.id, method="ML")
}
print(did_aic <-sapply(ded_mset,AIC))
if(require(AICcmodavg)) {
  source("common.R")
  write_aictable_form(ded_mset, file = "detect_conc_aictab.csv") 
  read.csv(file = "detect_conc_aictab.csv")%>% print()
  mapply(FUN = importance,cand.set=list(ded_mset)[c(1,1,1)],
         parm=c("distance" , "ambient.LAeq", "yearY2020")) %>% print()
  mapply(FUN = modavg,
         parm=c("distance" , "ambient.LAeq", "yearY2020"),
         MoreArgs = list(cand.set=ded_mset)) %>% print()
  summary(ded_mset$dist_year_amb) %>% print()
}
