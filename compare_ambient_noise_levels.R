library(dplyr)
source("site_information.R")
source("common.R");
tr_ambient_LAeq <- read.csv("territory_noise_pub.csv", row.names = 1) %>% tibble::rownames_to_column()
tr_ambient_LAF90 <- tr_ambient_LAeq %>% filter(!is.na(LAF90))



cat("\n\n\nSample count, LAeq\n")
print(with(tr_ambient_LAeq,table(Region=Region,Year=year.val)), zero.print=".")
print(with(tr_ambient_LAeq,table(Location=Location,Year=year.val)), zero.print=".")

cat("\n\n\nSample count, LAF90\n")
print(with(tr_ambient_LAF90,table(Region=Region,Year=year.val)), zero.print=".")
print(with(tr_ambient_LAF90,table(Location=Location,Year=year.val)), zero.print=".")

# Random effects models, avoid over sampling


library(nlme)
me.LAeq <- list()
me.LAeq$object <- lme(LAeq~1, random = ~1|Location, data=tr_ambient_LAeq, method = "ML")
me.LAeq$Y <- lme(LAeq~year.val, random = ~1|Location, data=tr_ambient_LAeq, method = "ML")
me.LAeq$R <- lme(LAeq~Region, random = ~1|Location, data=tr_ambient_LAeq, method = "ML")
me.LAeq$RuY <- lme(LAeq~Region+year.val, random = ~1|Location, data=tr_ambient_LAeq, method = "ML")
me.LAeq$RxY <- lme(LAeq~Region*year.val, random = ~1|Location, data=tr_ambient_LAeq, method = "ML")
me.LAF90 <- list()
me.LAF90$object <- lme(LAF90~1, random = ~1|Location, data=tr_ambient_LAF90, method = "ML")
me.LAF90$Y <- lme(LAF90~year.val, random = ~1|Location, data=tr_ambient_LAF90, method = "ML")
me.LAF90$R <- lme(LAF90~Region, random = ~1|Location, data=tr_ambient_LAF90, method = "ML")
me.LAF90$RuY <- lme(LAF90~Region+year.val, random = ~1|Location, data=tr_ambient_LAF90, method = "ML")
me.LAF90$RxY <- lme(LAF90~Region*year.val, random = ~1|Location, data=tr_ambient_LAF90, method = "ML")

cat("\n\n\nMixed Effect Model AIC, LAeq, grouped by site\n")
print(sapply(me.LAeq, AIC))
cat("\n\n\nMixed Effect Model AIC, LAF90, grouped by site\n")
print(sapply(me.LAF90, AIC))
print(summary(me.LAeq$RxY))
print(summary(lme(LAeq~Region*year.val, random = ~1|Location, data=tr_ambient_LAeq %>% 
                    mutate(year.val = factor(year.val, levels= c("Y2020", "Y2016"))), method = "ML")))
print(summary(lme(LAeq~Region*year.val, random = ~1|Location, data=tr_ambient_LAeq %>% 
                    mutate(Region = factor(Region, levels= c("Urban", "Rural"))), method = "ML")))
print(summary(me.LAF90$RxY))
print(summary(lme(LAF90~Region*year.val, random = ~1|Location, data=tr_ambient_LAF90 %>% 
              mutate(year.val = factor(year.val, levels= c("Y2020", "Y2016"))), method = "ML")))
print(summary(lme(LAF90~Region*year.val, random = ~1|Location, data=tr_ambient_LAF90 %>% 
                    mutate(Region = factor(Region, levels= c("Urban", "Rural"))), method = "ML")))

if(require(AICcmodavg)){
  cat("Ambient models:")
  print(tmp <-aictab(me.LAeq))
  # write.csv(tmp,"ambient_aictab.csv")
  write_aictable_form(me.LAeq, file = "ambient_aictab.csv")
  cat("Background models:")
  print(tmp<-aictab(me.LAF90))
  # write.csv(tmp,"background_aictab.csv")
  write_aictable_form(me.LAF90, file = "background_aictab.csv")
  
}

show_prediction2 <- 
  rbind(
    as.data.frame(expand.grid(Region=c("Rural"),year.val=c("Y2016","Y2020"),Location=site_regions$Rural)),
    as.data.frame(expand.grid(Region=c("Urban"),year.val=c("Y2016","Y2020"),Location=site_regions$Urban)))
    
show_prediction2$LAF90 <- predict(me.LAF90$RxY, newdata=show_prediction2) 
show_prediction2$LAeq <- predict(me.LAeq$RxY, newdata=show_prediction2) 

print(show_prediction2)

