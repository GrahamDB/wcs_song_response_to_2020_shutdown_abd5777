## Comparing overall change in instantenous noise between 2015 and 2020 recordings

library(foreach)
library(dplyr)
library(nlme)
library(ggplot2)
library(AICcmodavg)
source("site_information.R")
source("common.R")

SENEL.levels <- read.csv(file="song_amplitude.csv", row.names = 1) %>%
  tibble::rownames_to_column()
song.levels <- SENEL.levels %>% 
  filter(!is.na(distance), !is.na(ambient.LAeq)) %>% 
  mutate(ambient.LAeq2 =ambient.LAeq^2)
noise.5sec.levels <- SENEL.levels %>% 
  rename(ambient_10sec.LAeq =ambient.LAeq,ambient.LAeq =ambient_5sec.LAeq) %>%
  filter( !is.na(ambient.LAeq)) %>% 
  mutate(ambient.LAeq2 =ambient.LAeq^2)
noise.10sec.levels <- SENEL.levels %>% 
  filter( !is.na(ambient.LAeq)) %>% 
  mutate(ambient.LAeq2 =ambient.LAeq^2)

# general noise model set
me_model_set <- function(song.q=quote(song.levels), 
                         random=~1|bird.id,dependent=quote(ambient.LAeq), 
                         include.site=FALSE,
                         include.dist=TRUE){
  me_A <- list()
  # No year change
  me_A$object           <- eval(bquote(lme(.(dependent)~1, data = .(song.q), random = .(random), method="ML") ))
  me_A$region           <- eval(bquote(lme(.(dependent)~region, data = .(song.q), random = .(random), method="ML") ))
  
  me_A$year             <- eval(bquote(lme(.(dependent)~year, data = .(song.q), random = .(random), method="ML") ))
  me_A$region_year      <- eval(bquote(lme(.(dependent)~region+year, data = .(song.q), random = .(random), method="ML") ))
  
  me_A$regionXyear      <- eval(bquote(lme(.(dependent)~region*year, data = .(song.q), random = .(random), method="ML") ))
  
  if(include.dist){
    me_A$dist             <- eval(bquote(lme(.(dependent)~distance, data = .(song.q), random = .(random), method="ML") ))
    me_A$dist_region      <- eval(bquote(lme(.(dependent)~distance+region, data = .(song.q), random = .(random), method="ML") ))
    me_A$distXregion      <- eval(bquote(lme(.(dependent)~distance*region, data = .(song.q), random = .(random), method="ML") ))
    me_A$dist_year        <- eval(bquote(lme(.(dependent)~distance+year, data = .(song.q), random = .(random), method="ML") ))
    me_A$dist_region_year <- eval(bquote(lme(.(dependent)~distance+region+year, data = .(song.q), random = .(random), method="ML") ))
    me_A$distXregion_year <- eval(bquote(lme(.(dependent)~distance*region+year, data = .(song.q), random = .(random), method="ML") ))
    me_A$distXyear        <- eval(bquote(lme(.(dependent)~distance*year, data = .(song.q), random = .(random), method="ML") ))
    me_A$dist_regionXyear <- eval(bquote(lme(.(dependent)~distance+region*year, data = .(song.q), random = .(random), method="ML") ))
    me_A$distXyear_region <- eval(bquote(lme(.(dependent)~distance*year+region, data = .(song.q), random = .(random), method="ML") ))
    me_A$distXregionXyear <- eval(bquote(lme(.(dependent)~distance*region*year, data = .(song.q), random = .(random), method="ML") ))
    
  }
  
  me_A
}


print(aictab(me_song_only <- me_model_set(quote(song.levels))))

print(aictab(me_noise_10s <- me_model_set(quote(noise.10sec.levels), include.dist = F)))

print(aictab(me_noise_5s <- me_model_set(quote(noise.5sec.levels), include.dist = F)))

adv_importance <- function(model.set, parameter, model.tab=aictab(model.set), verbose=TRUE){
  all.parm = unique(unlist(lapply(model.set, function(m) names(coef(m)))))[-1]
  if(length(grep(paste0("^",parameter), names(model.set[[1]]$data)))==1){
    par.key=grep(paste0("^",parameter),all.parm,value = T)[1] 
  }else   {
    par.key = match.arg(parameter,all.parm)
  }
  p_key=sapply(model.set, function(m) par.key %in% names(coef(m)))
  m_parm <- 
    list(m_plus= names(model.set)[p_key],
         m_minus= names(model.set)[!p_key])
  
  int.regex <- paste0(c(par.key,":"),c(":",par.key),collapse = "|") 
  # print(int.regex)
  if(grepl(":",par.key)) 
    warning("Requested importance of interactive parameter! Values may be incorrect!")
  if(any(pX_key<-sapply(model.set, function(m) any(grepl(int.regex, names(coef(m)) )) ))){
    m_parm$m_Splus=names(model.set)[p_key & ! pX_key]
    m_parm$m_Xplus=names(model.set)[p_key & pX_key]
  }
  w_parmX <- 
    sapply(m_parm, 
           function(nL) sum(model.tab[as.character(which(names(model.set) %in% nL)),"AICcWt"]) )
  if(verbose) {
    cat(sprintf("%s W+: %0.03f\n", parameter, w_parmX[1]))
    if(any(pX_key)){
      
      cat(sprintf("%s non-interactive relative weight W+: %0.03f\n", parameter, w_parmX[3]/sum(w_parmX[2:3])))
      
      cat(sprintf("%s interactive relative weight W+: %0.03f\n", parameter, Xrw<-w_parmX[4]/sum(w_parmX[3:4])))
      if(Xrw< 0.4){
        cat(sprintf("Low weight on %s interaction, estimate average B for %s:\n", parameter, par.key))
        # Additive models
        m.tmp=which(names(model.set) %in% m_parm$m_Splus)
        cat(sum(sapply(names(model.set)[m.tmp], function(mN) coef(model.set[[mN]])[[par.key]][1] )* 
                  model.tab[as.character(m.tmp),"AICcWt"]) /sum(w_parmX[2:3]),"\n")
      }
    }
  }
  return(w_parmX)
}

cat("\n\nParameter importance: \n")

print(sapply(c("year","distance","region"), function(p) adv_importance(me_song_only, p)))

print(sapply(c("year","region"), function(p) adv_importance(me_noise_10s, p)))

print(sapply(c("year","region"), function(p) adv_importance(me_noise_5s, p)))


# random effect based on segment, not territory?

print(aictab(fe_noise_10s <- 
               me_model_set(quote(noise.10sec.levels), 
                            random = quote(~1|song.id),
                            include.dist = F)))
print(sapply(c("year","region"), function(p) adv_importance(fe_noise_10s, p)))
print(summary(fe_noise_10s$regionXyear))

print(aictab(fe_noise_10s <- 
               me_model_set(quote(noise.10sec.levels %>% 
                                    mutate(year=factor(year, levels=c("Y2020", "Y2015")),
                                           region=factor(region, levels=c("Rural", "Urban")))), 
                            random = quote(~1|song.id),
                            include.dist = F)))
print(sapply(c("year","region"), function(p) adv_importance(fe_noise_10s, p)))
print(summary(fe_noise_10s$regionXyear))

print(aictab(fe_noise_song <- 
               me_model_set(quote(song.levels %>% 
                                    mutate(year=factor(year, levels=c("Y2020", "Y2015")),
                                           region=factor(region, levels=c("Rural", "Urban")))), 
                            random = quote(~1|song.id),
                            include.dist = F)))
print(sapply(c("year","region"), function(p) adv_importance(fe_noise_song, p)))
print(summary(fe_noise_song$regionXyear))
print(summary(fe_noise_song$region))

print(ggplot(noise.10sec.levels) +geom_boxplot(aes(region,ambient.LAeq,color=year)))
print(ggplot(song.levels) +geom_boxplot(aes(region,ambient.LAeq,color=year)))
