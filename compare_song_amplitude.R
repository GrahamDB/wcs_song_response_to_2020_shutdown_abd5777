library(foreach)
library(dplyr)
library(nlme)
library(ggplot2)
source("site_information.R")
source("common.R");

song.snr.v2 <-  read.csv(file="song_amplitude_pub.csv", row.names = 1) %>%
  tibble::rownames_to_column()
song.levels <- song.snr.v2
song.amb.levels <- song.snr.v2 %>% filter(!is.na(ambient.LAeq))


print.table(apply(with(song.levels,table(bird=bird.id,site=location,year=year)) >0,c(2:3),sum), zero.print=".")
print.table(apply(with(song.levels,table(bird=bird.id,region=region,year=year)) >0,c(2:3),sum), zero.print="XXX")
print.table(apply(with(song.levels,table(bird=bird.id,region=region,dist=cut_number(distance, 4),year=year)) >0,c(2:4),sum), zero.print=".")
print.table(apply(with(song.levels,table(bird=bird.id,region=region,dist=cut_interval(distance, 4),year=year)) >0,c(2:4),sum), zero.print=".")
print.table(with(song.levels,table(region=region,dist=cut_interval(distance, 8),year=year)), zero.print=".")
song.levels_sampling <- song.levels %>% mutate(dist=cut_interval(distance, 8), bird.id=factor(bird.id))
print.table(with(song.levels_sampling %>% filter(year %in% "Y2015"),table(bird=factor(bird.id),dist=dist)), zero.print=".")
print.table(with(song.levels_sampling %>% filter(year %in% "Y2020"),table(bird=factor(bird.id),dist=dist)), zero.print=".")

me_model_set <- function(song.levels=song.levels, include.site=FALSE, include.ambient=TRUE, include.region=TRUE, include.ambient2=FALSE){
me_A <- list()
# No year change
me_A$object      <- lme(SEL.LZeq~1, data = song.levels, random = ~1|bird.id, method="ML")
me_A$dist        <- lme(SEL.LZeq~distance, data = song.levels, random = ~1|bird.id, method="ML")
if(include.region) me_A$region      <- lme(SEL.LZeq~region, data = song.levels, random = ~1|bird.id, method="ML")
if(include.site)
me_A$site        <- lme(SEL.LZeq~location, data = song.levels, random = ~1|bird.id, method="ML")

if(include.region) me_A$dist_region <- lme(SEL.LZeq~region+distance, data = song.levels, random = ~1|bird.id, method="ML")
if(include.site)
me_A$dist_site   <- lme(SEL.LZeq~location+distance, data = song.levels, random = ~1|bird.id, method="ML")

if(include.region) me_A$distXregion      <- lme(SEL.LZeq~region*distance, data = song.levels, random = ~1|bird.id, method="ML")
if(include.site)
me_A$distXsite <- lme(SEL.LZeq~location*distance, data = song.levels, random = ~1|bird.id, method="ML") #Not enough data

# Year change (delta)
me_A$year             <- lme(SEL.LZeq~year, data = song.levels, random = ~1|bird.id, method="ML")
me_A$dist_year        <- lme(SEL.LZeq~distance+year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.region) me_A$region_year      <- lme(SEL.LZeq~region+year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.site)
me_A$site_year        <- lme(SEL.LZeq~location+year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.region) me_A$dist_region_year <- lme(SEL.LZeq~region+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
# if(include.region) me_A$dist_region_year <- lme(SEL.LZeq~region+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.site) me_A$dist_site_year   <- lme(SEL.LZeq~location+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.region) me_A$distXregion_year    <- lme(SEL.LZeq~region*distance+year, data = song.levels, random = ~1|bird.id, method="ML")
# if(include.region) me_A$distXregion_year <- lme(SEL.LZeq~region*distance+year, data = song.levels, random = ~1|bird.id, method="ML")


# Year change (interaction)
me_A$distXyear        <- lme(SEL.LZeq~distance*year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.region) me_A$regionXyear      <- lme(SEL.LZeq~region*year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.region) me_A$dist_regionXyear <- lme(SEL.LZeq~distance+region*year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.region) me_A$region_distXyear <- lme(SEL.LZeq~region+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.region) me_A$regionXdistXyear <- lme(SEL.LZeq~region*distance*year, data = song.levels, random = ~1|bird.id, method="ML")

if(include.ambient){
  me_A$amb         <- lme(SEL.LZeq~ambient.LAeq, data = song.levels, random = ~1|bird.id, method="ML")
  me_A$dist_amb    <- lme(SEL.LZeq~ambient.LAeq+distance, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$distXamb         <- lme(SEL.LZeq~ambient.LAeq*distance, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2){
    me_A$amb2         <- lme(SEL.LZeq~ambient.LAeq+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
    me_A$dist_amb2   <- lme(SEL.LZeq~ambient.LAeq+ambient.LAeq2+distance, data = song.levels, random = ~1|bird.id, method="ML")
    # me_A$distXamb_amb2    <- lme(SEL.LZeq~ambient.LAeq*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
  }
  if(include.region){
  me_A$region_amb  <- lme(SEL.LZeq~region+ambient.LAeq, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$region_amb2 <- lme(SEL.LZeq~region+ambient.LAeq+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
  me_A$dist_region_amb  <- lme(SEL.LZeq~region+ambient.LAeq+distance, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$dist_region_amb2 <- lme(SEL.LZeq~region+ambient.LAeq+ambient.LAeq2+distance, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$regionXamb       <- lme(SEL.LZeq~region*ambient.LAeq, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.ambient2)me_A$regionXamb_amb2  <- lme(SEL.LZeq~region*ambient.LAeq+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
  me_A$distXregion_amb  <- lme(SEL.LZeq~region*distance+ambient.LAeq, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$dist_regionXamb  <- lme(SEL.LZeq~region*ambient.LAeq+distance, data = song.levels, random = ~1|bird.id, method="ML")
  me_A$distXamb_region  <- lme(SEL.LZeq~region+ambient.LAeq*distance, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$distXregionXamb  <- lme(SEL.LZeq~region*ambient.LAeq*distance, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$distXregion_amb2  <- lme(SEL.LZeq~region*distance+ambient.LAeq+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.ambient2)me_A$dist_regionXamb_amb2  <- lme(SEL.LZeq~region*ambient.LAeq+distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$distXamb_region_amb2  <- lme(SEL.LZeq~region+ambient.LAeq*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.ambient2)me_A$distXregionXamb_amb2  <- lme(SEL.LZeq~region*ambient.LAeq*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
  }
  me_A$amb_year         <- lme(SEL.LZeq~ambient.LAeq+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$amb2_year        <- lme(SEL.LZeq~ambient.LAeq+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
  me_A$dist_amb_year    <- lme(SEL.LZeq~ambient.LAeq+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$dist_amb2_year   <- lme(SEL.LZeq~ambient.LAeq+ambient.LAeq2+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$distXamb_year       <- lme(SEL.LZeq~ambient.LAeq*distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.ambient2)me_A$distXamb_amb2_year  <- lme(SEL.LZeq~ambient.LAeq*distance+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.region){
    me_A$region_amb_year  <- lme(SEL.LZeq~region+ambient.LAeq+year, data = song.levels, random = ~1|bird.id, method="ML")
    if(include.ambient2) me_A$region_amb2_year <- lme(SEL.LZeq~region+ambient.LAeq+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
  me_A$dist_region_amb_year  <- lme(SEL.LZeq~region+ambient.LAeq+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$dist_region_amb2_year <- lme(SEL.LZeq~region+ambient.LAeq+ambient.LAeq2+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$regionXamb_year     <- lme(SEL.LZeq~region*ambient.LAeq+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$regionXamb_amb2_year<- lme(SEL.LZeq~region*ambient.LAeq+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
  me_A$distXregion_amb_year<- lme(SEL.LZeq~region*distance+ambient.LAeq+year, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$dist_regionXamb_year<- lme(SEL.LZeq~region*ambient.LAeq+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$distXamb_region_year<- lme(SEL.LZeq~region+ambient.LAeq*distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$distXregionXamb_year<- lme(SEL.LZeq~region*ambient.LAeq*distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$distXregion_amb2_year<- lme(SEL.LZeq~region*distance+ambient.LAeq+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.ambient2)me_A$dist_regionXamb_amb2_year<- lme(SEL.LZeq~region*ambient.LAeq+distance+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.ambient2)me_A$distXamb_region_amb2_year<- lme(SEL.LZeq~region+ambient.LAeq*distance+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.ambient2)me_A$distXregionXamb_amb2_year<- lme(SEL.LZeq~region*ambient.LAeq*distance+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
  }
  # me_A$ambXyear         <- lme(SEL.LZeq~ambient.LAeq*year, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.ambient2)me_A$ambXyear_amb2        <- lme(SEL.LZeq~ambient.LAeq*year+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
  
  me_A$distXyear_amb    <- lme(SEL.LZeq~ambient.LAeq+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.ambient2)me_A$distXyear_amb2    <- lme(SEL.LZeq~ambient.LAeq+ambient.LAeq2+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$dist_yearXamb    <- lme(SEL.LZeq~ambient.LAeq*year+distance, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.ambient2)me_A$dist_yearXamb_amb2    <- lme(SEL.LZeq~ambient.LAeq*year+ambient.LAeq2+distance, data = song.levels, random = ~1|bird.id, method="ML")
# me_A$distXyearXamb    <- lme(SEL.LZeq~ambient.LAeq*year*distance, data = song.levels, random = ~1|bird.id, method="ML")
# if(include.ambient2)me_A$distXyearXamb_amb2    <- lme(SEL.LZeq~ambient.LAeq*year*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
if(include.region){
me_A$regionXyear_amb    <- lme(SEL.LZeq~ambient.LAeq+region*year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.ambient2)me_A$regionXyear_amb2    <- lme(SEL.LZeq~ambient.LAeq+ambient.LAeq2+region*year, data = song.levels, random = ~1|bird.id, method="ML")
# me_A$region_yearXamb    <- lme(SEL.LZeq~ambient.LAeq*year+region, data = song.levels, random = ~1|bird.id, method="ML")
# if(include.ambient2)me_A$region_yearXamb_amb2    <- lme(SEL.LZeq~ambient.LAeq*year+ambient.LAeq2+region, data = song.levels, random = ~1|bird.id, method="ML")
# me_A$regionXyearXamb    <- lme(SEL.LZeq~ambient.LAeq*year*region, data = song.levels, random = ~1|bird.id, method="ML")
# if(include.ambient2)me_A$regionXyearXamb_amb2    <- lme(SEL.LZeq~ambient.LAeq*year*region+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")


# me_A$region_dist_yearXamb    <- lme(SEL.LZeq~region+ambient.LAeq*year+distance, data = song.levels, random = ~1|bird.id, method="ML")
# if(include.ambient2)me_A$region_dist_yearXamb_amb2    <- lme(SEL.LZeq~region+ambient.LAeq*year+ambient.LAeq2+distance, data = song.levels, random = ~1|bird.id, method="ML")

me_A$region_distXyear_amb    <- lme(SEL.LZeq~region+ambient.LAeq+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.ambient2)me_A$region_distXyear_amb2    <- lme(SEL.LZeq~region+ambient.LAeq+ambient.LAeq2+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
# me_A$region_distXyearXamb    <- lme(SEL.LZeq~region+ambient.LAeq*year*distance, data = song.levels, random = ~1|bird.id, method="ML")
# if(include.ambient2)me_A$region_distXyearXamb_amb2    <- lme(SEL.LZeq~region+ambient.LAeq*year*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")

me_A$dist_regionXyear_amb    <- lme(SEL.LZeq~distance+ambient.LAeq+region*year, data = song.levels, random = ~1|bird.id, method="ML")
if(include.ambient2)me_A$dist_regionXyear_amb2    <- lme(SEL.LZeq~distance+ambient.LAeq+ambient.LAeq2+region*year, data = song.levels, random = ~1|bird.id, method="ML")
# me_A$dist_regionXyearXamb    <- lme(SEL.LZeq~distance+ambient.LAeq*year*region, data = song.levels, random = ~1|bird.id, method="ML")
# if(include.ambient2)me_A$dist_regionXyearXamb_amb2    <- lme(SEL.LZeq~distance+ambient.LAeq*year*region+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
}
}
# me_A$dist_region_amb_year  <- lme(SEL.LZeq~region+ambient.LAeq+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
# me_A$dist_region_amb2_year <- lme(SEL.LZeq~region+ambient.LAeq+ambient.LAeq2+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
me_A
}


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


me_amb <- me_model_set(song.amb.levels, include.ambient = T, include.region = F)


if(suppressWarnings( require("AICcmodavg", quietly = T))) {
write_aictable_form(me_amb, file = "SEL_aictab.csv")
}
print(me_amb_aic<- read.csv(file = "SEL_aictab.csv"))
print(sapply(c("year","distance","ambient.LAeq"), function(p) adv_importance(me_amb, p,model.tab = me_amb_aic)))

print(summary(me_amb[[as.character(me_amb_aic$Modnames[1])]]))

# Comparing song SNR values

me_model_set_SNR <- function(song.levels=song.snr.v2,song.band=quote(song.bands.OBCZ4000.SNR),
                             include.site=FALSE, include.ambient=TRUE, include.region=FALSE, include.ambient2=FALSE){
  me_A <- list()
  # No year change
  me_A$object      <- lme(song.bands.OBCZ4000.SNR~1, data = song.levels, random = ~1|bird.id, method="ML")
  me_A$dist        <- lme(song.bands.OBCZ4000.SNR~distance, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.region) me_A$region      <- lme(song.bands.OBCZ4000.SNR~region, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.site)
    me_A$site        <- lme(song.bands.OBCZ4000.SNR~location, data = song.levels, random = ~1|bird.id, method="ML")
  
  if(include.region) me_A$dist_region <- lme(song.bands.OBCZ4000.SNR~region+distance, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.site)
    me_A$dist_site   <- lme(song.bands.OBCZ4000.SNR~location+distance, data = song.levels, random = ~1|bird.id, method="ML")
  
  if(include.region) me_A$distXregion      <- lme(song.bands.OBCZ4000.SNR~region*distance, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.site)
    me_A$distXsite <- lme(song.bands.OBCZ4000.SNR~location*distance, data = song.levels, random = ~1|bird.id, method="ML") #Not enough data
  
  # Year change (delta)
  me_A$year             <- lme(song.bands.OBCZ4000.SNR~year, data = song.levels, random = ~1|bird.id, method="ML")
  me_A$dist_year        <- lme(song.bands.OBCZ4000.SNR~distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.region) me_A$region_year      <- lme(song.bands.OBCZ4000.SNR~region+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.site)
    me_A$site_year        <- lme(song.bands.OBCZ4000.SNR~location+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.region) me_A$dist_region_year <- lme(song.bands.OBCZ4000.SNR~region+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.region) me_A$dist_region_year <- lme(song.bands.OBCZ4000.SNR~region+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.site) me_A$dist_site_year   <- lme(song.bands.OBCZ4000.SNR~location+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.region) me_A$distXregion_year    <- lme(song.bands.OBCZ4000.SNR~region*distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  # if(include.region) me_A$distXregion_year <- lme(song.bands.OBCZ4000.SNR~region*distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  
  
  # Year change (interaction)
  me_A$distXyear        <- lme(song.bands.OBCZ4000.SNR~distance*year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.region) me_A$regionXyear      <- lme(song.bands.OBCZ4000.SNR~region*year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.region) me_A$dist_regionXyear <- lme(song.bands.OBCZ4000.SNR~distance+region*year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.region) me_A$region_distXyear <- lme(song.bands.OBCZ4000.SNR~region+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
  if(include.region) me_A$regionXdistXyear <- lme(song.bands.OBCZ4000.SNR~region*distance*year, data = song.levels, random = ~1|bird.id, method="ML")
  
  if(include.ambient){
    me_A$amb         <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq, data = song.levels, random = ~1|bird.id, method="ML")
    me_A$dist_amb    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+distance, data = song.levels, random = ~1|bird.id, method="ML")
    # me_A$distXamb         <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*distance, data = song.levels, random = ~1|bird.id, method="ML")
    if(include.ambient2){
      me_A$amb2         <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
      me_A$dist_amb2   <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+ambient.LAeq2+distance, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$distXamb_amb2    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
    }
    if(include.region){
      me_A$region_amb  <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$region_amb2 <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
      me_A$dist_region_amb  <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+distance, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$dist_region_amb2 <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+ambient.LAeq2+distance, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$regionXamb       <- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$regionXamb_amb2  <- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
      me_A$distXregion_amb  <- lme(song.bands.OBCZ4000.SNR~region*distance+ambient.LAeq, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$dist_regionXamb  <- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq+distance, data = song.levels, random = ~1|bird.id, method="ML")
      me_A$distXamb_region  <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq*distance, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$distXregionXamb  <- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq*distance, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$distXregion_amb2  <- lme(song.bands.OBCZ4000.SNR~region*distance+ambient.LAeq+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$dist_regionXamb_amb2  <- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq+distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$distXamb_region_amb2  <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$distXregionXamb_amb2  <- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
    }
    me_A$amb_year         <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+year, data = song.levels, random = ~1|bird.id, method="ML")
    if(include.ambient2)me_A$amb2_year        <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
    me_A$dist_amb_year    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
    if(include.ambient2)me_A$dist_amb2_year   <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+ambient.LAeq2+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
    # me_A$distXamb_year       <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*distance+year, data = song.levels, random = ~1|bird.id, method="ML")
    # if(include.ambient2)me_A$distXamb_amb2_year  <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*distance+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
    if(include.region){
      me_A$region_amb_year  <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+year, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2) me_A$region_amb2_year <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
      me_A$dist_region_amb_year  <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$dist_region_amb2_year <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+ambient.LAeq2+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$regionXamb_year     <- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq+year, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$regionXamb_amb2_year<- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
      me_A$distXregion_amb_year<- lme(song.bands.OBCZ4000.SNR~region*distance+ambient.LAeq+year, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$dist_regionXamb_year<- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$distXamb_region_year<- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq*distance+year, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$distXregionXamb_year<- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq*distance+year, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$distXregion_amb2_year<- lme(song.bands.OBCZ4000.SNR~region*distance+ambient.LAeq+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$dist_regionXamb_amb2_year<- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq+distance+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$distXamb_region_amb2_year<- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq*distance+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$distXregionXamb_amb2_year<- lme(song.bands.OBCZ4000.SNR~region*ambient.LAeq*distance+ambient.LAeq2+year, data = song.levels, random = ~1|bird.id, method="ML")
    }
    # me_A$ambXyear         <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year, data = song.levels, random = ~1|bird.id, method="ML")
    # if(include.ambient2)me_A$ambXyear_amb2        <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
    
    me_A$distXyear_amb    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
    if(include.ambient2)me_A$distXyear_amb2    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+ambient.LAeq2+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
    # me_A$dist_yearXamb    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year+distance, data = song.levels, random = ~1|bird.id, method="ML")
    # if(include.ambient2)me_A$dist_yearXamb_amb2    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year+ambient.LAeq2+distance, data = song.levels, random = ~1|bird.id, method="ML")
    # me_A$distXyearXamb    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year*distance, data = song.levels, random = ~1|bird.id, method="ML")
    # if(include.ambient2)me_A$distXyearXamb_amb2    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
    if(include.region){
      me_A$regionXyear_amb    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+region*year, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$regionXyear_amb2    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq+ambient.LAeq2+region*year, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$region_yearXamb    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year+region, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$region_yearXamb_amb2    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year+ambient.LAeq2+region, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$regionXyearXamb    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year*region, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$regionXyearXamb_amb2    <- lme(song.bands.OBCZ4000.SNR~ambient.LAeq*year*region+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
      
      
      # me_A$region_dist_yearXamb    <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq*year+distance, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$region_dist_yearXamb_amb2    <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq*year+ambient.LAeq2+distance, data = song.levels, random = ~1|bird.id, method="ML")
      
      me_A$region_distXyear_amb    <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$region_distXyear_amb2    <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+ambient.LAeq2+distance*year, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$region_distXyearXamb    <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq*year*distance, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$region_distXyearXamb_amb2    <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq*year*distance+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
      
      me_A$dist_regionXyear_amb    <- lme(song.bands.OBCZ4000.SNR~distance+ambient.LAeq+region*year, data = song.levels, random = ~1|bird.id, method="ML")
      if(include.ambient2)me_A$dist_regionXyear_amb2    <- lme(song.bands.OBCZ4000.SNR~distance+ambient.LAeq+ambient.LAeq2+region*year, data = song.levels, random = ~1|bird.id, method="ML")
      # me_A$dist_regionXyearXamb    <- lme(song.bands.OBCZ4000.SNR~distance+ambient.LAeq*year*region, data = song.levels, random = ~1|bird.id, method="ML")
      # if(include.ambient2)me_A$dist_regionXyearXamb_amb2    <- lme(song.bands.OBCZ4000.SNR~distance+ambient.LAeq*year*region+ambient.LAeq2, data = song.levels, random = ~1|bird.id, method="ML")
    }
  }
  # me_A$dist_region_amb_year  <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  # me_A$dist_region_amb2_year <- lme(song.bands.OBCZ4000.SNR~region+ambient.LAeq+ambient.LAeq2+distance+year, data = song.levels, random = ~1|bird.id, method="ML")
  me_A
}
me_SNR_simple<-me_model_set_SNR(song.snr.v2, include.ambient = F)
if(suppressWarnings(require("AICcmodavg", quietly = T)))
  write_aictable_form(me_SNR_simple, file = "SNR_aictab.csv")
print(aic_SNR_simple<-read.csv("SNR_aictab.csv"))
# print(summary(me_SNR_simple$dist_year))
print(summary(me_SNR_simple[[as.character(aic_SNR_simple$Modnames[1])]]))
