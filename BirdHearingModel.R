library(dplyr)


# Data on Critical ratios for bird masking functions
sosp.masking.dooling <-
  data.frame(frequency=c(1,2,4,8)*1000,
             crit.ratio=c(24.2718446602,
                          25.7281553398,
                          28.640776699,
                          33.9805825243))

# Audiogram curve estimate from:
# Derryberry EP, Danner RM, Danner JE, Derryberry GE, Phillips JN, Lipshutz SE, et al. (2016) 
# Patterns of Song across Natural and Anthropogenic Soundscapes Suggest That White-Crowned 
# Sparrows Minimize Acoustic Masking and Maximize Signal Content. PLoS ONE 11(4): e0154456.
# DOI:10.1371/journal.pone.0154456

# Citations for Auditory Evoked Potentials dataset used to estimate audiogram curve:

####  Velez A, Gall MD, Fu J, Lucas JR (2015) Song structure, not high-frequency
####  song content, determines high-frequency auditory sensitivity in nine
####  species of New World sparrows (Passeriformes: Emberizidae). Functional
####  Ecology 29(4): 487-497. http://dx.doi.org/10.1111/1365-2435.12352

####  Velez A, Gall MD, Fu J, Lucas JR (2014) Data from: Song structure, not
####  high-frequency song content, determines high-frequency auditory
####  sensitivity in nine species of New World sparrows (Passeriformes:
####  Emberizidae). Dryad Digital Repository.
####  http://dx.doi.org/10.5061/dryad.2n96q



wcsp.audiogram.min <- function(freq,
                           bird.offset=-10.1915680,
                           min.dB=25.3499996861262,
                           alpha= 29.0383591141284,
                           peak.freq= 3663.87633070415) {
    min.dB+bird.offset - alpha + alpha * sqrt(1 + (log(freq) - log(peak.freq))^2) }
wcsp.audiogram.median <- function(freq,
                               bird.offset=2.431016,
                               min.dB=25.3499996861262,
                               alpha= 29.0383591141284,
                               peak.freq= 3663.87633070415) {
  min.dB+bird.offset - alpha + alpha * sqrt(1 + (log(freq) - log(peak.freq))^2) }
bird.db.offsets <- data.frame(min.dB = c(-10.191568044643, -0.93500624505207,  
                                  -4.30102872265496, 5.79703870361983,
                                  -5.98403993775502, 6.63854430825421,  
                                  2.43101622441282, 3.2725218569091, 3.2725218569091),
                              row.names = c("8013", "8014", "8015", "8026", "8145",
                                  "8158", "8159", "9651", "9965" ))
wcsp.audiogram.all <- function(freq) {
  sapply(freq,wcsp.audiogram.min,bird.offset=bird.db.offsets$min.dB) }



wcsp.masking.spline <- with(sosp.masking.dooling, splinefun(x=frequency,y=crit.ratio))
wcsp.min.db.spline <- 
  data.frame(prob=(0:100)/100) %>% mutate(min.dB= quantile(bird.db.offsets$min.dB,prob)) %>%
  with(approxfun(x=min.dB,y=prob,yleft=0,yright=1))

audibility.test.oct3 <- 
  function(freq, bg.L, sig.L,q.val=FALSE, discriminate=TRUE){
    if(!is.numeric(freq)){
      freq<- as.numeric(substring(as.character(freq),5))
    }
    stopifnot(!is.na(freq),freq>=500,freq<=1e4)
    comp.L <- sig.L - ifelse(discriminate, 3,0)
    if(q.val){
      res <-ifelse(comp.L-bg.L > wcsp.masking.spline(freq),
                   wcsp.min.db.spline(comp.L-wcsp.audiogram.min(freq = freq,bird.offset = 0)  ),
                   rep(0, length(comp.L-bg.L) ))
    } else {
      res <-(comp.L-bg.L > wcsp.masking.spline(freq)) &
        (comp.L >wcsp.audiogram.median(freq = freq)  )
    }
    res
  }

audible_range<- 
  function(freq, bg.L, sig.L, discriminate=TRUE){
    if(!is.numeric(freq)){
      freq<- as.numeric(substring(as.character(freq),5))
    }
    stopifnot(!is.na(freq),freq>=500,freq<=1e4)
    comp.L <- sig.L - ifelse(discriminate, 3,0)
    suppressWarnings( {
      res <- ifelse(wcsp.masking.spline(freq)+bg.L > wcsp.audiogram.median(freq = freq) ,
                  comp.L-wcsp.masking.spline(freq)-bg.L,
                  comp.L-wcsp.audiogram.median(freq)) 
    ifelse(is.na(res),0,10^(res/20)) } )
  }
