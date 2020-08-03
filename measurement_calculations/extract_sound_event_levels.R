
source("measurement_calculations/LD831_helper_funcs.R");
source("measurement_calculations/cache_helper.R");

library(foreach)
library(iterators)
library(dplyr)


# Code for extracting noise bands, SENEL, SENEL.song, and SEL.song
# looks like 2015 measures for SEL and SENEL were LZeq not LAeq
#
# Note SENEL is the sound level at the microphone, and the SEL 
# is the sound level at 1m from the source.
# 
# May also want code for applying / removing A weighting, and 
# extracing peak LAF, peak LAF.song and song LAF_peak

# need to find function remove_noise_alt
remove_noise_alt <- function(song.data,noise.floor, samp.rate=1){
  foreach(s=iter(song.data,by="column"),n=iter(noise.floor,by="column" ),l.c=names(song.data),.combine=cbind) %do% { 
    r<-ifelse(mean(10^(s/10))>10^(n/10),10*log10(mean(10^(s/10))-10^(n/10)),-60);
    r <- r+ c(0,10*log10(length(s))- 10*log10(samp.rate))
    structure(list(r),.Names=l.c,row.names=c("Mean","Total"),class="data.frame") }
}


toZweight <- local({
base.codes<- 
  c("OBCA6.3", "OBCA8.0", "OBCA10.0", "OBCA12.5", "OBCA16.0", "OBCA20.0", 
    "OBCA25.0", "OBCA31.5", "OBCA40.0", "OBCA50.0", "OBCA63.0", "OBCA80.0", 
    "OBCA100", "OBCA125", "OBCA160", "OBCA200", "OBCA250", "OBCA315", 
    "OBCA400", "OBCA500", "OBCA630", "OBCA800", "OBCA1000", "OBCA1250", 
    "OBCA1600", "OBCA2000", "OBCA2500", "OBCA3150", "OBCA4000", "OBCA5000", 
    "OBCA6300", "OBCA8000", "OBCA10000", "OBCA12500", "OBCA16000", 
    "OBCA20000", "FFCA6.3", "FFCA8.0", "FFCA10.0", "FFCA12.5", "FFCA16.0", 
    "FFCA20.0", "FFCA25.0", "FFCA31.5", "FFCA40.0", "FFCA50.0", "FFCA63.0", 
    "FFCA80.0", "FFCA100", "FFCA125", "FFCA160", "FFCA200", "FFCA250", 
    "FFCA315", "FFCA400", "FFCA500", "FFCA630", "FFCA800", "FFCA1000", 
    "FFCA1250", "FFCA1600", "FFCA2000", "FFCA2500", "FFCA3150", "FFCA4000", 
    "FFCA5000", "FFCA6300", "FFCA8000", "FFCA10000", "FFCA12500", 
    "FFCA16000", "FFCA20000")
# bands 30 to 43; 1k to 20k
b8A = c(-85.3, -77.8, -70.4, -63.4,  -56.7, -50.5, -44.7, -39.4, -34.6, -30.2, -26.2, -22.5)
b20A = c(-19.1, -16.1, -13.4, -10.9, -8.6, -6.6, -4.8, -3.2, -1.9, -0.8)
b30A= c(0, 0.6, 1, 1.2, 1.3, 1.2, 1, 0.5, -0.1, -1.1)
b40A= c(-2.5, -4.3, -6.6, -9.3)
#A2Z
A2Z = -rep(c(b8A,b20A,b30A,b40A),2)
names(A2Z) <- base.codes
# print(A2Z)
function(vals, band.codes = names(vals)){
  # str(vals)
  # print(band.codes)

  # print(rbind(vals,delta=A2Z[band.codes]))
  if(is.null(dim(vals))) return(vals+A2Z[band.codes])
  if(!is.null(dim(band.codes)) && all(dim(band.codes) == dim(vals))) {
    print(rbind(vals,delta=A2Z[band.codes], LZ=vals+A2Z[band.codes]))
    return(vals+A2Z[band.codes])
  }
  band.codes=matrix(band.codes,ncol=ncol(vals), nrow = nrow(vals), 
                    byrow = (ncol(vals)==length(band.codes)))
  # print(rbind(vals,delta=A2Z[band.codes], LZ=vals+A2Z[band.codes]))
  print(vals+A2Z[band.codes])
  # stop("Not Implemented!")
  return(vals+A2Z[band.codes])
}
})

extract_SEL <- function(
  file.key,
  test.file,
  test.data,
  test.bands,
  song.id,
  samp.rate,
  bird.id = file.key,
  t.r=c(0:30),n.r=c(0,0), 
  song.bands=default.song.bands, 
  mic.dist=NA,
  verbose=F){
  if(missing(file.key) && !missing(test.file)){
    if(file.exists(test.file))
      file.key= as.character(get_scalar_fields(get_summary_sheet(test.file))[["File.Name"]])
    else
      file.key= as.character(get_scalar_fields(get_summary_sheet( data.file.path(test.file)))[["File.Name"]])
    if(verbose) cat( "\nExtracted File.Name ",file.key,"\n")
  }

  if(missing(test.data)){
    test.data <- NULL
    try({ test.data <- data_sheet_fk_cache(file.key); })
    if(is.null(test.data)){
      if(verbose) cat( "\nUnregistered key ",file.key,"\n")
      if(missing(test.file)){
        test.file<- get_session_file_name(file.key)
      }
      if(file.exists(test.file))
        test.data= get_data_sheet(test.file)
      else
        test.data= get_data_sheet(data.file.path(test.file))
      test.bands<-get_band_data(test.data)
      test.data$RelTime<-with(test.data,Time-min(Time))
      test.bands$RelTime<-(test.bands$Time-min(test.bands$Time))
      try({
      loader_cache[[file.key]] <<- test.data
      loader_band_cache[[file.key]] <<- test.bands
      })
      if(verbose) cat( "\nRegistered ",file.key,"\n")
    } else if(missing(test.bands)){
      test.bands<-NULL
      try({ test.bands <- band_data_fk_cache(file.key); })
      
    }
  }
  if(missing(song.id)){
    song.id=paste(file.key,"Song",t.r[1],t.r[2],sep="_")
  }
  
  # check band codes
  
  b.group = unique(substr(song.bands, 1,4))[1]
    #"OBCA"
  if(! b.group %in% levels(test.bands$BCode)){
    if(substr(b.group,4,4) != "Z"){
      b.group.new = paste0(substr(b.group,1,3), "Z")
    } else {
      b.group.new = paste0(substr(b.group,1,3), "A")
      
    }
    if(! b.group.new %in% levels(test.bands$BCode))
      stop("Neither ", b.group, " or ", b.group.new, " available for processing." )
    
    song.bands = sub(pattern = b.group, replacement = b.group.new, x= song.bands)
  }
  
  # 
  # s.name<-file.key
  # if(!file.exists(test.file))
  #   f.name <- test.file
  # else
  #   f.name<-sub("^.+/","",test.file)
  # 
  
  if(missing(samp.rate)) {
    if(max(test.data$RelTime) >=5) {
      samp.rate <- 
        max(sum(test.data$RelTime<1),
            sum(test.data$RelTime<2&test.data$RelTime>=1),
            sum(test.data$RelTime<3&test.data$RelTime>=2),
            sum(test.data$RelTime<4&test.data$RelTime>=3),
            sum(test.data$RelTime<5&test.data$RelTime>=4))
    } else if(max(test.data$RelTime) >=2) {
      samp.rate <- sum(test.data$RelTime<2)/2
    } else{
      samp.rate <- sum(test.data$RelTime<1)/1
    }
    if(verbose) cat("Estimated sample rate: ", samp.rate,"\n")
  }
  ambient_10s.LAeq<-numeric()
  ambient_05s.LAeq<-numeric()
  if(t.r[1]>=10 ){
    ambient_10s.LAeq<-as.numeric((test.data %>%filter(RelTime >= t.r[1]-10,RelTime <= t.r[1]))$LAeq)
  }
  if(t.r[1]>=5 ){
    ambient_05s.LAeq<-as.numeric((test.data %>%filter(RelTime >= t.r[1]-5,RelTime <= t.r[1]))$LAeq)
  }
  if(verbose) cat("Filtering,", range(t.r), song.bands,head(names(test.data)),"\n")
  song.data <- test.data %>%  filter(RelTime >= min(t.r), RelTime < max(t.r))
  if(verbose) str(song.data)
  song.data <- song.data[, song.bands]
  use.bands <- test.bands %>% 
    filter(Band %in% song.bands, 
           RelTime >= min(t.r), RelTime < max(t.r))
  if( diff(n.r)[1] > 0 ) {
    if(verbose) cat( "\nUsing specified noise segment for 1/3 octave band LAeq values.\n")
    noise.data=test.data[test.data$Time > (n.r[1]+min(test.data$Time)) &
                           test.data$Time < (n.r[2]+min(test.data$Time)),
                         levels(test.bands$Band)]
    full.floor<-10*log10(apply(10^(noise.data/10),2,mean))
    # print(
    noise.floor<-full.floor[song.bands] #)
    if(verbose) print(noise.floor)
    print(10*log10(sum(10^(noise.floor/10))))
    
    
    use.bands$L.clean<-
      with(use.bands,
           10*log10(ifelse(L<=full.floor[Band],
                           1,
                           10^(L/10)-10^(full.floor[Band]/10) )))
  } else {
    if(verbose) cat( "\nNo noise specified, using 1/3 octave band LA_90 values.\n")
    full.floor=apply(test.data[,levels(test.bands$Band)],2,quantile,probs=0.1)
    if(verbose) print(full.floor[song.bands])
    if(verbose) print(10*log10(sum(10^(full.floor[song.bands]/10))))
    use.bands$L.clean<-
      with(use.bands,10*log10(ifelse(L<=full.floor[Band],1,10^(L/10)-10^(full.floor[Band]/10) )))
    noise.floor<- full.floor[song.bands]
  }
  if(length(unique(substr(song.bands,1,4))) !=1 ) 
    stop("Cannot interpret multiple band types! ", unique(substr(song.bands,1,4)))
  band.weighting <- unique(substr(song.bands,4,4))
  if(! band.weighting %in% c("A","Z")) {
    stop("Cannot interpret band weighting: ", band.weighting)
  }
  if(band.weighting %in% c("Z")){
    song.LZeq<-remove_noise_alt(song.data,noise.floor, samp.rate = samp.rate)
  } else if(band.weighting %in% c("A")){
    song.LAeq<-remove_noise_alt(song.data,noise.floor, samp.rate = samp.rate)
    song.LZeq <- toZweight(song.LAeq)
    A.codes<-names(song.LZeq)
    substr(names(song.LZeq), 4,4) <-"Z"
    noise.floor.LA <- noise.floor
    noise.floor <- toZweight(noise.floor.LA)
    substr(names(noise.floor),4,4) <-"Z"
    
  } 
  if(verbose) cat("A") # all collected songs, report function arguments and noise floor
  res=data.frame(song.id=song.id,
                 bird.id=bird.id,
                 start=min(use.bands$Time),
                 duration=max(use.bands$RelTime)-min(use.bands$RelTime),
                 distance=mic.dist,
                 bg=do.call(data.frame,as.list(noise.floor)),
                 ambient.LAeq=10*log10(mean(10^(ambient_10s.LAeq/10))),
                 ambient_5sec.LAeq=10*log10(mean(10^(ambient_05s.LAeq/10))),
                 SENEL.total= 10*log10(sum(10^(song.data/10))) - 10*log10(samp.rate),
                 SENEL.song= 10*log10(sum(10^(song.LZeq[2,]/10))))
  if(verbose) cat("B") # if distance is available, calculate SEL statistics
  if(!is.na(mic.dist) && ! is.null(mic.dist) ){
    if(verbose) cat("C")
    res$SEL.LZeq<-10*log10(sum(10^(song.LZeq[2,]/10)))+20*log10(mic.dist)
    res$song.LZeq.mean<-10*log10(sum(10^(song.LZeq[1,]/10)))+20*log10(mic.dist)
    #res$song.LZeq.median<-median(10*log10(apply(10^(remove_noise(song.data,noise.floor)/10),1,sum)))+20*log10(mic.dist)
    res<-cbind(res,song.bands=song.LZeq[1,]+20*log10(mic.dist) )
    
  } else {
    if(verbose) cat("D")
    res$SEL.LZeq<-NA
    res$song.LZeq.mean<-NA
    #res$song.LZeq.median<-NA
    res<-cbind(res,song.bands=song.LZeq[1,]+NA )
    
  }
  if(verbose) print(res)
  rownames(res)<-as.character(res$song.id)
  res
}
