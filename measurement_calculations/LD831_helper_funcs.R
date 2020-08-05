# Read 831 Summary sheet

options(digits.secs=3);
library(readxl)
library(foreach)
library(dplyr)
if(utils::packageVersion("readxl") > "1.2.0"){
  # shut off name repair
  get_summary_sheet<- function(file){
    tmp<-read_excel(file,sheet = "Summary",col_names = FALSE, .name_repair="minimal")
    tmp
  }
}else {
get_summary_sheet<- function(file){
  tmp<-read_excel(file,sheet = "Summary",col_names = FALSE)
  tmp
}
}
ignore_fields=c("Avg.Wind.Speed", "Gust.Speed", "Min.Wind.Speed", "Gust.Dir..Compass.",
                "Temperature.Avg", "Temperature.Max", "Temperature.Min", "Humidity.Avg", 
                "Humidity.Max", "Humidity.Min", "LAS5.00", "LAS10.00", "LAS33.30", "LAS50.00", 
                "LAF5.00", "LAF10.00", "LAF33.30", "LAF50.00","LAeq.1", "LAeq.2", "Job.Description")
numeric_fields=c("Start", "Stop", "Duration",  
                 "Run.Time", "Pause", "Pre.Calibration", "X..Overloads", "X..OBA.Overloads",
                 "Gain","Overload", "LAeq", "LAE", "EA", "SEA", "LCeq", "LAeq.1", "LCeq...LAeq", 
                 "LAIeq", "LAeq.2", "LAIeq...LAeq", "X..Overloads", "Overload.Duration", 
                 "LAF5.00", "LAF10.00", "LAF33.30", "LAF50.00", "LAF66.60", "LAF90.00", 
                 "Avg.Wind.Speed", "Gust.Speed", "Min.Wind.Speed", "OBA.Overload.Duration",
                 "Temperature.Avg", "Temperature.Max", "Temperature.Min", "Humidity.Avg", 
                 "Humidity.Max", "Humidity.Min", "LAS5.00", "LAS10.00", "LAS33.30", "LAS50.00", 
                 "LAS66.60", "LAS90.00" )
time_fields=c("Start", "Stop", "Pre.Calibration")
dtime_fields=c("Duration", "Run.Time", "Pause")
fix.time <- function(xlsx.time){ foo=xlsx.time*24*3600 -2209136400;class(foo)<-"POSIXct"; foo}
fix.difftime  <- function(xlsx.time) xlsx.time*24*3600
hist_fix=c("Record #", "Record Type", "Int. Temp (C)", 
           "1/1 LAeq 8.0","1/1 LZeq 8.0", 
           "1/1 LAF 8.0","1/1 LZF 8.0", 
           "1/3 LAeq 6.3","1/3 LZeq 6.3",  "OBA Ovrld.",
           "1/3 LAF 6.3","1/3 LZF 6.3") 
hist_cor=c("R", "Event", "Temp.C", 
           "OOOA8.0","OOOZ8.0", 
           "FFFA8.0","FFFZ8.0", 
           "OBCA6.3","OBCZ6.3",  "OBAOver",
           "FFCA6.3","FFCZ6.3") 
hist_band_head=c("OOOA8.0","OOOZ8.0","FFFA8.0","FFFZ8.0", "OBCA6.3","OBCZ6.3", "FFCA6.3","FFCZ6.3")
hist_band_code=substr(hist_band_head,1,4)
field_name_correction <- c(Filename="File.Name", File.Name.on.Meter="File.Name")
known_fields<-
  c("File.Name", "Serial.Number", "Model", "Firmware.Version", "User", "Location", "Start", "Stop", "Duration", 
    "Run.Time", "Pause", "Pre.Calibration", "Post.Calibration", "Calibration.Deviation", "RMS.Weight", "Peak.Weight", 
    "Detector", "Preamp", "Microphone.Correction", "Integration.Method", "OBA.Range", "OBA.Bandwidth", "OBA.Freq..Weighting", 
    "OBA.Max.Spectrum", "Gain", "Overload", "LAeq", "LAE", "EA", "SEA", "LCeq", "LAeq.1", "LCeq...LAeq", 
    "LAIeq", "LAeq.2", "LAIeq...LAeq", "X..Overloads", "Overload.Duration", "X..OBA.Overloads", "OBA.Overload.Duration", 
    "LAF5.00", "LAF10.00", "LAF33.30", "LAF50.00", "LAF66.60", "LAF90.00", "Avg.Wind.Speed", "Gust.Speed", 
    "Min.Wind.Speed", "Gust.Dir..Compass.", "Temperature.Avg", "Temperature.Max", "Temperature.Min", "Humidity.Avg", 
    "Humidity.Max", "Humidity.Min", "LAS5.00", "LAS10.00", "LAS33.30", "LAS50.00", "LAS66.60", "LAS90.00", 
    "Job.Description", "Note")

get_data_sheet<- function(file){
  # sN<-readxl:::standardise_sheet("Time History",range=NULL,excel_sheets(file))
  # fN<-readxl:::check_file(file)
  #cT<-readxl:::xlsx_col_types(  fN,sheet = sN,na="",nskip = 1)
  #cT[cT=="blank"]="text"
  if(utils::packageVersion("readxl") > "1.2.0"){
    tmp<-read_excel(file,sheet = "Time History",na="NA", .name_repair="minimal")
  }else {
  tmp<-read_excel(file,sheet = "Time History",na="NA")
  }
  for(i in 1:length(hist_fix)){
    if(any(names(tmp) %in% hist_fix[i])){
      names(tmp)[names(tmp) %in% hist_fix[i] ]<- hist_cor[i]
    }}
  keyH<-which(names(tmp) %in% hist_band_head)
  
  if(length(keyH) > 1){
    keyA<-which(grepl(pattern="^[0-9]+(\\.[0-9]+)?(__[1-9])?$",x=names(tmp)))
    if(length(keyA)==0){
      keyA<-which(grepl(pattern="^1/[13] L[AZ][SIF] [0-9]+(\\.[0-9]+)?$",x=names(tmp)) |
                    grepl(pattern="^1/[13] L[AZ]eq [0-9]+(\\.[0-9]+)?$",x=names(tmp))  )
      names(tmp)[keyA] <- sub(pattern="^1/[13] L[AZ]([SIF]|eq) ",replacement="", x=names(tmp)[keyA])
    }
    kI <- matrix(c(keyH, keyH[-1]-1,length(names(tmp))),ncol=2,byrow = FALSE)
    for(k in 1:nrow(kI)){
      keyB<-keyA[keyA >= kI[k,1] &keyA <= kI[k,2] ]
      names(tmp)[keyB] <- paste(substr(names(tmp)[keyH[k]],1,4),
                                sub(pattern="__[1-9]$", replacement = "", x=names(tmp)[keyB]),sep="")
    }
    for(vL in c(keyH,keyA))
      tmp[[vL]] <-as.numeric(tmp[[vL]])
  } else if (length(keyH)==1){
    
    keyB<-which(grepl(pattern="^[0-9]+(\\.[0-9]+)?$",x=names(tmp)))
    if(length(keyB)==0){
      keyB<-which(grepl(pattern="^1/[13] L[AZ][SIF] [0-9]+(\\.[0-9]+)?$",x=names(tmp)) |
                    grepl(pattern="^1/[13] L[AZ]eq [0-9]+(\\.[0-9]+)?$",x=names(tmp))  )
      names(tmp)[keyB] <- sub(pattern="^1/[13] L[AZ]([SIF]|eq) ",replacement="", x=names(tmp)[keyB])
    }
    names(tmp)[keyB] <- paste(substr(names(tmp)[keyH],1,4),names(tmp)[keyB],sep="")
    for(vL in c(keyH,keyB))
      tmp[[vL]] <-as.numeric(tmp[[vL]])
  }
  tmp<-data.frame(tmp)
  tmp$Segment<-0
  tmp$Segment[1+which(as.character(tmp$Event)%in% c("Run","Resume"))]<- 1
  tmp$Segment <- cumsum(tmp$Segment)
  tmp<-tmp[is.na(tmp$Event),]
  if(length(unique(tmp$Time))*2<length(tmp$Time)){
    tmp.e<-max(tmp$Time)
#    tmp.mL <-sort(unique(tmp$Time))[-(1:2)]
    tmp.f<-sort(unique(tmp$Time))[2]
    if(length(unique(tmp$Time))>4){
      tmp.rate<-median(summary(factor(tmp$Time)))
    } else{
      tmp.rate<-summary(factor(tmp$Time))[2]
    }
    tmp$Time[tmp$Time<tmp.f]<-tmp.f+(-sum(tmp$Time<tmp.f):-1)/tmp.rate
    if(length(unique(tmp$Time[tmp$Time>=tmp.f & tmp$Time<tmp.e])) == as.double(tmp.e-tmp.f,units="secs")){
    tmp$Time[tmp$Time>=tmp.f & tmp$Time<tmp.e] <- 
      tmp$Time[tmp$Time>=tmp.f & tmp$Time<tmp.e]+rep(0:(tmp.rate-1),length(unique(tmp$Time[tmp$Time>=tmp.f & tmp$Time<tmp.e])))/tmp.rate
    } else {
      tmp.iL=sort(unique(tmp$Time[tmp$Time>tmp.f & tmp$Time<tmp.e]))
      tmp$Time[tmp$Time==tmp.f ] <- 
        tmp$Time[tmp$Time==tmp.f]+ (0:(length(tmp$Time[tmp$Time==tmp.f])-1))/tmp.rate
      tmp.L = tmp.f
      for(L_i in seq_along(tmp.iL)){
        tmp.i= tmp.iL[L_i]
        if(as.double(tmp.i-tmp.L,units="secs") == 1){
          tmp$Time[tmp$Time==tmp.i ] <- 
            tmp$Time[tmp$Time==tmp.i]+ (0:(sum(tmp$Time==tmp.i)-1))/tmp.rate
        } else {
          tmp$Time[tmp$Time==tmp.i]<-tmp.i+1+(-sum(tmp$Time==tmp.i):-1)/tmp.rate
        }
        tmp.L = tmp.i
      }
    }
    tmp$Time[tmp$Time>=tmp.e]<-tmp$Time[tmp$Time>=tmp.e]-1/tmp.rate+(1:sum(tmp$Time>=tmp.e))/tmp.rate
  }
  
  tmp
}
get_scalar_fields <- function(sSheet){
  isField <- which(sapply(1:nrow(sSheet), function(i) all(!is.na(sSheet[i,1:2]), is.na(sSheet[i,-(1:3)]))))
  #  print(length(isField))
  fN <- as.character(sSheet[isField,1][[1]])
  fV <- t(sSheet[isField,2])
  fN-> names(fV)
  fV<-as.data.frame(fV)
  fN-> names(fV)
  fV<-data.frame(fV)
  dN <-names(fV)
  if(any(dN %in% names(field_name_correction))) 
    names(fV)[dN %in% names(field_name_correction)] <- 
    field_name_correction[names(fV)[dN %in% names(field_name_correction)] ]
  dN <-names(fV)
  for(i in known_fields[! known_fields %in% dN ]) fV[[i]] <- NA
  dN <-names(fV)
  for(i in numeric_fields) { if(i %in% dN) fV[,i]<- as.numeric(as.character(fV[,i])) else warning(i)}
  for(i in time_fields) { if(i %in% dN) fV[,i]<- fix.time(fV[,i])else warning(i)}
  for(i in dtime_fields) { if(i %in% dN) fV[,i]<- fix.difftime(fV[,i])else warning(i)}
  #print(fV[,8:9])
  dN <-names(fV)
  if(any(dN %in% ignore_fields))
    return(fV[,!dN %in% ignore_fields])
  fV
}

get_detector_type <- function(sSheet){ sSheet$X1[which(sSheet$X0 == "Detector")] }
get_xlsx_structure_factor <- 
  function(files) factor(foreach(f=files,.combine=c) %do% excel_sheets(f) )

get_band_data= function(hist.data,band.codes=hist_band_code,bands=NULL){
  if(is.factor(bands) || is.numeric(bands))
    bands<- as.character(bands)
  #print(bands)
  
  foreach(b.code=band.codes, .combine=rbind) %:% when(any(grepl(pattern=paste("^",b.code,sep=""),names(hist.data))))   %do% {
    c.bands<-bands
    # print(c(b.code,c.bands))
    if(!is.null(c.bands)){
      if(any(grepl(pattern=paste("^",b.code),c.bands))){
        c.bands<-c.bands[grepl(pattern=paste("^",b.code,sep=""),c.bands)]
      }else{
        c.bands<-paste(b.code,c.bands[grepl(pattern="^[0-9]+(\\.[0-9]+)?$",c.bands)],sep="")
      }
    } else{
      c.bands<-names(hist.data)[grepl(pattern=paste("^",b.code,sep=""),names(hist.data))]
    }
    res<-foreach(band=c.bands, .combine=rbind) %do% data.frame(Time=hist.data$Time,
                                                               Band=band,
                                                               BCode=b.code, 
                                                               FC=as.numeric(substr(band,5,nchar(band))),
                                                               L=hist.data[[band]])
    if(nrow(res)!=0)
      res$BN=round(10*log10(res$FC))
    res
  }
}

collect_rec_summaries<- function(files, only.cache=TRUE){
  foreach(f=files,.combine = bind_rows, .multicombine = T) %:% when(!only.cache || is.cached(f))   %dopar% {
    if(!file.exists(data.file.path( f)  )) return(NULL)
    if(file.size(data.file.path( f)  )==0) return(NULL)
    res <- NULL
    try({ res<- data.frame(fName=f,
               get_scalar_fields(get_summary_sheet(data.file.path( f)  ))) 
    })
    if(!inherits(res$Location, "factor") ){
      if(is.na(res$Location)) res$Location <- factor("Unknown")
      else res$Location <- as.factor(res$Location)
    }
    res
  }
}

