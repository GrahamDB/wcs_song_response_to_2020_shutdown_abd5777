
master.data.dir <- "#FIXME dryad" # Point to remote directory containing LD831 excel sheets, dryad repo in this case
local.data.dir <- "cache" # Point to local directory containing LD831 excel sheets

data.file.path <- function(file.name) {
  if(length(file.name)!=1) return(sapply(file.name, data.file.path))
  if(file.exists(file.path(local.data.dir,file.name)))
    return(file.path(local.data.dir,file.name))
  tmp <- file.path(master.data.dir,file.name)
  if( length(tmp) >1 && sum(file.exists(tmp)) == 1) return(tmp[file.exists(tmp)]) 
  tmp[1]
}

is.cached <- function(file.name) {
  file.exists(file.path(local.data.dir,file.name))
}


read_ld831_metadata_cache <- function() {
  read.csv(file = "LD831_metadata.csv", row.names = 1, 
           colClasses = c(fName = "factor", File.Name = "factor", Serial.Number = "factor", 
                          Model = "factor", Firmware.Version = "factor", User = "factor", 
                          Location = "factor", Start = "POSIXct", Stop = "POSIXct", 
                          Duration = "numeric", Run.Time = "numeric", Pause = "numeric", 
                          Pre.Calibration = "POSIXct", Post.Calibration = "factor", 
                          Calibration.Deviation = "factor", RMS.Weight = "factor", 
                          Peak.Weight = "factor", Detector = "factor", Preamp = "factor", 
                          Microphone.Correction = "factor", Integration.Method = "factor", 
                          OBA.Range = "factor", OBA.Bandwidth = "factor", OBA.Freq..Weighting = "factor", 
                          OBA.Max.Spectrum = "factor", Gain = "numeric", Overload = "numeric", 
                          LAeq = "numeric", LAE = "numeric", EA = "numeric", SEA = "numeric", 
                          LCeq = "numeric", LCeq...LAeq = "numeric", LAIeq = "numeric", 
                          LAIeq...LAeq = "numeric", X..Overloads = "numeric", Overload.Duration = "numeric", 
                          X..OBA.Overloads = "numeric", OBA.Overload.Duration = "numeric", 
                          LAF66.60 = "numeric", LAF90.00 = "numeric", LAS66.60 = "numeric", 
                          LAS90.00 = "numeric"))
}

write_ld831_metadata <- function(use.data=ld831.metadata){
  res<- matrix(character(1),ncol=44,nrow=nrow(use.data)+1)
  res[,1][-1] <- as.character(use.data$File.Name)
  res[1,][-1] <- c("fName", "File.Name", "Serial.Number", 
                   "Model", "Firmware.Version", "User", "Location", "Start", "Stop", 
                   "Duration", "Run.Time", "Pause", "Pre.Calibration", "Post.Calibration", 
                   "Calibration.Deviation", "RMS.Weight", "Peak.Weight", "Detector", 
                   "Preamp", "Microphone.Correction", "Integration.Method", "OBA.Range", 
                   "OBA.Bandwidth", "OBA.Freq..Weighting", "OBA.Max.Spectrum", "Gain", 
                   "Overload", "LAeq", "LAE", "EA", "SEA", "LCeq", "LCeq...LAeq", 
                   "LAIeq", "LAIeq...LAeq", "X..Overloads", "Overload.Duration", 
                   "X..OBA.Overloads", "OBA.Overload.Duration", "LAF66.60", "LAF90.00", 
                   "LAS66.60", "LAS90.00")
  colClasses = c(fName = "factor", File.Name = "factor", Serial.Number = "factor", 
                 Model = "factor", Firmware.Version = "factor", User = "factor", 
                 Location = "factor", Start = "POSIXct", Stop = "POSIXct", 
                 Duration = "numeric", Run.Time = "numeric", Pause = "numeric", 
                 Pre.Calibration = "POSIXct", Post.Calibration = "factor", 
                 Calibration.Deviation = "factor", RMS.Weight = "factor", 
                 Peak.Weight = "factor", Detector = "factor", Preamp = "factor", 
                 Microphone.Correction = "factor", Integration.Method = "factor", 
                 OBA.Range = "factor", OBA.Bandwidth = "factor", OBA.Freq..Weighting = "factor", 
                 OBA.Max.Spectrum = "factor", Gain = "numeric", Overload = "numeric", 
                 LAeq = "numeric", LAE = "numeric", EA = "numeric", SEA = "numeric", 
                 LCeq = "numeric", LCeq...LAeq = "numeric", LAIeq = "numeric", 
                 LAIeq...LAeq = "numeric", X..Overloads = "numeric", Overload.Duration = "numeric", 
                 X..OBA.Overloads = "numeric", OBA.Overload.Duration = "numeric", 
                 LAF66.60 = "numeric", LAF90.00 = "numeric", LAS66.60 = "numeric", 
                 LAS90.00 = "numeric")
  for(key in 1:43){
    key.col=res[1,key+1]
    col.type=colClasses[key.col]
    if(col.type %in% c("POSIXct")){
      res[,key+1][-1] <- strftime(use.data[[key.col]], format="%F %T %z")
    } else if(col.type %in% c("numeric")){
      if(key.col %in% c("OBA.Overload.Duration","LAF66.60","LAF90.00"))
        res[,key+1][-1] <- format(use.data[[key.col]], digits = 1, nsmall=1, scientific =FALSE, trim=TRUE)
      else if(key.col %in% c("EA"))
        res[,key+1][-1] <- format(use.data[[key.col]], digits = 1, nsmall=4, scientific =FALSE, trim=TRUE)
      else if(key.col %in% c("LAIeq...LAeq","LCeq...LAeq"))
        res[,key+1][-1] <- format(use.data[[key.col]], digits = 1, nsmall=4, scientific =FALSE, trim=TRUE)
      else if(key.col %in% c("Pause", "Run.Time","Duration"))
        res[,key+1][-1] <- format(use.data[[key.col]], digits = 1, nsmall=1, scientific =FALSE, trim=TRUE)
      else
        res[,key+1][-1] <- format(use.data[[key.col]], digits = 1, nsmall=4, scientific =FALSE, trim=TRUE)
    } else {
      if(key.col %in% "Firmware.Version")
        res[,key+1][-1] <- substr(as.character(use.data[[key.col]]),1,5)
      else
        res[,key+1][-1] <- as.character(use.data[[key.col]])
      
    }
  }
  
  write.table(res,file = "LD831_metadata.csv", col.names = FALSE, row.names=FALSE,
              quote=1+which(colClasses %in% c("factor","POSIXct")),qmethod = "double",sep=",")
  invisible(res)
}

if(file.exists("LD831_metadata.csv"))
  ld831.metadata<- read_ld831_metadata_cache()

rebuild_ld831.metadata <- function(only.cache=TRUE){
  scan.file.list <- read.csv("song_ld831_dryad_ref.csv", stringsAsFactors = F)[,"dryad_name"]
  ld831.metadata<-collect_rec_summaries(scan.file.list, only.cache=only.cache)
  write_ld831_metadata(ld831.metadata)
  invisible(read_ld831_metadata_cache())
}

# if(!exists("scan.file.list"))
#   scan.file.list <- all.data.files()

get_session_file_name <- function(file.key) {
  stopifnot(file.key %in% rownames(ld831.metadata))
  as.character(ld831.metadata[file.key,"fName"])
}

if(!exists("loader_cache"))
  loader_cache <- list()
if(!exists("loader_band_cache"))
  loader_band_cache <- list()

data_sheet_fk_cache <- function(f_k){
  if(! f_k %in% names(loader_cache)){
    
    res <- get_data_sheet(data.file.path(get_session_file_name(f_k)))
    
    if(! f_k %in% names(loader_band_cache)){
      tmp <- get_band_data(res)
      tmp$RelTime<-with(tmp,Time-min(Time))
      loader_band_cache[[f_k]] <<- tmp
    }
    res$RelTime<-with(res,Time-min(Time))
    loader_cache[[f_k]] <<- res
    return(res)
  } else {
    res <- loader_cache[[f_k]] 
    if(! "RelTime" %in% names(res)){
      if(! f_k %in% names(loader_band_cache)){
        tmp <- get_band_data(res)
        tmp$RelTime<-with(tmp,Time-min(Time))
        loader_band_cache[[f_k]] <<- tmp
      }
      res$RelTime<-with(res,Time-min(Time))
      loader_cache[[f_k]] <<- res
    }
    return(res)
  }
}

band_data_fk_cache <- function(f_k){
  if(! f_k %in% names(loader_band_cache))
    res <- data_sheet_fk_cache(f_k)
  if(! f_k %in% names(loader_band_cache)){
    res <- data_sheet_fk_cache(f_k)
    tmp <- get_band_data(res)
    tmp$RelTime<-with(tmp,Time-min(Time))
    loader_band_cache[[f_k]] <<- tmp
    return(tmp)
  }
  return(loader_band_cache[[f_k]])
}

preload_many_831_band <- function(key.list, args=list()){
  cache_set =list()
  key.list = key.list[!key.list %in% names(loader_band_cache)]
  try({
    cache_set = foreach(f_k = key.list, .combine=c, .multicombine = TRUE, .inorder = F) %dopar% {
      try({
        res <- get_data_sheet(data.file.path(get_session_file_name(f_k)))
        tmp <- get_band_data(res)
        tmp$RelTime<-with(tmp,Time-min(Time))
        v = list(tmp)
        names(v) <- f_k
        return(v)
      })
      list()
    }
  })
  loader_band_cache <<- c(loader_band_cache, cache_set)
  return(length(cache_set))
  
}
preload_many_831_data <- function(key.list, args=list()){
  cache_set =list()
  key.list = key.list[!key.list %in% names(loader_cache)]
  try({
    cache_set = foreach(f_k = key.list, .combine=c, .multicombine = TRUE, .inorder = F) %dopar% {
      try({
        res <- get_data_sheet(data.file.path(get_session_file_name(f_k)))
        res$RelTime<-with(res,Time-min(Time))
        v = list(res)
        names(v) <- f_k
        return(v)
      })
      list()
    }
  })
  loader_cache <<- c(loader_cache, cache_set)
  return(length(cache_set))
  
}
