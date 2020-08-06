library(dplyr)

#Verify song ambient levels will load correctly
song.amb.levels <- local({
  song.metadata <- read.csv("internal_data/SongTimeStamps.csv", row.names = 1)
  
  SENEL.levels <- read.csv("internal_data/SongAmplitudeCOMP_formatB.csv", row.names = 1) %>%
    tibble::rownames_to_column() %>%
    mutate(region = region.from.site(location)) 
  song.amb.levels <- SENEL.levels %>% filter(!is.na(distance), !is.na(ambient.LAeq))
})
song.snr.v2<-local({
  
  song.metadata <- read.csv("internal_data/SongTimeStamps.csv", row.names = 1)
  
  SENEL.levels <- read.csv("internal_data/SongAmplitudeCOMP_formatB.csv", row.names = 1) %>%
    tibble::rownames_to_column() %>%
    mutate(region = region.from.site(location)) 
  song.snr.v2 <- SENEL.levels %>% filter(!is.na(distance) )
  for(bnd in substr(grep("^bg\\.", names(song.snr.v2), value=T),4,12)){
    song.snr.v2[[paste0("song.bands.",bnd,".SNR")]] <-
      song.snr.v2[[paste0("song.bands.",bnd)]] -song.snr.v2[[paste0("bg.",bnd)]] - 10*log10(song.snr.v2$duration)
  }
  song.snr.v2
})

amp_cols<-c("rowname","song.id","bird.id","location","dialect","region","year","distance", "duration",
            "bg.OBCZ2000","bg.OBCZ2500","bg.OBCZ3150","bg.OBCZ4000","bg.OBCZ5000","bg.OBCZ6300","bg.OBCZ8000",
            "ambient.LAeq","SENEL.total","SENEL.song","SEL.LZeq",
            "song.bands.OBCZ2000","song.bands.OBCZ2500","song.bands.OBCZ3150","song.bands.OBCZ4000",
            "song.bands.OBCZ5000","song.bands.OBCZ6300","song.bands.OBCZ8000",
            "song.bands.OBCZ2000.SNR", "song.bands.OBCZ2500.SNR", "song.bands.OBCZ3150.SNR", 
            "song.bands.OBCZ4000.SNR", "song.bands.OBCZ5000.SNR", "song.bands.OBCZ6300.SNR", 
            "song.bands.OBCZ8000.SNR")

summary(amp_cols %in% names(song.snr.v2))
amp_cols[!amp_cols %in% names(song.snr.v2)]
song.amp.out <- song.snr.v2 %>%  mutate(dialect = dialect.from.site(location))
song.amp.out[,amp_cols] %>%
  mutate_if(is.numeric,~round(.,digits=3)) %>%
  tibble::column_to_rownames("rowname") %>% write.csv(file="song_amplitude_pub.csv")
