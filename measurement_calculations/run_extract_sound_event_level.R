# run SEL extract

source("measurement_calculations/extract_sound_event_levels.R")

current_songs <- read.csv(file="internal_data/SongTimeStamps.csv", row.names = 1)
default.song.bands.2020<-
  c("OBCZ2000","OBCZ2500","OBCZ3150","OBCZ4000","OBCZ5000","OBCZ6300","OBCZ8000")
run.time<-system.time({
  res.data<-foreach(sMark=iter(current_songs,by="row"), .combine = bind_rows, .multicombine = T)%do%{
    f_k <- as.character(sMark$File.Name)
    print(res.row<-with(sMark,
                        extract_SEL(file.key = f_k,
                                    test.data = data_sheet_fk_cache(f_k),
                                    test.bands = band_data_fk_cache(f_k),
                                    song.id = rownames(sMark),
                                    bird.id = sMark$Bird,
                                    t.r = c(Rel.Start, Rel.end),
                                    n.r = c(BG.start,BG.end) ,
                                    song.bands = default.song.bands.2020,
                                    mic.dist = distance))); 
    cat("\nScanned: ",f_k,rownames(sMark) ,"\n")
    res.row %>% tibble::rownames_to_column() %>%
      mutate(location="", year = factor(strftime(start,format = "Y%Y"))) %>%
      select(-start,-duration, -SENEL.song, -song.LZeq.mean, -ambient_5sec.LAeq)
  }
  
})
print(res.data)
print(run.time)
res.data$location <- current_songs[res.data$rowname,"Location"]
write.csv(res.data %>% mutate_if(is.numeric, ~round(.,digits=3)) %>%
            tibble::column_to_rownames(),
          row.names = F,
          file = "internal_data/SongAmplitudeCOMP_formatB.csv")

