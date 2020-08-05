# run peak extract

source("measurement_calculations/extract_peak_sound_level.R")
current_songs <- read.csv(file="internal_data/SongTimeStamps.csv", row.names = 1)
default.song.bands.2020<-
  c("OBCZ2000","OBCZ2500","OBCZ3150","OBCZ4000","OBCZ5000","OBCZ6300","OBCZ8000")

default.LAF.bands.2020<-
  c("FFCZ2000","FFCZ2500","FFCZ3150","FFCZ4000","FFCZ5000","FFCZ6300","FFCZ8000")
sweep_LZF_key <- function(.x) sprintf("F%0.3f", ((as.numeric(substring(.x,2))-0.01) %/% 0.1)*0.1 +0.1 )
run.time<-system.time({
  # pdf(file="SongLAeqCleaning2020_alt.pdf", height=8, width=10); 
  # try({
  res.data<-foreach(sMark=iter(current_songs,by="row"), .combine = bind_rows, .multicombine = T)%do%{
    f_k <- as.character(sMark$File.Name)
    res.row<-with(sMark,
                  extract_peak_level(file.key = f_k,
                                     test.data = data_sheet_fk_cache(f_k),
                                     test.bands = band_data_fk_cache(f_k),
                                     song.id = rownames(sMark),
                                     bird.id = sMark$Bird,
                                     t.r = c(Rel.Start, Rel.end),
                                     n.r = c(BG.start,BG.end) ,
                                     all.slices = F,
                                     song.bands = default.LAF.bands.2020,
                                     mic.dist = distance)); 
    
    res.row_2<-with(sMark,
                    extract_peak_level(file.key = f_k,
                                       test.data = data_sheet_fk_cache(f_k),
                                       test.bands = band_data_fk_cache(f_k),
                                       song.id = rownames(sMark),
                                       bird.id = sMark$Bird,
                                       t.r = c(Rel.Start, Rel.end),
                                       n.r = c(BG.start,BG.end) ,
                                       all.slices = T,
                                       song.bands = default.song.bands.2020,
                                       mic.dist = distance)); 
    res.rowF<-with(sMark,
                   extract_peak_level(file.key = f_k,
                                      test.data = data_sheet_fk_cache(f_k),
                                      test.bands = band_data_fk_cache(f_k),
                                      song.id = rownames(sMark),
                                      bird.id = sMark$Bird,
                                      t.r = c(Rel.Start, Rel.end),
                                      n.r = c(BG.start,BG.end) ,
                                      all.slices = T,
                                      song.bands = default.LAF.bands.2020,
                                      mic.dist = distance)); 
    if(any(res.rowF$samp.rate >= 20)){
      col.order=names(res.rowF)
      res.rowF <- res.rowF %>% tibble::rownames_to_column() %>% 
        mutate(slice_id =sweep_LZF_key(slice_id) ) %>% 
        group_by(song.id,slice_id) %>% group_modify(~.x[which.max(.x$peak_LZF.song),]) %>% ungroup() %>%
        mutate(samp.rate=10) %>% tibble::column_to_rownames()
      res.rowF <- res.rowF[col.order]
    }
    cat("\nScanned: ",f_k,rownames(sMark) ,"\n")
    rbind(P=res.row,EQ=res.row_2, ZF=res.rowF) %>% tibble::rownames_to_column() #%>%
      #select(-start,-duration, -SENEL.song, -song.LZeq.mean, -ambient_5sec.LAeq)
  }
  
  # })
  # dev.off()
})
print(run.time)
res.data$location <- current_songs[as.character(res.data$song.id),"Location"]
write.csv(res.data[,-1] %>% tibble::rowid_to_column("rowname") %>%
            # mutate(bg.band=round(bg.band,digits = 3),
            #        peak_LZeq.total=round(peak_LZeq.total,digits = 3),
            #        peak_LZeq.song=round(peak_LZeq.song,digits = 3),
            #        song.LZ_peak=round(song.LZ_peak,digits = 3),
            #        peak_LZF.song=round(peak_LZF.song,digits = 3),
            #        song.LZF_peak=round(song.LZF_peak,digits = 3)) %>%
            mutate_if(is.numeric, ~round(.,digits=3)) %>%
            tibble::column_to_rownames(), row.names = F,
          file = "internal_data/SongPeakLevel_formatB.csv")