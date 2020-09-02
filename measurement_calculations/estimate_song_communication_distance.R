
source("communication/BirdHearingModel.R")

peak_data <- read.csv(file="internal_data/SongPeakLevel_formatB.csv") %>% 
  filter(!is.na(distance))
rms_data <- peak_data %>% filter(slice_id %in% "Max" | substr(peak_LZeq.band, 1,1) %in% "O")
slm_data <- peak_data %>% filter(slice_id %in% "Max" | substr(peak_LZeq.band, 1,1) %in% "F") 


ann_data <- rms_data %>% group_by(song.id) %>% 
  mutate(is_song_rms = song.LZ_peak >= song.LZ_peak[slice_id %in% "Max"]-26,
         is_song_slm = song.LZF_peak >= song.LZF_peak[slice_id %in% "Max"]-26,
         rel.Leq=song.LZ_peak-song.LZ_peak[slice_id %in% "Max"],
         rel.LF=song.LZF_peak-song.LZF_peak[slice_id %in% "Max"],
         note_cnt= cumsum(is_song_rms & ! lag(is_song_rms) & ! slice_id %in% "Max"),
         note_id=paste0(as.numeric(song.id),"_",note_cnt ))%>%
  ungroup()%>% 
  mutate(max_range_rms=audible_range(peak_LZeq.band,bg.L = bg.band, sig.L=song.LZ_peak),
         max_range_slm=audible_range(peak_LZeq.band,bg.L = bg.band, sig.L=song.LZF_peak)) 
ann_data_slm <- slm_data %>% 
  left_join(ann_data %>% select(song.id,slice_id,is_song_rms,note_cnt,note_id)) %>%
  mutate(note_cnt = ifelse(is.na(is_song_rms),lag(note_cnt),note_cnt),
         note_id = ifelse(is.na(is_song_rms),lag(note_id),note_id))%>% 
  group_by(song.id) %>% 
  mutate(is_song_slm = song.LZF_peak >= song.LZF_peak[slice_id %in% "Max"]-26,
         rel.Leq=NA,
         rel.LF=song.LZF_peak-song.LZF_peak[slice_id %in% "Max"])%>%
  ungroup()%>% 
  mutate(max_range_rms=NA,
         max_range_slm=audible_range(peak_LZeq.band,bg.L = bg.band, sig.L=song.LZF_peak)) 

tstamp_data <- ann_data %>% filter(!slice_id %in% "Max") %>% 
  mutate(rel.time=as.numeric(substring(slice_id,2)))%>%
  group_by(song.id) %>% mutate(rel.time2=rel.time-min(rel.time)) %>% ungroup()
tstamp_data_slm <- ann_data_slm %>% filter(!slice_id %in% "Max") %>% 
  mutate(rel.time=as.numeric(substring(slice_id,2)))%>%
  group_by(song.id) %>% mutate(rel.time2=rel.time-min(rel.time)) %>% ungroup()


comm_data <-
  peak_data %>% filter(slice_id %in% "Max") %>% 
  mutate(max_range=audible_range(peak_LZeq.band,bg.L = bg.band, sig.L=song.LZ_peak),
         peak_LZeq.band=factor(peak_LZeq.band)) 

distort_data_rEQ <- ann_data %>% filter(!slice_id %in% "Max", is_song_rms) %>%
  group_by(song.id) %>%  
  mutate(r.eq.p75=quantile(max_range_rms,0.25),
         r.eq.p50=quantile(max_range_rms,0.5),
         r.eq.p25=quantile(max_range_rms,0.75))%>%ungroup()%>%
  select(song.id,bird.id,distance,location, r.eq.p75,r.eq.p50,r.eq.p25) %>% unique()
distort_data_rF <- ann_data %>% filter(!slice_id %in% "Max", is_song_slm) %>%
  group_by(song.id) %>%  
  mutate(r.F.p75=quantile(max_range_slm,0.25),
         r.F.p50=quantile(max_range_slm,0.5),
         r.F.p25=quantile(max_range_slm,0.75))%>%ungroup()%>%
  select(song.id,bird.id,distance,location, r.F.p75,r.F.p50,r.F.p25) %>% unique()
distort_data_sF <- ann_data_slm %>% filter(!slice_id %in% "Max", is_song_slm) %>%
  group_by(song.id) %>%  
  mutate(s.F.p75=quantile(max_range_slm,0.25),
         s.F.p50=quantile(max_range_slm,0.5),
         s.F.p25=quantile(max_range_slm,0.75))%>%ungroup()%>%
  select(song.id,bird.id,distance,location, s.F.p75,s.F.p50,s.F.p25) %>% unique()

combo_data= comm_data %>% 
  inner_join(distort_data_rEQ)%>% 
  inner_join(distort_data_rF)%>% 
  inner_join(distort_data_sF)

combo_data %>% rename(bird=bird.id) %>% 
  inner_join(read.csv("internal_data/song_amplitude_pub.csv") %>% 
               select(song.id,bird.id,dialect,region,year ) ) %>%
  select(song.id,bird.id,location,dialect,region,year,
         distance,peak_LZeq.band,
         bg.band,peak_LZeq.total,peak_LZeq.song,song.LZ_peak,song.LZF_peak,
         max_range,
         r.eq.p75,r.eq.p50,r.eq.p25,
         r.F.p75,r.F.p50,r.F.p25,
         s.F.p75,s.F.p50,s.F.p25) %>%
  mutate_if(is.numeric,~round(.,digits = 3))%>%
  write.csv(file="internal_data/song_audibility.csv", row.names=F)

