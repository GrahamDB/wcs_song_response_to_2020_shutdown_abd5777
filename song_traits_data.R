# song traits loader


source("site_information.R")
use.sites <-c("ABLA", "COMW", "BATE", "BATW", "BABE", "LODU", "FWSC", "RICH" )

song_obs  <- read.csv("song_traits_pub.csv") %>%
  mutate(location=factor(location, levels=use_sites),
         region = region.from.site(location), dialect = factor(dialect.from.site(location)) )