site_dialects <- 
  list( San_Fran= c("BATE", "BATW", "BABE",
                    "LODU","FWSC"),
        Merced=c("LAME","FOPO","FOFU", "LAEN"),
        Richmond= c("RICH"),
        Abbot=c("ABLA","ELPR"),
        Commonweal=c("COMW"),
        Limantor="LIMA", 
        San_Sim="SANS", Cambria="CAMB", Morro="MORR", Schooner="marin"
  )
dialect_type <- 
  factor(c( San_Fran= "used",
            Merced="extra",
            Richmond= "used",
            Abbot="used",
            Commonweal="used",
            Limantor="extra", 
            San_Sim="extra", Cambria="extra", Morro="extra", Schooner="extra"
  ))
dialect.from.site <- function(site){
  res<-factor(rep(NA, length(site))  , levels=names(site_dialects))
  for(reg in names(site_dialects))
    res[site %in% site_dialects[[reg]] ] <- reg
  res
}

site_regions <- 
  list( Urban= c("BATE", "BATW", "BABE",
                 "LODU","LAME","FWSC","FOPO","FOFU", "RICH", "LAEN"),
        Rural=c("ABLA","LIMA","COMW", "ELPR", "SANS", "CAMB", "MORR", "marin")
  )

region.from.site <- function(site){
  res<-factor(rep(NA, length(site))  , levels=names(site_regions))
  for(reg in names(site_regions))
    res[site %in% site_regions[[reg]] ] <- reg
  res
}

locations.short_hand <-
  c("ABLA", "BABE", "BATE", "BATW", "COMW", "ELPR", "FOFU", "FOPO", "FWSC", 
    "LIMA", "LODU", "RICH", "LAME", "LAEN", "SANS", "CAMB", "MORR")
locations.long_hand <-
  c("Abbott's Lagoon", "Baker's Beach", "Battery East","Battery West",
    "Commonweal", "Elk Preserve", "Fort Funston", "Fort Point", "Fort Scott", 
    "Limantour", "Lobos Dunes", "Richmond", "Lake Merced", "Lands End", 
    "San Simeon", "Cambria", "Morro Bay")
names(locations.long_hand) <-locations.short_hand
dialect_pub_all <- c("Abbot", "San_Fran", "Commonweal", "Merced", "Limantor", "Richmond")
dialect_pub <- c("Abbot", "San_Fran", "Commonweal",  "Richmond")
sites_pub_all <- unlist(site_dialects[dialect_pub_all])
sites_pub <- unlist(site_dialects[dialect_pub])

dialect_full_names <- 
  c(San_Fran="San Francisco",
    Richmond="Berkeley",
    Abbot="Drake",
    Commonweal="Clear")
