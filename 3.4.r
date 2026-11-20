# -------------------
# opgave 3.4 - Algoritme
# --------------------

library(dplyr)
library(tibble)
library(jsonlite)
library(httr)

# Koordinater: vest, syd, øst, nord
lomin <- 28.248586
lamin <- 28.883161
lomax <- 39.539224
lamax <- 37.300276

# Find alle fly i boksen
boxurl <- paste0(
  baseurl, "/states/all?lamin=", lamin,
  "&lomin=", lomin, "&lamax=", lamax, "&lomax=", lomax
)

token <- getToken()
boxres <- getRawFromUrl(boxurl, token)

boxDF <- as.data.frame(
  fromJSON(httr::content(boxres, as = "text"))$states
)

names(boxDF) <- names(currentDF)
icaoListe <- unique(boxDF$icao24)

HeaderPath <- c(
  "time", "latitude", "longitude",
  "baro_altitude", "true_track", "on_ground"
)

KursDF <- tibble(
  icao24 = character(),
  time = double(),
  kurs = double()
)

for (FlyIcao24 in icaoListe) {
  url <- paste0(baseurl, "/tracks/all?icao24=", FlyIcao24, "&time=0")
  svar <- getRawFromUrl(url, token)
  
  if (svar$status_code == 200) {
    rute <- as.data.frame(
      jsonlite::fromJSON(httr::content(svar, as = "text"))$path
    )
    
    if (nrow(rute) > 0) {
      names(rute) <- HeaderPath
      
      rute <- rute |>
        filter(
          latitude >= lamin, latitude <= lamax,
          longitude >= lomin, longitude <= lomax
        )
      
      KursDF <- bind_rows(
        KursDF,
        tibble(
          icao24 = FlyIcao24,
          time = as.numeric(rute$time),
          kurs = as.numeric(rute$true_track)
        )
      )
    }
  }
  
  saveRDS(KursDF, "KursDF.rds")
}

KursDrejning <- KursDF |>
  arrange(icao24, time) |>
  group_by(icao24) |>
  mutate(
    ændring = kurs - lag(kurs),
    ændring = if_else(ændring > 180, ændring - 360, ændring),
    ændring = if_else(ændring < -180, ændring + 360, ændring)
  ) |>
  summarise(
    antal_punkter = n(),
    samlet_drejning = sum(ændring, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(antal_punkter >= 10, abs(samlet_drejning) >= 360) |>
  arrange(desc(abs(samlet_drejning)))

# Markér fly fundet af din algoritme
myOwnAlgDF <- tibble(
  icao = icaoListe,
  myOwnAlg = as.integer(icaoListe %in% KursDrejning$icao24)
)

# Læs referencefly fra RDS-filerne
rdsMappe <- "C:/Users/Lasse/Desktop/R/OLA 4/OpenSky/train"

rdsFiler <- list.files(
  rdsMappe,
  pattern = "\\.rds$",
  full.names = TRUE
)

offIcao <- unique(unlist(
  lapply(rdsFiler, function(fil) readRDS(fil)$icao24)
))

# Markér referencefly med isOff = 1
isOffDF <- tibble(
  icao = icaoListe,
  isOff = as.integer(icaoListe %in% offIcao)
)

# Saml og gem resultatet
Resultat34 <- left_join(myOwnAlgDF, isOffDF, by = "icao")

saveRDS(isOffDF, "isOff.rds")
saveRDS(Resultat34, "Resultat_3_4.rds")

View(KursDF)
View(KursDrejning)
View(Resultat34)
