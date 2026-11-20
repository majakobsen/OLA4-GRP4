library(dplyr)
library(tibble)
library(jsonlite)
library(httr)

setwd("C:/Users/Lasse/Desktop/R/OLA 4/OpenSky")

# Læg disse værdier i din lokale .Renviron-fil

clientId <- "majakobsen-api-client"
clientSecret <- "BrtuOXetWxklsh4ee8CBSaI7oja9SNIT"

baseurl <- "https://opensky-network.org/api"

getToken <- function() {
  svar <- POST(
    "https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token",
    body = list(
      grant_type = "client_credentials",
      client_id = clientId,
      client_secret = clientSecret
    ),
    encode = "form"
  )
  
  content(svar, as = "parsed")$access_token
}

getRawFromUrl <- function(url, token) {
  svar <- GET(
    url,
    add_headers(Authorization = paste("Bearer", token))
  )
  
  if (svar$status_code == 401) {
    token <- getToken()
    svar <- GET(
      url,
      add_headers(Authorization = paste("Bearer", token))
    )
  }
  
  svar
}

# -------------------
# Opgave 3.4
# -------------------

# Koordinater: vest, syd, ost, nord
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
  fromJSON(content(boxres, as = "text"))$states
)

icaoListe <- unique(as.character(boxDF[[1]]))

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
      fromJSON(content(svar, as = "text"))$path
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
}

saveRDS(KursDF, "KursDF.rds")

KursDrejning <- KursDF |>
  arrange(icao24, time) |>
  group_by(icao24) |>
  mutate(
    aendring = kurs - lag(kurs),
    aendring = if_else(aendring > 180, aendring - 360, aendring),
    aendring = if_else(aendring < -180, aendring + 360, aendring)
  ) |>
  summarise(
    antal_punkter = n(),
    samlet_drejning = sum(aendring, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(antal_punkter >= 10, abs(samlet_drejning) >= 360) |>
  arrange(desc(abs(samlet_drejning)))

myOwnAlgDF <- tibble(
  icao = icaoListe,
  myOwnAlg = as.integer(icaoListe %in% KursDrejning$icao24)
)

# Læs referencefly fra RDS-filerne
rdsMappe <- "C:/Users/Lasse/Desktop/R/OLA 4/OpenSky/train"

rdsFiler <- list.files(
  rdsMappe,
  pattern = "^circ.*\\.rds$",
  full.names = TRUE
)

offIcao <- unique(unlist(
  lapply(rdsFiler, function(fil) readRDS(fil)$icao24)
))

isOffDF <- tibble(
  icao = icaoListe,
  isOff = as.integer(icaoListe %in% offIcao)
)

# sdcourse og lmres beregnes ikke i denne kode
Resultat34 <- left_join(myOwnAlgDF, isOffDF, by = "icao") |>
  mutate(
    sdcourse = NA_integer_,
    lmres = NA_integer_
  ) |>
  select(icao, sdcourse, lmres, myOwnAlg, isOff)

# -------------------
# Opgave 3.5
# -------------------

outputMappe <- "C:/Users/Lasse/Desktop/R/OLA 4/Mistænkelige fly"

# Gem hele tabellen
write.csv2(
  Resultat34,
  file.path(outputMappe, "Resultat_3_5.csv"),
  row.names = FALSE
)

saveRDS(
  Resultat34,
  file.path(outputMappe, "Resultat_3_5.rds")
)

# Gem fly fundet af jeres algoritme
mistaenkelige <- Resultat34 |>
  filter(myOwnAlg == 1)

tid <- format(Sys.time(), "%Y%m%d_%H%M%S")

csvfil <- file.path(
  outputMappe,
  paste0("mistaenkelige_fly_", tid, ".csv")
)

rdsfil <- file.path(
  outputMappe,
  paste0("mistaenkelige_fly_", tid, ".rds")
)

write.csv2(mistaenkelige, csvfil, row.names = FALSE)
saveRDS(mistaenkelige, rdsfil)

