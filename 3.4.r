# --------------------
# Opgave 3.4 - Algoritme
# --------------------

View(currentDF)

CurrentDF9000 <- currentDF |>
  filter(as.numeric(baro_altitude) > 9000)

View(CurrentDF9000)

HeaderPath <- c(
  "time", "latitude", "longitude",
  "baro_altitude", "true_track", "on_ground"
)

icaoListe <- head(CurrentDF9000$icao24, 10)

KursDF <- tibble(
  icao24 = character(),
  time = double(),
  kurs = double()
)

token <- getToken()

for (FlyIcao24 in icaoListe) {
  
  url <- str_c(
    baseurl, "/tracks/all?icao24=", FlyIcao24, "&time=0"
  )
  
  svar <- getRawFromUrl(url, token)
  
  print(paste(FlyIcao24, "status:", svar$status_code))
  
  if (svar$status_code == 429) break
  
  if (svar$status_code != 200) next
  
  flydata <- fromJSON(content(svar, as = "text"))
  rute <- as.data.frame(flydata$path)
  
  print(paste("Antal rutepunkter:", nrow(rute)))
  
  if (nrow(rute) == 0) next
  
  colnames(rute) <- HeaderPath
  
  KursDF <- bind_rows(
    KursDF,
    tibble(
      icao24 = FlyIcao24,
      time = as.numeric(rute$time),
      kurs = as.numeric(rute$true_track)
    )
  )
}

saveRDS(KursDF, "KursDF.rds")

View(KursDF)
