
# -------------------
# opgave 3.3 - Move fast - alle fly
# --------------------

View(currentDF)

# Brug de første 10 fly som test
currentDF10 <- data.frame(icao24 = currentDF$icao24[1:10])

HeaderPath <- c(
  "time", "latitude", "longitude",
  "baro_altitude", "true_track", "on_ground"
)

dfFlights <- vector("list", nrow(currentDF10))
names(dfFlights) <- currentDF10$icao24
KursSD <- rep(NA_real_, nrow(currentDF10))

token <- getToken()

for (i in 1:nrow(currentDF10)) {
  FlyIcao24 <- currentDF10$icao24[i]
  totUrl10 <- str_c(
    baseurl, "/tracks/all?icao24=", FlyIcao24, "&time=0"
  )
  test2 <- getRawFromUrl(totUrl10, token)
  
  if (test2$status_code == 200) {
    rawtext2 <- content(test2, as = "text")
    myJason2 <- fromJSON(rawtext2, flatten = TRUE)
    dfFlights[[i]] <- as.data.frame(myJason2$path)
    colnames(dfFlights[[i]]) <- HeaderPath
    KursSD[i] <- sd(dfFlights[[i]]$true_track, na.rm = TRUE)
  }
}

KursResultat <- data.frame(
  icao24 = currentDF10$icao24,
  sd_kurs = KursSD
)

View(KursResultat)
