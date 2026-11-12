# -------------------
# opgave 3.2 - Start small - ét fly
# --------------------
# Forbered ét fly
FlyEt <- data.frame(icao24 = icaov$icao24[5]) |>
  left_join(currentDF, by = "icao24")

# Hent flyets rute
token <- getToken()
turl <- paste0(baseurl, endpointT, "?icao24=", FlyEt$icao24, "&time=0")

res <- httr::GET(
  turl,
  httr::add_headers(Authorization = paste("Bearer", token))
)

res$status_code
resretval <- jsonlite::fromJSON(httr::content(res, as = "text"))
statedf <- as.data.frame(resretval)

colnv <- c("icao24", "callsign", "startTime", "endTime", "time",
           "lat", "lng", "alt", "crs", "grd")
colnames(statedf) <- colnv

# Plot flyets data og rute
myEDAPlots(statedf)
Plot1 <- myMapPlot(statedf)
Plot1

# Plot cirkelflyets rute
CirkleneFly <- as.data.frame(
  readRDS("C:/Users/Lasse/Desktop/R/OLA 4/OpenSky/train/circMT.rds")
)
View(CirkleneFly)

Plot2 <- myMapPlot(CirkleneFly)
Plot2
