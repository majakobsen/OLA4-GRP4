
# -------------------
# opgave 3.2 - Start small - ét fly
# --------------------
# Forbered ét fly
FlyEt <- data.frame(icao24 = icaov$icao24[5]) |>
  left_join(currentDF, by = "icao24")

# Hent flyets rute
token <- getToken()
turl <- paste0(baseurl, endpointT, "?icao24=", FlyEt$icao24, "&time=0")

res <- GET(
  turl,
  add_headers(Authorization = paste("Bearer", token))
)

res$status_code
resretval <- fromJSON(content(res, as = "text"))
statedf <- as.data.frame(resretval$path)
colnv <- c("time", "lat", "lng", "alt", "crs", "grd")
colnames(statedf) <- colnv
statedf$lat <- as.numeric(statedf$lat)
statedf$lng <- as.numeric(statedf$lng)

library(leaflet)

myMapPlot <- function(df) {
  leaflet(df) %>%
    addTiles() %>%
    addPolylines(lat = ~lat, lng = ~lng)
}

myEDAPlots <- function(df) {
  par(mfcol = c(1, 3), ann = FALSE)
  plot(df$lat, df$lng)
  plot(df$crs, df$time)
  plot(df$alt, df$time)
}


# Plot flyets data og rute
myEDAPlots(statedf)
Plot1 <- myMapPlot(statedf) |>
  addControl(
    paste("ICAO:", FlyEt$icao24, "Callsign:", FlyEt$callsign),
    position = "topright"
  )
Plot1


# Plot cirkelflyets rute
CirkleneFly <- as.data.frame(
  readRDS("C:/Users/Lasse/Desktop/R/OLA 4/OpenSky/train/circMT.rds")
)
View(CirkleneFly)

Plot2 <- myMapPlot(CirkleneFly) |>
  addControl(
    paste(
      "ICAO:", CirkleneFly$icao24[1],
      "Callsign:", trimws(CirkleneFly$callsign[1])
    ),
    position = "topright"
  )

Plot2

