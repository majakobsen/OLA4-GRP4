# -------------------
# opgave 1 - Webscraping
# --------------------

# -------------------
# Opgave 1.1 - Bilbasen
# -------------------

library(chromote)
library(rvest)
library(dplyr)
library(stringr)

hent_tekst <- function(x, selector) {
  x |> html_element(selector) |> html_text2()
}

bilbasen_base <- "https://www.bilbasen.dk/brugt/bil/bmw/i4?fuel=3&includeengroscvr=true&includeleasing=false&make=bmw&model-bmw=i5&model-bmw=i7&model-bmw=i3&model-bmw=i8"

antalsiderbb <- 1
bilbasen_browser <- ChromoteSession$new()
biler_bilbasen <- data.frame()

for (side in 1:antalsiderbb) {
  bilbasen_browser$go_to(paste0(bilbasen_base, "&page=", side))
  Sys.sleep(4)
  
  html <- bilbasen_browser$Runtime$evaluate(
    "document.documentElement.outerHTML",
    returnByValue = TRUE
  )$result$value
  
  billiste <- read_html(html) |>
    html_elements(xpath = '//div[div[@data-e2e="search-result-make-model"]]')
  
  for (bil in billiste) {
    model_felt <- bil |>
      html_element('[data-e2e="search-result-make-model"]')
    
    overskrift <- hent_tekst(
      model_felt,
      '[data-e2e="listing-make-model"]'
    )
    
    egenskaber <- bil |>
      html_elements('[class*="Listing_properties__"] > div') |>
      html_text2()
    
    lokation <- hent_tekst(bil, '[class*="Listing_location__"]') |>
      str_split_fixed(",", 2)
    
    link <- bil |>
      html_element('a[href*="/brugt/bil/"]') |>
      html_attr("href")
    
    forhandler <-  bil |>
      html_element('img[class*="Listing_dealerLogoCard__"]') |>
      html_attr("src") |>
      str_extract("(?<=info/).*?(?=\\.jpeg)")
    
    bil_data <- data.frame(
      maerke = toupper(word(overskrift, 1)),
      model = toupper(str_squish(overskrift)),
      pris = hent_tekst(bil, '[data-e2e="search-result-price"]'),
      registrering = egenskaber[1],
      kilometer = egenskaber[2],
      by = str_trim(lokation[1, 1]),
      region = str_trim(lokation[1, 2]),
      forhandler = bil |>
        html_element('img[class*="Listing_dealerLogoCard__"]') |>
        html_attr("src") |>
        str_extract("(?<=info/).*?(?=\\.jpeg)"),
      link = link,
      id = str_extract(link, "[0-9]+/?$") |> str_remove("/$")
    )
    
    biler_bilbasen <- bind_rows(biler_bilbasen, bil_data)
  }
}

bilbasen_browser$close()

biler_bilbasen <- biler_bilbasen |>
  distinct(id, .keep_all = TRUE)

write.csv2(biler_bilbasen, "bilbasen.csv", row.names = FALSE)

View(biler_bilbasen)

saveRDS(biler_bilbasen, "Bilbasen-Biler.rds")

forhandlere_bilbasen <- biler_bilbasen |>
  filter(!is.na(forhandler)) |>
  count(forhandler, region, name = "antal-biler-til-salg") |>
  rename(
    `Forhandler-id` = forhandler,
    Region = region
  ) |>
  arrange(desc(`antal-biler-til-salg`))

View(forhandlere_bilbasen)

# -------------------
# Opgave 1.2 - AutoScout24 Tyskland
# -------------------

autoscout_base <- paste0(
  "https://www.autoscout24.com/lst?cat=ma13mo20319%2Cma13mo75941%2Cma13mo76505%2Cma13mo76318%2Cma13mo20320&sort=standard&desc=0&ustate=N%2CU&cy=D&ocs_listing=include&damaged_listing=exclude&atype=C&source=homepage_search-mask"
)

autoscout_browser <- ChromoteSession$new()
bilertyskland <- data.frame()
autoscout_browser$view()


#Hvor mange sider skal scrapes

antalsideras <- 1

for (autoscout_side in 1:antalsideras) {
  autoscout_side_url <- paste0(autoscout_base, "&page=", autoscout_side)
  
  autoscout_browser$Page$navigate(url = autoscout_side_url)
  Sys.sleep(4)
  
  autoscout_html <- autoscout_browser$Runtime$evaluate(
    "document.documentElement.outerHTML",
    returnByValue = TRUE
  )$result$value
  
  autoscout_page <- read_html(autoscout_html)
  autoscout_carlist <- autoscout_page |> html_elements("article")
  
  for (autoscout_car in autoscout_carlist) {
    autoscout_bil <- data.frame(
      model = autoscout_car |>
        html_element("h2 > span:first-child") |>
        html_text2(),
      
      type = autoscout_car |>
        html_element("h2 > span:nth-child(2)") |>
        html_text2(),
      
      kilometer = autoscout_car |>
        html_element('[data-testid="VehicleDetails-mileage_odometer"]') |>
        html_text2(),
      
      
      registreringsdato = autoscout_car |>
        html_element('[data-testid="VehicleDetails-calendar"]') |>
        html_text2(),
      
      
      prisEUR = as.numeric(gsub(
        "[^0-9]", "",
        autoscout_car |>
          html_element('[data-testid="regular-price"]') |>
          html_text2()
      )))
    
    bilertyskland <- bind_rows(bilertyskland, autoscout_bil)
  }
}

View(bilertyskland)

#Omregn til DKK kurs 7,48

bilertyskland$dkkpris <- bilertyskland$prisEUR*7.5

# Beregner elbilens registreringsafgift: først almindelig afgift efter bilens værdi,
# derefter 40 % af afgiften minus elbilens fradrag på 161.300 kr.

bilertyskland$ImportOmk <- pmax(
  0,
  (
    pmin(bilertyskland$dkkpris, 76400) * 0.25 +
      pmin(pmax(bilertyskland$dkkpris - 76400, 0), 237400 - 76400) * 0.85 +
      pmax(bilertyskland$dkkpris - 237400, 0) * 1.50 -
      25500
  ) * 0.40 - 161300
) + 5000



bilertyskland$TotalDKKPris <- bilertyskland$ImportOmk + bilertyskland$dkkpris 

#Sammenligning samme modeller fra de to datasæt

bilertyskland$model <- tolower(bilertyskland$model)
biler_bilbasen$model <- tolower(biler_bilbasen$model)

bilertyskland$modelserie <- str_extract(bilertyskland$model, "\\bi[34578]\\b")
biler_bilbasen$modelserie <- str_extract(biler_bilbasen$model, "\\bi[34578]\\b")

View(bilertyskland)

sammenligning <- merge(
  bilertyskland[, c("modelserie", "TotalDKKPris")],
  biler_bilbasen[, c("modelserie", "pris")],
  by = "modelserie"
)

sammenligning <- sammenligning |>
  mutate(pris = as.numeric(gsub("[^0-9]", "", pris))) |>
  group_by(modelserie) |>
  summarise(
    median_totalimport = median(TotalDKKPris, na.rm = TRUE),
    median_bilbasen_pris = median(pris, na.rm = TRUE),
    forskel = median_bilbasen_pris - median_totalimport,
    .groups = "drop"
  )

View(sammenligning)


# -------------------
# Opgave 1.3 - POWER
# -------------------

library(chromote)
library(rvest)
library(dplyr)

power_browser <- ChromoteSession$new()

power_browser$Emulation$setDeviceMetricsOverride(
  width = 1440,
  height = 1000,
  deviceScaleFactor = 1,
  mobile = FALSE
)

power_browser$Page$navigate(
  url = "https://www.power.dk/kampagne/aktuelt/"
)

power_browser$view()
Sys.sleep(10)

power_browser$Runtime$evaluate("window.scrollTo(0, 0)")
Sys.sleep(5)

power <- data.frame()

for (i in 1:40) {
  
  Sys.sleep(2)
  
  power_html <- power_browser$Runtime$evaluate(
    "document.documentElement.outerHTML",
    returnByValue = TRUE
  )$result$value
  
  power_page <- read_html(power_html)
  
  produktliste <- power_page |>
    html_elements("article.product-item")
  
  if (length(produktliste) == 0) {
    produktliste <- power_page |>
      html_elements("pwr-product-item-v2")
  }
  
  for (produkt in produktliste) {
    
    navn <- produkt |>
      html_element("h3 a") |>
      html_text2()
    
    link <- produkt |>
      html_element("h3 a") |>
      html_attr("href")
    
    if (is.na(navn) || navn == "" || is.na(link)) next
    
    detaljer <- produkt |>
      html_elements(
        ".product-item-description-container, .product-item-arguments-container"
      ) |>
      html_text2()
    
    produkt_data <- data.frame(
      navn = navn,
      
      pris = produkt |>
        html_element("data.main-price") |>
        html_attr("value") |>
        as.numeric(),
      
      tilbud = produkt |>
        html_element("pwr-product-price-savings-label .savings-label") |>
        html_text2(),
      
      bedommelse = produkt |>
        html_element('pwr-product-review [aria-label^="Bedømmelse"]') |>
        html_attr("aria-label"),
      
      detaljer = paste(detaljer, collapse = "; "),
      link = url_absolute(link, "https://www.power.dk")
    )
    
    power <- bind_rows(power, produkt_data)
  }
  
  power_browser$Runtime$evaluate("window.scrollBy(0, 400)")
}

power <- power |>
  arrange(is.na(pris)) |>
  distinct(link, .keep_all = TRUE)

View(power)

write.csv2(power, "power.csv", row.names = FALSE)

power_browser$close()


# -------------------
# Opgave 2 - SQL
# -------------------

library(RMariaDB)
library(DBI)
library(dplyr)

con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "127.0.0.1",
  port = 3306,
  username = "root",
  password = "root123"
)

dbExecute(con, "USE bilbasen_ola")

# 2.1
# Opret tabellerne

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS Forhandler (
    forhandlerid VARCHAR(100) PRIMARY KEY
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS Bil (
    carid VARCHAR(30) PRIMARY KEY,
    maerke VARCHAR(100),
    model VARCHAR(255),
    registrering VARCHAR(30),
    link TEXT,
    forhandlerid VARCHAR(100),
    FOREIGN KEY (forhandlerid)
      REFERENCES Forhandler(forhandlerid)
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS BilObservation (
    observationid INT AUTO_INCREMENT PRIMARY KEY,
    carid VARCHAR(30) NOT NULL,
    scrapedate DATETIME NOT NULL,
    pris DECIMAL(12,2),
    kilometer INT,
    sold BOOLEAN DEFAULT FALSE,
    forrige_observationid INT,
    FOREIGN KEY (carid)
      REFERENCES Bil(carid),
    FOREIGN KEY (forrige_observationid)
      REFERENCES BilObservation(observationid)
  )
")

dbListTables(con)

# 2.2
# Gem foerste scrape i de tre tabeller
# Koer kun indsaettelserne nedenfor paa tomme tabeller.

biler <- readRDS("Bilbasen-Biler.rds") |>
  distinct(id, .keep_all = TRUE) |>
  mutate(
    carid = as.character(id),
    forhandlerid = na_if(trimws(as.character(forhandler)), "")
  )

# Her bruges importtidspunktet. Erstat med det faktiske
# scrapetidspunkt, hvis data blev hentet tidligere.
# Eksempel paa format: as.POSIXct("2026-11-19 10:00:00", tz = "Europe/Copenhagen")
scrapedato <- Sys.time()

# Forhandlere
forhandlere <- biler |>
  filter(!is.na(forhandlerid)) |>
  distinct(forhandlerid)

dbWriteTable(con, "Forhandler", forhandlere,
             append = TRUE, row.names = FALSE)

# Faste biloplysninger
biloplysninger <- biler |>
  select(carid, maerke, model, registrering, link, forhandlerid)

dbWriteTable(con, "Bil", biloplysninger,
             append = TRUE, row.names = FALSE)

# Foerste observation: tidligere observation er automatisk NULL.
observationer <- biler |>
  transmute(
    carid,
    scrapedate = scrapedato,
    pris = as.numeric(gsub("[^0-9]", "", as.character(pris))),
    kilometer = as.integer(gsub("[^0-9]", "", as.character(kilometer))),
    sold = FALSE
  )

dbWriteTable(con, "BilObservation", observationer,
             append = TRUE, row.names = FALSE)

# Se data fra de sammenkoblede tabeller
BilerFraDatabase <- dbGetQuery(con, "
  SELECT b.carid, b.maerke, b.model, b.registrering,
         b.link, f.forhandlerid,
         o.scrapedate, o.pris, o.kilometer, o.sold
  FROM Bil b
  LEFT JOIN Forhandler f ON f.forhandlerid = b.forhandlerid
  JOIN BilObservation o ON o.carid = b.carid
")

View(BilerFraDatabase)



# -------------------
# opgave 3 - Opensky
# --------------------

library(dplyr)
library(ggplot2)
library(gridExtra)
library(jsonlite)
library(httr)
library(rvest)
library(stringr)
library(lubridate)
library(leaflet)

clientId="emmahojelse-api-client"
clientSecret="6UGXoVlxS86c38OefuHPvP77baT7Mgkf"

# Request an access token

getToken <- function() {
  token=NULL
  response <- POST(
    url = "https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token",
    body = list(
      grant_type = "client_credentials",
      client_id = clientId,
      client_secret = clientSecret
    ),
    encode = "form"
  )
  
  # Parse the JSON response
  response$status_code
  if (response$status_code == 200) {
    content <- content(response, as = "parsed", type = "application/json")
    token <- content$access_token
  }
  return(token)
}

getRawFromUrl <- function(url,token) {
  rawres=GET(
    url,
    add_headers(Authorization = paste("Bearer", token, sep = " "))
  )
  status <- rawres$status_code
  if(status==401){
    token <- getToken() 
  }
  return(rawres)
}

baseurl <- "https://opensky-network.org/api"
endpointS <- "/states/all"
endpointT <- "/tracks/all"

#### NORTH SEA ####
lamin=52.055129
lamax=56.196869
lomin=-6.065140
lomax=4.305954
fullurl=paste0(baseurl,endpointS,"?lamin=",lamin,"&lomin=",lomin,"&lamax=",lamax,"&lomax=",lomax)

# one take over NS (skateboard)
token=getToken()
res <- GET(fullurl,add_headers(Authorization = paste("Bearer", token)))
rescontent <- content(res, as="text")
resretval <- jsonlite::fromJSON(rescontent)
statedfpr <- as.data.frame(resretval$states)
# find all icaos and put in vector
icaov=statedfpr$V1

baseurl="https://opensky-network.org/api"
endpoint="/states/all"
toturl=str_c(baseurl,endpoint)
token=getToken()
rawres=GET(
  url = toturl,
  add_headers(Authorization = paste("Bearer", token, sep = " "))
)
rawres <- getRawFromUrl(toturl,token)

rawres$status_code 
current=content(rawres, as="text")
currentJS=fromJSON(current)
currentJS$states
currentDF=currentJS$states
currentDF=as.data.frame(currentDF)
page=read_html("https://openskynetwork.github.io/opensky-api/rest.html")
tabs=page |> html_elements("table") |> html_table()
vars=tabs[[5]]
colnames(vars)
vars$Property
names=unlist(vars$Property)
names=names[1:(length(names)-1)]
colnames(currentDF)=names
currentDF$time_position <- as.POSIXct(
  as.numeric(currentDF$time_position),
  origin = "1970-01-01",
  tz = "UTC"
)
currentDF$last_contact <- as.POSIXct(
  as.numeric(currentDF$last_contact),
  origin = "1970-01-01",
  tz = "UTC"
)

icaov <- as.data.frame(icaov)

View(icaov)

icaov <- icaov |> rename(icao24 = icaov)

View(icaov)
View(currentDF)

icaov <- icaov |> left_join(currentDF, by = "icao24")

count(icaov, origin_country)
Antalfly <- as.data.frame(count(icaov, origin_country))
View(Antalfly)

Antalfly <- Antalfly |> arrange(n, desc(n))

ggplot(Antalfly, aes(x = reorder(origin_country, -n), y = n)) +
  geom_col(fill = "#287C8E", width = 0.75) +
  geom_text(aes(label = n), vjust = -0.3) +
  labs(
    x = "Land",
    y = "Antal fly",
    caption = "Kilde: OpenSky, egne beregninger",
    title = "Flest fly over Nordsøen kommer fra Storbritannien",
    subtitle = "Antal fly fordelt på oprindelsesland"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, color = "grey40")
  )


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

