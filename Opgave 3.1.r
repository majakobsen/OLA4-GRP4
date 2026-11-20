
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

token=getToken()
res <- GET(fullurl,add_headers(Authorization = paste("Bearer", token)))
rescontent <- content(res, as="text")
resretval <- jsonlite::fromJSON(rescontent)
statedfpr <- as.data.frame(resretval$states)
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
