library(dplyr)
library(rvest)
library(tidyr)
library(stringr)
library(jsonlite)
library(lubridate)
source("https://raw.githubusercontent.com/majakobsen/OLA4-GRP4/refs/heads/main/OpenSky/util.R", chdir = TRUE)
library(ggplot2)
library(sf)
library(rnaturalearth)

# UTIL r

library(jsonlite)
library(dplyr)
library(httr)

clientId="lassekj12-api-client"
clientSecret="F3X4ijm8A3JgnTwK0REm7NWsn9MJ1CtN"
user_name <- "lassekj12@gmail.com"

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

# UTIL R

client_id <- "lassekj12-api-client"
secret <- "F3X4ijm8A3JgnTwK0REm7NWsn9MJ1CtN"

# 1) research goal
# Find SASFlights in current states/all
baseurl="https://opensky-network.org/api"

# create url
endpoint="/states/all"
toturl=str_c(baseurl,endpoint)
toturl
# get my token
token=getToken()


# lav dette til en function i util - getRawFromUrl(url)
rawres=GET(
  url = toturl,
  add_headers(Authorization = paste("Bearer", token, sep = " "))
)

rawres <- getRawFromUrl(toturl,token)

# tjek statuskode
rawres$status_code 
current=content(rawres, as="text")
currentJS=fromJSON(current)
currentJS$states
currentDF=currentJS$states
currentDF=as.data.frame(currentDF)
# scrape variables
page=read_html("https://openskynetwork.github.io/opensky-api/rest.html")
tabs=page |> html_elements("table") |> html_table()
vars=tabs[[5]]
colnames(vars)
vars$Property

PathVars=tabs[[18]]
HeaderPath <- PathVars$Property

#Time Defination

time <- 1793708000
as.POSIXct(time, origin = "1970-01-01", tz = "UTC")

#lav dette til en vector der kan bruges i colnames
#vars=rename(vars, Property=`3‐Ltr`)
names=unlist(vars$Property)
names=names[1:(length(names)-1)]
saveRDS(names,"vars.rds")


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

#For loop med 10 fly, slå Icao nummer og finde track i en liste.

currentDF10 <- currentDF[1:10, ]
ET <- "/tracks/all?"
parameter <- "icao24"
EndParameter <- "&time=0"

#FlyTracks <- vector("list", nrow(currentDF10))
#  FlyTracks[[i]] <- fromJSON(content(test2,as="text"))
token <- getToken()

dfFlights <- vector("list", nrow(currentDF10))
names(dfFlights) <- currentDF10$icao24

for (i in 1:nrow(currentDF10)){
  FlyIcao24 <- currentDF10[i, "icao24"]
  totUrl10 <- str_c(baseurl,ET,parameter,"=",FlyIcao24,EndParameter)
  test2 <- getRawFromUrl(totUrl10,token)
  
  if (test2$status_code == 200){
    rawtext2 <- content(test2, as = "text")
    myJason2 <- fromJSON(rawtext2, flatten = T)
    dfFlights[[i]] <- as.data.frame(myJason2$path)
    colnames(dfFlights[[i]]) <- HeaderPath
  }
}

#Hvis alle ti

View(dfFlights[[1]])
View(dfFlights[[2]])
View(dfFlights[[3]])
View(dfFlights[[4]])
View(dfFlights[[5]])
View(dfFlights[[6]])
View(dfFlights[[7]])
View(dfFlights[[8]])
View(dfFlights[[9]])
View(dfFlights[[10]])

#Lav tid om til læsbar tid.

for (i in 1:length(dfFlights)) {
  dfFlights[[i]]$time <- as.POSIXct(
    as.numeric(dfFlights[[i]]$time),
    origin = "1970-01-01",
    tz = "UTC"
  )
}

dfFlights2 <- as.data.frame(dfFlights2)
dfFlights2$time <- as.POSIXct(
  as.numeric(dfFlights2$time),
  origin = "1970-01-01",
  tz = "UTC"
)

plot(dfFlights2$latitude,dfFlights2$longitude)
plot(dfFlights2$time,dfFlights2$baro_altitude)
plot(dfFlights2$time,dfFlights2$true_track)



#Fly i luften per land

FlyLand <- group_by(currentDF,origin_country)
ILuftenPerland <- count(FlyLand)
ILuftenPerland <- arrange(ILuftenPerland,n)

UdvalgtDF <- currentDF |> filter(str_detect(origin_country,"Iraq"))

# find operator from callsign
currentDF$cslet=str_extract(currentDF$callsign,"[A-Z]+")
unique(currentDF$cslet)
grep("SAS",unique(currentDF$cslet))
sort(unique(currentDF$cslet))

# find list of operators from FAA and get these operators with rvest
airurl="https://www.faa.gov/air_traffic/publications/atpubs/cnt_html/chap3_section_3.html"
page=read_html(airurl)
tabclass=".table.table-scroll"
tabs=page |> html_elements(tabclass) |> html_table()
tabsdf=bind_rows(tabs)
tabsdf=rename(tabsdf, operator=`3‐Ltr`)
saveRDS(tabsdf, "operators.rds")

# join operator with the current df
currwithop=currentDF |> left_join(tabsdf)

# 



# get the airports
airports=read.csv("/Users/thor/Downloads/airports.csv")
colnames(airports)

# get iban countries 
ctrurl="https://www.iban.com/country-codes"
page=read_html(ctrurl)
ctrdf=page |> html_elements("table") |> html_table() |> as.data.frame()
ctrdf=rename(ctrdf, iso_country=Alpha.2.code)

# join with airports
airports = airports |> left_join(ctrdf,by = "iso_country")
unique(airports$type)

# current ts
tsnow=as.integer(Sys.time())-86400
tsthen=tsnow-1*86400
tsnow-tsthen

# find flights from Rome
airportsIT=airports |> filter(str_detect(Alpha.3.code, "ITA")) |> filter(type=="large_airport") |> filter(str_detect(name, "Rom"))
endpoint="/flights/departure"
parameters=str_c("?airport=LIRF&begin=",tsthen,"&end=",tsnow)
toturl=str_c(baseurl,endpoint,parameters)

romedep=GET(
  url = toturl,
  add_headers(Authorization = paste("Bearer", token, sep = " "))
)

romedep$status_code
romejs=content(romedep,as="parsed")
romedf=bind_rows(romejs)

# fly over den persiske golf
# one plane
icao24="4ba9c5"
icao24="706071"
endpoint="/tracks/all"
tykpar=str_c("?icao24=",icao24,"&time=0")
url=str_c(baseurl,endpoint,tykpar)
url
token=getToken()
trackTYK=GET(
  url=url,
  add_headers(Authorization = paste("Bearer", token, sep = " "))
)

trackTYK$status_code
trackTYK$status_code
tyk2=content(trackTYK,as="text")
tykdf2=as.data.frame(fromJSON(tyk2, flatten = T))
colnv=c("icao24","callsign","startTime","endTime","time","lat","lng","alt","crs","grd")
colnames(tykdf2)=colnv

# circling in high alt
summary(tykdf2$alt)
tykdfH2=tykdf2 |> filter(alt > 6000)
plot(tykdf$lat,tykdf$alt)
plot(tykdfH2$time,tykdfH2$crs)

sd(tykdfH$crs)
sd(tykdfH2$crs)


world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white") +
  geom_point(data = tykdf,
             aes(x = lng, y = lat),
             color = "red",
             size = 1,
             alpha = 0.5) +
  coord_sf(
    xlim = range(tykdf$lng, na.rm = TRUE) + c(-1, 1),
    ylim = range(tykdf$lat, na.rm = TRUE) + c(-1, 1)
  ) +
  theme_minimal()

