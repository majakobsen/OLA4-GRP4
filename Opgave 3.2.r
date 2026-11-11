# -------------------
# opgave 3.2 - Start small - ét fly
# --------------------

#Forbered Fly Et

FlyEt <- data.frame(icao24 = icaov$icao24[5])

FlyEt <- FlyEt |>
  left_join(currentDF, by = "icao24")

token=getToken()
turl=paste0(baseurl,endpointT,"?icao24=",FlyEt$icao24, "&time=0")
res <- httr::GET(turl,add_headers(Authorization = paste("Bearer", token)))
res$status_code
resretval <- fromJSON(content(res, as="text"))
statedf <- as.data.frame(resretval)
colnv=c("icao24","callsign","startTime","endTime","time","lat","lng","alt","crs","grd")
colnames(statedf)=colnv


#Plot FlyEt

myEDAPlots(statedf)
Plot1 <- myMapPlot(statedf)
Plot1

#Forbered Cirklene Fly

CirkleneFly <- as.data.frame(readRDS("C:/Users/Lasse/Desktop/R/OpenSky/train/circMT.rds"))
View(CirkleneFly)

#Plot Cirklene Fly

Plot2 <- myMapPlot(CirkleneFly)
Plot2
