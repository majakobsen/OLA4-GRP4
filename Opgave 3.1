

# -------------------
# opgave 3.1
# --------------------

library(dplyr)
library(ggplot2)
library(gridExtra)

#Hent GetCircI.R som indeholder nordsø bonduty box
source("C:/Users/Lasse/Desktop/R/OLA 4/OpenSky/getCircI.R", chdir = TRUE)

#Hent GetPlanes.R som indeholder currentDF, som man kan hente lande fra
source("C:/Users/Lasse/Desktop/R/OLA 4/OpenSky/getPlanes.R", chdir = TRUE)


icaov <- as.data.frame(icaov)

View(icaov)

icaov <- icaov |> rename(icao24 = icaov)

View(icaov)
View(currentDF)

icaov <- icaov |> left_join(currentDF, by = "icao24")


count(icaov, origin_country)
Antalfly <- as.data.frame(count(icaov, origin_country))

ggplot(Antalfly, aes(x = origin_country, y = n)) +
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
