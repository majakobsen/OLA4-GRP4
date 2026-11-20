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

antalsiderbb <- 33
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

antalsideras <- 25

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
