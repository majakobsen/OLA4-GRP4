# -------------------
# opgave 1 - Webscraping
# --------------------

# -------------------
# opgave 1.1 - Bilbasen
# -------------------

library(chromote)
library(rvest)
library(dplyr)
library(logr)

# Base URL indeholder VW og fuel = 1
bilbasen_base <- "https://www.bilbasen.dk/brugt/bil/vw?fuel=1&includeengroscvr=true&includeleasing=false"

log_open(file_name = "log.txt")

bilbasen_browser <- ChromoteSession$new()
bilbasen_browser$Page$navigate(url = bilbasen_base)
log_print(bilbasen_base)
bilbasen_browser$view()

biler_bilbasen <- data.frame()
bil_urls_bilbasen <- data.frame(id = character(), url = character())

for (bilbasen_side in 1:10) {
  
  bilbasen_side_url <- paste0(bilbasen_base, "&page=", bilbasen_side)
  
  bilbasen_browser$Page$navigate(url = bilbasen_side_url)
  log_print(bilbasen_side_url)
  Sys.sleep(4)
  
  bilbasen_html <- bilbasen_browser$Runtime$evaluate(
    "document.documentElement.outerHTML",
    returnByValue = TRUE
  )$result$value
  
  bilbasen_page <- read_html(bilbasen_html)
  
  bilbasen_carlist <- bilbasen_page |> html_elements(
    xpath = '//div[div[@data-e2e="search-result-make-model"]]'
  )
  
  if (length(bilbasen_carlist) == 0) break
  
  side_biler_bilbasen <- data.frame()
  side_urls_bilbasen <- data.frame(id = character(), url = character())
  
  for (bilbasen_car in bilbasen_carlist) {
    egenskaber <- bilbasen_car |>
      html_elements('[class*="Listing_properties__"] > div') |>
      html_text2()
    
    model_felt <- bilbasen_car |>
      html_element('[data-e2e="search-result-make-model"]')
    
    overskrift <- model_felt |>
      html_element('[data-e2e="listing-make-model"]') |>
      html_text2()
    
    dele <- strsplit(overskrift, "\\s+")[[1]]
    maerke <- toupper(dele[1])
    model <- toupper(paste(dele[-1], collapse = " "))
    
    tekst_felt <- html_text2(model_felt)
    type <- trimws(sub(
      paste0("^", overskrift),
      "",
      tekst_felt
    ))
    
    # Fx "2,0 FSi Comfortline 4d" bliver til "2,0 COMFORT"
    if (grepl("comfortline", type, ignore.case = TRUE)) {
      motor <- sub(
        "^([0-9]+,[0-9]+).*",
        "\\1",
        type
      )
      type <- paste(motor, "COMFORT")
    }
    
    type <- toupper(type)
    
    link <- bilbasen_car |>
      html_elements('a[href*="/brugt/bil/"]') |>
      html_attr("href")
    
    link <- link[
      !is.na(link) & grepl("/[0-9]+([/?#]|$)", link)
    ][1]
    
    if (is.na(link)) next
    
    if (startsWith(link, "/")) {
      link <- paste0("https://www.bilbasen.dk", link)
    }
    
    link <- sub("[?#].*$", "", link)
    id <- sub(".*/([0-9]+)/?$", "\\1", link)
    
    side_urls_bilbasen <- bind_rows(
      side_urls_bilbasen,
      data.frame(id = id, url = link)
    )
    
    bilbasen_bil <- data.frame(
      maerke = maerke,
      model = model,
      type = type,
      pris = bilbasen_car |>
        html_element('[data-e2e="search-result-price"]') |>
        html_text2(),
      registrering = egenskaber[1],
      kilometer = egenskaber[2],
      km_pr_liter = egenskaber[3],
      detaljer = bilbasen_car |>
        html_element('[class*="Listing_details__"]') |>
        html_text2(),
      lokation = bilbasen_car |>
        html_element('[class*="Listing_location__"]') |>
        html_text2(),
      forhandler = bilbasen_car |>
        html_element('a[href*="/forhandler/"]') |>
        html_text2(),
      link = link,
      id = id
    )
    
    side_biler_bilbasen <- bind_rows(side_biler_bilbasen, bilbasen_bil)
  }
  
  if (nrow(side_biler_bilbasen) == 0) break
  if (nrow(biler_bilbasen) > 0 && all(side_biler_bilbasen$id %in% biler_bilbasen$id)) break
  
  nye_urls_bilbasen <- side_urls_bilbasen |>
    filter(!id %in% bil_urls_bilbasen$id) |>
    distinct(id, .keep_all = TRUE)
  
  for (bil_url_bilbasen in nye_urls_bilbasen$url) {
    log_print(paste("Fundet bil-URL:", bil_url_bilbasen))
  }
  
  bil_urls_bilbasen <- bind_rows(bil_urls_bilbasen, nye_urls_bilbasen)
  
  biler_bilbasen <- bind_rows(biler_bilbasen, side_biler_bilbasen) |>
    distinct(id, .keep_all = TRUE)
  
  write.csv2(biler_bilbasen, "bilbasen.csv", row.names = FALSE)
  write.csv2(bil_urls_bilbasen, "bil_urls.csv", row.names = FALSE)
}

log_close()

View(biler_bilbasen)

#KØR KUN HVIS MAN VIL GEMME EN NY SCRAPE I BILERFAST

BilerFast_bilbasen <- biler_bilbasen

View(bil_urls_bilbasen)

# -------------------
# Opgave 1.2 - AutoScout24 Tyskland
# -------------------

autoscout_base <- "https://www.autoscout24.com/lst/volkswagen/ft_gasoline?cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&powertype=kw&sort=standard&ustate=N%2CU&page=1&atype=C&search_id=1cpi43x08l4&source=listpage_pagination"

autoscout_browser <- ChromoteSession$new()
bilertyskland <- data.frame()

for (autoscout_side in 1:10) {
  autoscout_side_url <- sub("page=[0-9]+", paste0("page=", autoscout_side), autoscout_base)
  
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
      
      pris = as.numeric(gsub(
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

bilertyskland$DKK <- bilertyskland$pris*7.48

#Sammenligning samme modeller fra de to datasæt



tyskland <- bilertyskland |>
  mutate(model_match = toupper(sub("^Volkswagen\\s+", "", model))) |>
  group_by(model_match) |>
  summarise(
    pris_tyskland = median(DKK, na.rm = TRUE),
    .groups = "drop"
  )

bilbasen <- BilerFast_bilbasen |>
  mutate(
    model_match = toupper(model),
    pris_bilbasen = as.numeric(
      gsub("\\.", "", gsub("[^0-9.]", "", pris))
    )
  ) |>
  group_by(model_match) |>
  summarise(
    pris_bilbasen = median(pris_bilbasen, na.rm = TRUE),
    .groups = "drop"
  )

sammenligning <- inner_join(
  tyskland,
  bilbasen,
  by = "model_match"
) |>
  mutate(prisforskel = pris_bilbasen - pris_tyskland)

View(sammenligning)


# -------------------
# opgave 1.3 - POWER 
# --------------------

# -------------------
# Opgave 1.3 - POWER
# -------------------

library(chromote)
library(rvest)

power_browser <- ChromoteSession$new()
power_browser$Page$navigate(url = "https://www.power.dk")
power_browser$view()
Sys.sleep(15)

power_html <- power_browser$Runtime$evaluate(
  "document.documentElement.outerHTML",
  returnByValue = TRUE
)$result$value

power_page <- read_html(power_html)

# Hent alle produktkort på siden
navne <- power_page |>
  html_elements('[class*="product-item-title-container"] h3 a')

priser <- power_page |>
  html_elements(".main-price")

tilbud <- power_page |>
  html_elements("pwr-product-price-savings-label .savings-label")

bedommelser <- power_page |>
  html_elements('pwr-product-review [aria-label^="Bedømmelse"]')

detalje_felter <- power_page |>
  html_elements('[class*="product-item-arguments-container"]')

# Saml detaljerne for hvert produktkort
detaljer <- vapply(detalje_felter, function(felt) {
  tekst <- felt |>
    html_elements(".subtitle-2") |>
    html_text2()
  
  paste(tekst, collapse = "; ")
}, character(1))

c(
  navne = length(power_page |> html_elements('[class*="product-item-title-container"] h3 a')),
  priser = length(power_page |> html_elements(".main-price")),
  tilbud = length(power_page |> html_elements("pwr-product-price-savings-label .savings-label")),
  bedommelser = length(power_page |> html_elements('pwr-product-review [aria-label^="Bedømmelse"]')),
  detaljer = length(power_page |> html_elements('[class*="product-item-arguments-container"]'))
)

bedommelse <- rep(NA_character_, length(navne))

if (length(bedommelser) > 0) {
  bedommelse[seq_along(bedommelser)] <- html_attr(
    bedommelser,
    "aria-label"
  )
}

# Saml oplysningerne i én dataframe
power <- data.frame(
  navn = html_text2(navne),
  pris = html_attr(priser, "value"),
  tilbud = html_text2(tilbud),
  bedommelse = bedommelse,
  detaljer = detaljer
)

View(power)
