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
