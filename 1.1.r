# -------------------
# opgave 1 - Webscraping
# --------------------

# -------------------
# Opgave 1.1 - Bilbasen
# -------------------

library(chromote)
library(rvest)
library(dplyr)
library(logr)
library(stringr)

bilbasen_base <- "https://www.bilbasen.dk/brugt/bil/vw?fuel=1&includeengroscvr=true&includeleasing=false"

bilbasen_browser <- ChromoteSession$new()
bilbasen_browser$go_to(bilbasen_base)
Sys.sleep(4)
bilbasen_browser$view()
biler_bilbasen <- data.frame()

log_open(file_name = "log.txt")

# Hvor mange sider skal scrapes?
antalsiderbb <- 1

for (bilbasen_side in 1:antalsiderbb) {
  
  bilbasen_side_url <- paste0(bilbasen_base, "&page=", bilbasen_side)
  
  bilbasen_browser$Page$navigate(url = bilbasen_side_url)
  log_print(bilbasen_side_url)
  Sys.sleep(4)
  
  bilbasen_html <- bilbasen_browser$Runtime$evaluate(
    "document.documentElement.outerHTML",
    returnByValue = TRUE
  )$result$value
  
  bilbasen_page <- read_html(bilbasen_html)
  
  bilbasen_carlist <- bilbasen_page |>
    html_elements(
      xpath = '//div[div[@data-e2e="search-result-make-model"]]'
    )
  
  for (bilbasen_car in bilbasen_carlist) {
    
    egenskaber <- bilbasen_car |>
      html_elements('[class*="Listing_properties__"] > div') |>
      html_text2()
    
    model_felt <- bilbasen_car |>
      html_element('[data-e2e="search-result-make-model"]')
    
    overskrift <- model_felt |>
      html_element('[data-e2e="listing-make-model"]') |>
      html_text2()
    
    # Del overskriften i mærke og model
    maerke <- toupper(sub(" .*", "", overskrift))
    model <- toupper(sub("^\\S+\\s*", "", overskrift))
    
    type <- trimws(sub(
      overskrift, "", html_text2(model_felt), fixed = TRUE
    ))
    
    link <- bilbasen_car |>
      html_elements('a[href*="/brugt/bil/"]') |>
      html_attr("href")
    
    link <- link[!is.na(link) & grepl("/[0-9]+([/?#]|$)", link)][1]
    
    if (is.na(link)) next
    
    link <- url_absolute(link, "https://www.bilbasen.dk")
    link <- sub("[?#].*$", "", link)
    id <- sub(".*/([0-9]+)/?$", "\\1", link)
    
    log_print(paste("Fundet bil-URL:", link))
    
    bilbasen_bil <- data.frame(
      maerke = maerke,
      model = model,
      type = toupper(type),
      
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
        html_element("img.Listing_dealerLogoCard__wHl9H") |>
        html_attr("src") |>
        str_extract("(?<=info/).*?(?=\\.jpeg)") |>
        coalesce("Intet forhandler logo tilgængeligt"),
      
      link = link,
      id = id,
      tid = Sys.time()
    )
    
    biler_bilbasen <- bind_rows(biler_bilbasen, bilbasen_bil)
  }
}

log_close()

if (nrow(biler_bilbasen) > 0) {
  
  biler_bilbasen <- biler_bilbasen |>
    distinct(id, .keep_all = TRUE)
  
  bil_urls_bilbasen <- biler_bilbasen |>
    select(id, url = link)
  
  write.csv2(biler_bilbasen, "bilbasen.csv", row.names = FALSE)
  write.csv2(bil_urls_bilbasen, "bil_urls.csv", row.names = FALSE)
  
  View(biler_bilbasen)
  View(bil_urls_bilbasen)
}
