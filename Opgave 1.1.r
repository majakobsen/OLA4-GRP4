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

