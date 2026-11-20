
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

