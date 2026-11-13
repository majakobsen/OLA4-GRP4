
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
