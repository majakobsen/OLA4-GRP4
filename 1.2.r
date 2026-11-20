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
