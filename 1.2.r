
# -------------------
# Opgave 1.2 - AutoScout24 Tyskland
# -------------------

autoscout_base <- "https://www.autoscout24.com/lst/volkswagen/ft_gasoline?cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&powertype=kw&sort=standard&ustate=N%2CU&page=1&atype=C&search_id=1cpi43x08l4&source=listpage_pagination"

autoscout_browser <- ChromoteSession$new()
bilertyskland <- data.frame()
autoscout_browser$view()


#Hvor mange sider skal scrapes

antalsideras <- 5

for (autoscout_side in 1:antalsideras) {
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

bilertyskland$ImportOmk <-
  pmin(bilertyskland$dkkpris, 76400) * 0.25 +
  pmin(pmax(bilertyskland$dkkpris - 76400, 0), 237400 - 76400) * 0.85 +
  pmax(bilertyskland$dkkpris - 237400, 0) * 1.50



bilertyskland$TotalDKKPris <- bilertyskland$ImportOmk + bilertyskland$dkkpris + 5000

#Sammenligning samme modeller fra de to datasæt

bilertyskland$model <- tolower(bilertyskland$model)
biler_bilbasen$model <- tolower(biler_bilbasen$model)

sammenligning <- merge(
  bilertyskland[, c("model", "TotalDKKPris")],
  biler_bilbasen[, c("model", "pris")],
  by = "model"
)

sammenligning <- sammenligning |>
  mutate(pris = as.numeric(gsub("[^0-9]", "", pris))) |>
  group_by(model) |>
  summarise(
    median_totalimport = median(TotalDKKPris, na.rm = TRUE),
    median_bilbasen_pris = median(pris, na.rm = TRUE),
    forskel = median_bilbasen_pris - median_totalimport,
    .groups = "drop"
  )
