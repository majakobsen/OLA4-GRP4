library(chromote)
library(rvest)
library(dplyr)

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
