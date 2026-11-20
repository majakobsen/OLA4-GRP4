# -------------------
# Opgave 3.5
# -------------------

outputMappe <- "C:/Users/Lasse/Desktop/R/OLA 4/Mistænkelige fly"

# Gem hele tabellen
write.csv2(
  Resultat34,
  file.path(outputMappe, "Resultat_3_5.csv"),
  row.names = FALSE
)

saveRDS(
  Resultat34,
  file.path(outputMappe, "Resultat_3_5.rds")
)

# Gem fly fundet af jeres algoritme
mistaenkelige <- Resultat34 |>
  filter(myOwnAlg == 1)

tid <- format(Sys.time(), "%Y%m%d_%H%M%S")

csvfil <- file.path(
  outputMappe,
  paste0("mistaenkelige_fly_", tid, ".csv")
)

rdsfil <- file.path(
  outputMappe,
  paste0("mistaenkelige_fly_", tid, ".rds")
)

write.csv2(mistaenkelige, csvfil, row.names = FALSE)
saveRDS(mistaenkelige, rdsfil)

