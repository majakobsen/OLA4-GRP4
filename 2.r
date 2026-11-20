# -------------------
# Opgave 2 - SQL
# -------------------

library(RMariaDB)
library(DBI)
library(dplyr)

con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "127.0.0.1",
  port = 3306,
  username = "root",
  password = "root123"
)

dbExecute(con, "USE bilbasen_ola")

# 2.1
# Opret tabellerne

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS Forhandler (
    forhandlerid VARCHAR(100) PRIMARY KEY
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS Bil (
    carid VARCHAR(30) PRIMARY KEY,
    maerke VARCHAR(100),
    model VARCHAR(255),
    registrering VARCHAR(30),
    link TEXT,
    forhandlerid VARCHAR(100),
    FOREIGN KEY (forhandlerid)
      REFERENCES Forhandler(forhandlerid)
  )
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS BilObservation (
    observationid INT AUTO_INCREMENT PRIMARY KEY,
    carid VARCHAR(30) NOT NULL,
    scrapedate DATETIME NOT NULL,
    pris DECIMAL(12,2),
    kilometer INT,
    sold BOOLEAN DEFAULT FALSE,
    forrige_observationid INT,
    FOREIGN KEY (carid)
      REFERENCES Bil(carid),
    FOREIGN KEY (forrige_observationid)
      REFERENCES BilObservation(observationid)
  )
")

dbListTables(con)

# 2.2
# Gem foerste scrape i de tre tabeller
# Koer kun indsaettelserne nedenfor paa tomme tabeller.

biler <- readRDS("Bilbasen-Biler.rds") |>
  distinct(id, .keep_all = TRUE) |>
  mutate(
    carid = as.character(id),
    forhandlerid = na_if(trimws(as.character(forhandler)), "")
  )

# Her bruges importtidspunktet. Erstat med det faktiske
# scrapetidspunkt, hvis data blev hentet tidligere.
# Eksempel paa format: as.POSIXct("2026-11-19 10:00:00", tz = "Europe/Copenhagen")
scrapedato <- Sys.time()

# Forhandlere
forhandlere <- biler |>
  filter(!is.na(forhandlerid)) |>
  distinct(forhandlerid)

dbWriteTable(con, "Forhandler", forhandlere,
             append = TRUE, row.names = FALSE)

# Faste biloplysninger
biloplysninger <- biler |>
  select(carid, maerke, model, registrering, link, forhandlerid)

dbWriteTable(con, "Bil", biloplysninger,
             append = TRUE, row.names = FALSE)

# Foerste observation: tidligere observation er automatisk NULL.
observationer <- biler |>
  transmute(
    carid,
    scrapedate = scrapedato,
    pris = as.numeric(gsub("[^0-9]", "", as.character(pris))),
    kilometer = as.integer(gsub("[^0-9]", "", as.character(kilometer))),
    sold = FALSE
  )

dbWriteTable(con, "BilObservation", observationer,
             append = TRUE, row.names = FALSE)

# Se data fra de sammenkoblede tabeller
BilerFraDatabase <- dbGetQuery(con, "
  SELECT b.carid, b.maerke, b.model, b.registrering,
         b.link, f.forhandlerid,
         o.scrapedate, o.pris, o.kilometer, o.sold
  FROM Bil b
  LEFT JOIN Forhandler f ON f.forhandlerid = b.forhandlerid
  JOIN BilObservation o ON o.carid = b.carid
")

View(BilerFraDatabase)

