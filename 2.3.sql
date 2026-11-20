-- Opgave 2.3: komplet, genkørbart MySQL Workbench-script
-- Kør hele fanen med lynet uden markeret tekst.
-- Bygger kørsel II fra kørsel I den 2026-11-19 kl. 13:50:00.
-- Scriptet er genkørbart: det indsætter ikke dubletter for kørsel II.
-- Biler slettes aldrig. De fem biler, der mangler, får sold = TRUE.

USE bilbasen_ola;

-- Gem og slå Workbench Safe Updates fra for denne kørsel.
SET @forrige_safe_updates = @@SQL_SAFE_UPDATES;
SET SQL_SAFE_UPDATES = 0;

SET @dato_I = '2026-11-19 13:50:00';
SET @dato_II = DATE_ADD(@dato_I, INTERVAL 1 DAY);

-- Fjern kun midlertidige arbejdsborde fra denne Workbench-session.
DROP TEMPORARY TABLE IF EXISTS sim_run1;
DROP TEMPORARY TABLE IF EXISTS sim_solgte;
DROP TEMPORARY TABLE IF EXISTS sim_korsel_II;
DROP TEMPORARY TABLE IF EXISTS sim_prisaendringer;

-- Hent kørsel I fra databasen.
CREATE TEMPORARY TABLE sim_run1 AS
SELECT
    o.observationid,
    b.carid,
    b.maerke,
    b.model,
    b.registrering,
    b.link,
    b.forhandlerid,
    o.pris,
    o.kilometer
FROM bilobservation AS o
JOIN bil AS b ON b.carid = o.carid
WHERE o.scrapedate = @dato_I;

-- Vælg fem biler deterministisk som solgte.
CREATE TEMPORARY TABLE sim_solgte AS
SELECT observationid, carid
FROM sim_run1
ORDER BY carid
LIMIT 5;

-- Kørsel II består af de øvrige biler fra kørsel I.
CREATE TEMPORARY TABLE sim_korsel_II AS
SELECT
    r.carid,
    r.maerke,
    r.model,
    r.registrering,
    r.link,
    r.forhandlerid,
    r.pris,
    r.kilometer
FROM sim_run1 AS r
LEFT JOIN sim_solgte AS s ON s.carid = r.carid
WHERE s.carid IS NULL;

-- Vælg tre af bilerne og sænk prisen med 5%.
CREATE TEMPORARY TABLE sim_prisaendringer AS
SELECT carid
FROM sim_korsel_II
ORDER BY carid
LIMIT 3;

UPDATE sim_korsel_II AS k
JOIN sim_prisaendringer AS p ON p.carid = k.carid
SET k.pris = ROUND(k.pris * 0.95, 2);

-- Tilføj to simulerede biler. De er tydeligt mærket som simulation.
INSERT INTO sim_korsel_II
    (carid, maerke, model, registrering, link, forhandlerid, pris, kilometer)
VALUES
    ('SIM-20261120-1', 'BMW', 'BMW i4 (simuleret)', '1/2024',
     'https://example.invalid/simuleret-bil-1', NULL, 399900.00, 25000),
    ('SIM-20261120-2', 'BMW', 'BMW i5 (simuleret)', '1/2025',
     'https://example.invalid/simuleret-bil-2', NULL, 599900.00, 12000);

-- Vis hvad scriptet vil gøre.
SELECT 'Solgte - beholdes og markeres TRUE' AS kategori, carid
FROM sim_solgte
ORDER BY carid;

SELECT 'Prisændringer' AS kategori,
       r.carid,
       r.pris AS pris_I,
       k.pris AS pris_II
FROM sim_run1 AS r
JOIN sim_korsel_II AS k ON k.carid = r.carid
WHERE r.pris <> k.pris
ORDER BY r.carid;

SELECT 'Nye simulerede biler' AS kategori, carid, model, pris
FROM sim_korsel_II
WHERE carid LIKE 'SIM-%'
ORDER BY carid;

SELECT
    (SELECT COUNT(*) FROM sim_run1) AS biler_i_korsel_I,
    (SELECT COUNT(*) FROM sim_solgte) AS simuleret_solgte,
    (SELECT COUNT(*) FROM sim_prisaendringer) AS simuleret_prisaendringer,
    (SELECT COUNT(*) FROM sim_korsel_II) AS biler_i_korsel_II;

-- Opdater databasen. INSERT IGNORE / NOT EXISTS gør denne del genkørbar.
START TRANSACTION;

-- Opret de nye biler, men spring over hvis de allerede findes.
INSERT IGNORE INTO bil
    (carid, maerke, model, registrering, link, forhandlerid)
SELECT
    k.carid, k.maerke, k.model, k.registrering, k.link, k.forhandlerid
FROM sim_korsel_II AS k
WHERE k.carid LIKE 'SIM-%';

-- Slet ikke de fem biler: markér deres observation fra kørsel I som solgt.
UPDATE bilobservation AS o
JOIN sim_solgte AS s ON s.observationid = o.observationid
SET o.sold = TRUE;

-- Ret observationer fra kørsel II, hvis scriptet tidligere blev kørt delvist.
-- Det sikrer bl.a., at de tre simulerede prisændringer bliver gemt korrekt.
UPDATE bilobservation AS existing
JOIN sim_korsel_II AS k
  ON k.carid = existing.carid
LEFT JOIN sim_run1 AS r
  ON r.carid = k.carid
SET existing.pris = k.pris,
    existing.kilometer = k.kilometer,
    existing.sold = FALSE,
    existing.forrige_observationid = r.observationid
WHERE existing.scrapedate = @dato_II;

-- Gem observationer, der endnu ikke findes for kørsel II.
INSERT INTO bilobservation
    (carid, scrapedate, pris, kilometer, sold, forrige_observationid)
SELECT
    k.carid,
    @dato_II,
    k.pris,
    k.kilometer,
    FALSE,
    r.observationid
FROM sim_korsel_II AS k
LEFT JOIN sim_run1 AS r ON r.carid = k.carid
WHERE NOT EXISTS (
    SELECT 1
    FROM bilobservation AS existing
    WHERE existing.carid = k.carid
      AND existing.scrapedate = @dato_II
);

COMMIT;

-- Slutkontrol: viser antal observationer på datoen for kørsel II.
SELECT COUNT(*) AS observationer_fra_korsel_II
FROM bilobservation
WHERE scrapedate = @dato_II;

-- Slutkontrol: skal vise 3 biler med ændret pris.
SELECT r.carid, r.pris AS pris_I, o.pris AS pris_II
FROM sim_run1 AS r
JOIN bilobservation AS o ON o.carid = r.carid
WHERE o.scrapedate = @dato_II
  AND o.pris <> r.pris
ORDER BY r.carid;

-- Slutkontrol: de fem solgte biler skal alle stå med sold = 1.
SELECT o.carid, o.sold
FROM bilobservation AS o
JOIN sim_solgte AS s ON s.observationid = o.observationid
ORDER BY o.carid;

-- Gendan Workbench Safe Updates-indstillingen.
SET SQL_SAFE_UPDATES = @forrige_safe_updates;


